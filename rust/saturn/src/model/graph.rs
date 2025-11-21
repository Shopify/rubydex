use std::collections::hash_map::Entry;
use std::sync::LazyLock;

use crate::indexing::local_graph::LocalGraph;
use crate::model::declaration::Declaration;
use crate::model::definitions::Definition;
use crate::model::document::Document;
use crate::model::identity_maps::IdentityHashMap;
use crate::model::ids::{DeclarationId, DefinitionId, NameId, ReferenceId, StringId, UriId};
use crate::model::name::{NameRef, ResolvedName};
// use crate::model::integrity::IntegrityChecker;
use crate::model::references::{ConstantReference, MethodRef};
use crate::resolution::Unit;
use crate::stats;

pub static OBJECT_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("Object"));

// The `Graph` is the global representation of the entire Ruby codebase. It contains all declarations and their
// relationships
#[derive(Default, Debug)]
pub struct Graph {
    // Map of declaration nodes
    declarations: IdentityHashMap<DeclarationId, Declaration>,
    // Map of document nodes
    documents: IdentityHashMap<UriId, Document>,
    // Map of definition nodes
    definitions: IdentityHashMap<DefinitionId, Definition>,

    definitions_to_declarations: IdentityHashMap<DefinitionId, DeclarationId>,

    // Map of unqualified names
    strings: IdentityHashMap<StringId, String>,
    // Map of names
    names: IdentityHashMap<NameId, NameRef>,
    // Map of constant references
    constant_references: IdentityHashMap<ReferenceId, ConstantReference>,
    // Map of method references that still need to be resolved
    method_references: IdentityHashMap<ReferenceId, MethodRef>,
}

impl Graph {
    #[must_use]
    pub fn new() -> Self {
        // Insert the magic top level self <main> object into the graph, so that we can associate global variables or
        // definitions made at the top level with it
        // let main_id = DeclarationId::from("<main>");
        // declarations.insert(main_id, Declaration::new(String::from("<main>"), main_id));

        // Insert `Object` head of time into the graph
        let mut declarations = IdentityHashMap::default();
        declarations.insert(*OBJECT_ID, Declaration::new(String::from("Object"), *OBJECT_ID));

        Self {
            declarations,
            definitions: IdentityHashMap::default(),
            definitions_to_declarations: IdentityHashMap::default(),
            documents: IdentityHashMap::default(),
            strings: IdentityHashMap::default(),
            names: IdentityHashMap::default(),
            constant_references: IdentityHashMap::default(),
            method_references: IdentityHashMap::default(),
        }
    }

    // TODO: remove this method once we have proper synchronization. This is a hack
    #[must_use]
    pub(crate) fn declarations_mut(&mut self) -> &mut IdentityHashMap<DeclarationId, Declaration> {
        &mut self.declarations
    }

    // Returns an immutable reference to the declarations map
    #[must_use]
    pub fn declarations(&self) -> &IdentityHashMap<DeclarationId, Declaration> {
        &self.declarations
    }

    pub fn add_declaration(
        &mut self,
        fully_qualified_name: String,
        definition_id: DefinitionId,
        owner_id: DeclarationId,
    ) -> DeclarationId {
        let declaration_id = DeclarationId::from(&fully_qualified_name);

        let declaration = self
            .declarations
            .entry(declaration_id)
            .or_insert_with(|| Declaration::new(fully_qualified_name, owner_id));

        declaration.add_definition(definition_id);
        self.definitions_to_declarations.insert(definition_id, declaration_id);
        declaration_id
    }

    pub fn clear_declarations(&mut self) {
        self.declarations.clear();
        self.definitions_to_declarations.clear();
    }

    // Returns an immutable reference to the definitions map
    #[must_use]
    pub fn definitions(&self) -> &IdentityHashMap<DefinitionId, Definition> {
        &self.definitions
    }

    /// Returns the ID of the unqualified name of a definition
    ///
    /// # Panics
    ///
    /// This will panic if there's inconsistent data in the graph
    #[must_use]
    pub fn definition_string_id(&self, definition: &Definition) -> StringId {
        let id = match definition {
            Definition::Class(it) => {
                let name = self.names.get(it.name_id()).unwrap();
                name.str()
            }
            Definition::Module(it) => {
                let name = self.names.get(it.name_id()).unwrap();
                name.str()
            }
            Definition::Constant(it) => {
                let name = self.names.get(it.name_id()).unwrap();
                name.str()
            }
            Definition::GlobalVariable(it) => it.str_id(),
            Definition::InstanceVariable(it) => it.str_id(),
            Definition::ClassVariable(it) => it.str_id(),
            Definition::AttrAccessor(it) => it.str_id(),
            Definition::AttrReader(it) => it.str_id(),
            Definition::AttrWriter(it) => it.str_id(),
            Definition::Method(it) => it.str_id(),
        };

        *id
    }

    /// Returns a tuple of r vectors:
    /// - The first one contains all constants, sorted in order for resolution (less complex constant names first)
    /// - The second one contains all other definitions, in no particular order
    ///
    /// # Panics
    ///
    /// This will panic if there's a definition with a name ID that doesn't exist
    #[must_use]
    pub fn sorted_resolution_units(&self) -> (Vec<Unit>, Vec<DefinitionId>) {
        let estimated_length = self.definitions.len() / 2;
        let mut constants = Vec::with_capacity(estimated_length);
        let mut others = Vec::with_capacity(estimated_length);

        for (id, definition) in &self.definitions {
            match definition {
                Definition::Class(def) => {
                    constants.push((Unit::Definition(*id), self.names.get(def.name_id()).unwrap()));
                }
                Definition::Module(def) => {
                    constants.push((Unit::Definition(*id), self.names.get(def.name_id()).unwrap()));
                }
                Definition::Constant(_) => {
                    // TODO: This should go in the constants vector, but we didn't change constant definitions to hold a
                    // name id yet. Fix the `resolving_nested_reference` test in resolution.rs too!!!
                    others.push(*id);
                }
                _ => {
                    others.push(*id);
                }
            }
        }

        for (id, constant_ref) in &self.constant_references {
            constants.push((Unit::Reference(*id), self.names.get(constant_ref.name_id()).unwrap()));
        }

        // Sort namespaces based on their name complexity so that simpler names are always first
        constants.sort_by(|(_, name_a), (_, name_b)| {
            let complexity_a = self.name_complexity(name_a);
            let complexity_b = self.name_complexity(name_b);
            complexity_a.cmp(&complexity_b)
        });

        others.shrink_to_fit();
        (constants.into_iter().map(|(id, _)| id).collect(), others)
    }

    /// Returns a complexity score for a given name, which is used to sort names for resolution. The complexity is based
    /// on how many parent scopes are involved in a name's nesting. This is because simple names are always
    /// straightforward to resolve no matter how deep the nesting is. For example:
    ///
    /// ```ruby
    /// module Foo
    ///   module Bar
    ///     class Baz; end
    ///   end
    /// end
    /// ```
    ///
    /// These are all simple names because they don't require resolution logic to determine the final name of each step.
    /// We only have to ensure that they are ordered by nesting level. Names with parent scopes require that their parts
    /// be resolved to determine what they refer to and so they must be sorted last.
    ///
    /// ```ruby
    /// module Foo
    ///   module Bar::Baz
    ///     class Qux; end
    ///  end
    /// end
    /// ```
    ///
    /// In this case, we need `Bar` to have already been processed so that we can resolve the `Bar` reference inside of
    /// the `Foo` nesting, which then unblocks the resolution of `Baz` and finally `Qux`. Notice how `Qux` is a simple
    /// name, but it's nested under a complex name so we have to sort it last. This is why we consider the number of
    /// parent scopes in the entire nesting, not just for the name itself
    #[must_use]
    fn name_complexity(&self, name: &NameRef) -> u32 {
        let mut current_name = name;
        let mut complexity = if current_name.parent_scope().is_some() { 2 } else { 1 };

        while let Some(nesting) = current_name.nesting() {
            if let Some(parent_id) = current_name.parent_scope() {
                let mut parent = self.names.get(parent_id).unwrap();
                let mut current_complexity = if current_name.parent_scope().is_some() { 2 } else { 1 };

                while let Some(parent_scope) = parent.parent_scope() {
                    current_complexity += 1;
                    parent = self.names.get(parent_scope).unwrap();
                }

                complexity += current_complexity;
            }

            complexity += 1;
            current_name = self.names.get(nesting).unwrap();
        }

        complexity
    }

    // Returns an immutable reference to the strings map
    #[must_use]
    pub fn strings(&self) -> &IdentityHashMap<StringId, String> {
        &self.strings
    }

    // Returns an immutable reference to the URI pool map
    #[must_use]
    pub fn documents(&self) -> &IdentityHashMap<UriId, Document> {
        &self.documents
    }

    #[must_use]
    pub fn definitions_to_declarations(&self) -> &IdentityHashMap<DefinitionId, DeclarationId> {
        &self.definitions_to_declarations
    }

    // Returns an immutable reference to the constant references map
    #[must_use]
    pub fn constant_references(&self) -> &IdentityHashMap<ReferenceId, ConstantReference> {
        &self.constant_references
    }

    // Returns an immutable reference to the method references map
    #[must_use]
    pub fn method_references(&self) -> &IdentityHashMap<ReferenceId, MethodRef> {
        &self.method_references
    }

    #[must_use]
    pub fn get(&self, name: &str) -> Option<Vec<&Definition>> {
        let declaration_id = DeclarationId::from(name);
        let declaration = self.declarations.get(&declaration_id)?;

        Some(
            declaration
                .definitions()
                .iter()
                .filter_map(|id| self.definitions.get(id))
                .collect(),
        )
    }

    #[must_use]
    pub fn get_declaration_mut(&mut self, declaration_id: &DeclarationId) -> Option<&mut Declaration> {
        self.declarations.get_mut(declaration_id)
    }

    #[must_use]
    pub fn names(&self) -> &IdentityHashMap<NameId, NameRef> {
        &self.names
    }

    /// Register a member relationship from a declaration to another declaration through its unqualified name id. For example, in
    ///
    /// ```ruby
    /// module Foo
    ///   class Bar; end
    ///   def baz; end
    /// end
    /// ```
    ///
    /// `Foo` has two members:
    /// ```ruby
    /// {
    ///   NameId(Bar) => DeclarationId(Bar)
    ///   NameId(baz) => DeclarationId(baz)
    /// }
    /// ```
    pub fn add_member(
        &mut self,
        owner_id: &DeclarationId,
        member_declaration_id: DeclarationId,
        member_str_id: StringId,
    ) {
        if let Some(declaration) = self.declarations.get_mut(owner_id) {
            declaration.add_member(member_str_id, member_declaration_id);
        }
    }

    /// # Panics
    ///
    /// This function will panic when trying to record a resolve name for a name ID that does not exist
    pub fn record_resolved_name(&mut self, name_id: NameId, declaration_id: DeclarationId) {
        match self.names.entry(name_id) {
            Entry::Occupied(entry) => match entry.get() {
                NameRef::Unresolved(_) => {
                    if let NameRef::Unresolved(unresolved) = entry.remove() {
                        let resolved_name = NameRef::Resolved(Box::new(ResolvedName::new(*unresolved, declaration_id)));
                        self.names.insert(name_id, resolved_name);
                    }
                }
                NameRef::Resolved(_) => {
                    // TODO: consider if this is a valid scenario with the resolution phase design. Either collect
                    // metrics here or panic if it's never supposed to occur
                }
            },
            Entry::Vacant(_) => panic!("Trying to record resolved name for a name ID that does not exist"),
        }
    }

    pub fn record_resolved_reference(&mut self, reference_id: ReferenceId, declaration_id: DeclarationId) {
        if let Some(declaration) = self.declarations.get_mut(&declaration_id) {
            declaration.add_reference(reference_id);
        }
    }

    //// Handles the deletion of a document identified by `uri`
    pub fn delete_uri(&mut self, uri: &str) {
        let uri_id = UriId::from(uri);
        self.remove_definitions_for_uri(uri_id);
        self.documents.remove(&uri_id);
    }

    /// Merges everything in `other` into this Graph. This method is meant to merge all graph representations from
    /// different threads, but not meant to handle updates to the existing global representation
    pub fn extend(&mut self, local_graph: LocalGraph) {
        let (uri_id, document, definitions, strings, names, constant_references, method_references) =
            local_graph.into_parts();

        self.documents.insert(uri_id, document);
        self.definitions.extend(definitions);
        self.strings.extend(strings);
        self.names.extend(names);
        self.constant_references.extend(constant_references);
        self.method_references.extend(method_references);
    }

    /// Updates the global representation with the information contained in `other`, handling deletions, insertions and
    /// updates to existing entries
    pub fn update(&mut self, other: LocalGraph) {
        // For each URI that was indexed through `other`, check what was discovered and update our current global
        // representation
        let uri_id = other.uri_id();
        self.remove_definitions_for_uri(uri_id);

        self.extend(other);
    }

    // Removes all nodes and relationships associated to the given URI. This is used to clean up stale data when a
    // document (identified by `uri_id`) changes or when a document is closed and we need to clean up the memory
    fn remove_definitions_for_uri(&mut self, uri_id: UriId) {
        // Vector of (owner_declaration_id, member_name_id) to delete after processing all definitions
        // let mut members_to_delete: Vec<(DeclarationId, NameId)> = Vec::new();

        if let Some(document) = self.documents.remove(&uri_id) {
            for def_id in document.definitions() {
                // let declaration_id = self.definitions_to_declarations.get(def_id).unwrap();

                if let Some(_definition) = self.definitions.remove(def_id)
                // && let Some(declaration) = self.declarations.get_mut(declaration_id)
                // && declaration.remove_definition(def_id)
                // && declaration.has_no_definitions()
                {
                    // let unqualified_name_id = NameId::from(&declaration.unqualified_name());
                    // if let Some(owner_id) = declaration.owner_id() {
                    //     members_to_delete.push((*owner_id, unqualified_name_id));
                    // }
                    // self.declarations.remove(declaration_id);
                }
            }
        }

        // // Clean up any members that pointed to declarations that were removed
        // for (owner_id, member_name_id) in members_to_delete {
        //     // Remove the `if` and use `unwrap` once we are indexing RBS files to have `Object`
        //     if let Some(owner) = self.declarations.get_mut(&owner_id) {
        //         owner.remove_member(&member_name_id);
        //     }
        // }
    }

    /// Asserts that the index is in a valid state.
    #[cfg(test)]
    pub fn assert_integrity(&self) {
        // Self::integrity_checker().assert_integrity(self);
    }

    // #[allow(clippy::too_many_lines)]
    // #[must_use]
    // pub fn integrity_checker() -> IntegrityChecker {
    //     let mut checker = IntegrityChecker::new();

    //     checker.add_rule("Each `declaration` has at least one definition", |index, errors| {
    //         for declaration in index.declarations().values() {
    //             if declaration.name() != "<main>" && declaration.definitions().is_empty() {
    //                 errors.push(format!(
    //                     "Declaration '{}' exists in `declarations`, but is not associated to any definitions",
    //                     declaration.name()
    //                 ));
    //             }
    //         }
    //     });

    //     checker.add_rule(
    //         "Each `definition` declaration_id is registered in `declarations`",
    //         |index, errors| {
    //             for definition_id in index.definitions().keys() {
    //                 let declaration_id = index.definitions_to_declarations.get(definition_id).unwrap();
    //                 if !index.declarations().contains_key(declaration_id) {
    //                     errors.push(format!(
    //                         "Definition '{definition_id}' is registered in `definitions` but not in `declarations`"
    //                     ));
    //                 }
    //             }
    //         },
    //     );

    //     checker.add_rule(
    //         "Each `declaration` definition is registered in `definitions`",
    //         |index, errors| {
    //             for declaration in index.declarations().values() {
    //                 for definition_id in declaration.definitions() {
    //                     if !index.definitions().contains_key(definition_id) {
    //                         errors.push(format!(
    //                             "Definition '{definition_id}' exists in `declarations` but not in `definitions`"
    //                         ));
    //                     }
    //                 }
    //             }
    //         },
    //     );

    //     checker.add_rule("Each `definition` URI is registered in `documents`", |index, errors| {
    //         for definition in index.definitions().values() {
    //             let uri_id = definition.uri_id();
    //             if !index.documents().contains_key(uri_id) {
    //                 errors.push(format!(
    //                     "URI id '{uri_id}' is registered in `definitions` but not in `documents`"
    //                 ));
    //             }
    //         }
    //     });

    //     checker.add_rule(
    //         "Each `document` definition is registered in `definitions`",
    //         |index, errors| {
    //             for document in index.documents().values() {
    //                 for definition_id in document.definitions() {
    //                     if !index.definitions().contains_key(definition_id) {
    //                         errors.push(format!(
    //                             "Definition '{definition_id}' is registered in `uris_to_definitions` but not in `definitions`"
    //                         ));
    //                     }
    //                 }
    //             }
    //         },
    //     );

    //     checker.add_rule("Each `definitions` URI is registered in `uri_pool`", |index, errors| {
    //         for definition in index.definitions().values() {
    //             let uri_id = definition.uri_id();
    //             if !index.documents().contains_key(uri_id) {
    //                 errors.push(format!(
    //                     "URI id '{uri_id}' is referenced by a definition but not present in `uri_pool`"
    //                 ));
    //             }
    //         }
    //     });

    //     checker
    // }

    #[allow(clippy::cast_precision_loss)]
    pub fn print_query_statistics(&self) {
        use std::collections::HashMap;

        let mut declarations_with_docs = 0;
        let mut total_doc_size = 0;
        let mut definition_types: HashMap<&str, usize> = HashMap::new();
        let mut multi_definition_count = 0;

        for declaration in self.declarations().values() {
            // Check documentation
            if let Some(definitions) = self.get(declaration.name()) {
                let has_docs = definitions.iter().any(|def| !def.comments().is_empty());
                if has_docs {
                    declarations_with_docs += 1;
                    let doc_size: usize = definitions.iter().map(|def| def.comments().len()).sum();
                    total_doc_size += doc_size;
                }
            }

            // Count definitions by type
            let definition_count = declaration.definitions().len();
            if definition_count > 1 {
                multi_definition_count += 1;
            }

            for def_id in declaration.definitions() {
                if let Some(def) = self.definitions().get(def_id) {
                    *definition_types.entry(def.kind()).or_insert(0) += 1;
                }
            }
        }

        println!();
        println!("Query statistics");
        let total_declarations = self.declarations().len();
        println!("  Total declarations:         {total_declarations}");
        println!(
            "  With documentation:         {} ({:.1}%)",
            declarations_with_docs,
            stats::percentage(declarations_with_docs, total_declarations)
        );
        println!(
            "  Without documentation:      {} ({:.1}%)",
            total_declarations - declarations_with_docs,
            stats::percentage(total_declarations - declarations_with_docs, total_declarations)
        );
        println!("  Total documentation size:   {total_doc_size} bytes");
        println!(
            "  Multi-definition names:     {} ({:.1}%)",
            multi_definition_count,
            stats::percentage(multi_definition_count, total_declarations)
        );

        println!();
        println!("Definition breakdown:");
        let mut types: Vec<_> = definition_types.iter().collect();
        types.sort_by_key(|(_, count)| std::cmp::Reverse(**count));
        for (kind, count) in types {
            println!("  {kind:20} {count:6}");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{model::comment::Comment, test_utils::GraphTest};

    #[test]
    fn deleting_a_uri() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        context.delete_uri("file:///foo.rb");
        context.resolve();

        assert!(context.graph.documents.is_empty());
        assert!(context.graph.definitions.is_empty());
        // Object is left
        assert_eq!(context.graph.declarations.len(), 1);

        context.graph.assert_integrity();
    }

    #[test]
    fn updating_index_with_deleted_definitions() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        // Update with empty content to remove definitions but keep the URI
        context.index_uri("file:///foo.rb", "");
        context.resolve();

        assert!(context.graph.definitions.is_empty());
        // Object is left
        assert_eq!(context.graph.declarations.len(), 1);
        // URI remains if the file was not deleted, but definitions got erased
        assert_eq!(context.graph.documents.len(), 1);

        context.graph.assert_integrity();
    }

    #[test]
    fn updating_index_with_new_definitions() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        context.resolve();

        assert_eq!(context.graph.definitions.len(), 1);
        let declaration = context.graph.declarations.get(&DeclarationId::from("Foo")).unwrap();
        assert_eq!(declaration.name(), "Foo");
        let document = context.graph.documents.get(&UriId::from("file:///foo.rb")).unwrap();
        assert_eq!(document.uri(), "file:///foo.rb");
        assert_eq!(declaration.definitions().len(), 1);
        assert_eq!(document.definitions().len(), 1);

        context.graph.assert_integrity();
    }

    #[test]
    fn updating_existing_definitions() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        // Update with the same definition but at a different position (with content before it)
        context.index_uri("file:///foo.rb", "\n\n\n\n\n\nmodule Foo; end");
        context.resolve();

        assert_eq!(context.graph.definitions.len(), 1);
        let declaration = context.graph.declarations.get(&DeclarationId::from("Foo")).unwrap();
        assert_eq!(declaration.name(), "Foo");
        assert_eq!(
            context
                .graph
                .documents
                .get(&UriId::from("file:///foo.rb"))
                .unwrap()
                .uri(),
            "file:///foo.rb"
        );

        let definitions = context.graph.get("Foo").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].offset().start(), 6);

        context.graph.assert_integrity();
    }

    #[test]
    fn adding_another_definition_from_a_different_uri() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        context.index_uri("file:///foo2.rb", "\n\n\n\n\nmodule Foo; end");
        context.resolve();

        let definitions = context.graph.get("Foo").unwrap();
        let mut offsets = definitions.iter().map(|d| d.offset().start()).collect::<Vec<_>>();
        offsets.sort_unstable();
        assert_eq!(definitions.len(), 2);
        assert_eq!(vec![0, 5], offsets);

        context.graph.assert_integrity();
    }

    #[test]
    fn adding_a_second_definition_from_the_same_uri() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");

        // Update with multiple definitions of the same module in one file
        context.index_uri("file:///foo.rb", {
            "
            module Foo; end


            module Foo; end
            "
        });

        context.resolve();

        let definitions = context.graph.get("Foo").unwrap();
        assert_eq!(definitions.len(), 2);

        let mut offsets = definitions
            .iter()
            .map(|d| [d.offset().start(), d.offset().end()])
            .collect::<Vec<_>>();
        offsets.sort_unstable();
        assert_eq!([0, 15], offsets[0]);
        assert_eq!([18, 33], offsets[1]);

        context.graph.assert_integrity();
    }

    #[test]
    fn get_documentation() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            # This is a class comment
            # Multi-line comment
            class CommentedClass; end

            # Module comment
            module CommentedModule; end

            class NoCommentClass; end
            "
        });

        context.resolve();

        let definitions = context.graph.get("CommentedClass").unwrap();
        let def = definitions.first().unwrap();
        assert_eq!(
            def.comments().iter().map(Comment::string).collect::<Vec<&String>>(),
            vec!["# This is a class comment", "# Multi-line comment"]
        );

        let definitions = context.graph.get("CommentedModule").unwrap();
        let def = definitions.first().unwrap();
        assert_eq!(
            def.comments().iter().map(Comment::string).collect::<Vec<&String>>(),
            vec!["# Module comment"]
        );

        let definitions = context.graph.get("NoCommentClass").unwrap();
        let def = definitions.first().unwrap();
        assert!(def.comments().is_empty());
    }

    #[test]
    fn members_are_updated_when_definitions_get_deleted() {
        let mut context = GraphTest::new();
        // Initially, have `Foo` defined twice with a member called `Bar`
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
            end
            "
        });
        context.index_uri("file:///foo2.rb", {
            r"
            module Foo
              class Bar; end
            end
            "
        });
        context.resolve();

        let foo = context.graph.declarations.get(&DeclarationId::from("Foo")).unwrap();
        assert!(foo.members().contains_key(&StringId::from("Bar")));

        // Delete `Bar`
        context.index_uri("file:///foo2.rb", {
            r"
            module Foo
            end
            "
        });
        context.resolve();

        let foo = context.graph.declarations.get(&DeclarationId::from("Foo")).unwrap();
        assert!(!foo.members().contains_key(&StringId::from("Bar")));
    }

    #[test]
    fn name_complexity() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              module Bar
                module Baz
                end
              end

              module Qux::Zip
                module Zap
                  class Zop::Boop
                  end
                end
              end
            end
            "
        });

        let mut names = context.graph.names().values().collect::<Vec<_>>();
        assert_eq!(8, names.len());

        names.sort_by_key(|a| context.graph.name_complexity(a));

        assert_eq!(
            vec!["Foo", "Qux", "Bar", "Baz", "Zip", "Zap", "Zop", "Boop"],
            names
                .iter()
                .map(|n| context.graph.strings.get(n.str()).unwrap().as_str())
                .collect::<Vec<_>>()
        );
    }
}
