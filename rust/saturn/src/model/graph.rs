use std::sync::LazyLock;

use crate::indexing::local_graph::LocalGraph;
use crate::model::declaration::Declaration;
use crate::model::definitions::Definition;
use crate::model::document::Document;
use crate::model::identity_maps::IdentityHashMap;
use crate::model::ids::{DeclarationId, DefinitionId, NameId, ReferenceId, StringId, UriId};
use crate::model::name::{Name, NameRef, ResolvedName};
// use crate::model::integrity::IntegrityChecker;
use crate::model::references::{ConstantReference, MethodRef};
use crate::stats;

pub static OBJECT_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("Object"));

/// Holds IDs of entities that were removed from the graph
pub struct RemovedIds {
    pub definition_ids: Vec<DefinitionId>,
    pub declaration_ids: Vec<DeclarationId>,
}

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
        // let mut declarations = IdentityHashMap::default();
        // Insert the magic top level self <main> object into the graph, so that we can associate global variables or
        // definitions made at the top level with it
        // let main_id = DeclarationId::from("<main>");
        // declarations.insert(main_id, Declaration::new(String::from("<main>"), main_id));

        //declarations.insert(object_id, Declaration::new(String::from("Object"), object_id));

        Self {
            declarations: IdentityHashMap::default(),
            definitions: IdentityHashMap::default(),
            definitions_to_declarations: IdentityHashMap::default(),
            documents: IdentityHashMap::default(),
            strings: IdentityHashMap::default(),
            names: IdentityHashMap::default(),
            constant_references: IdentityHashMap::default(),
            method_references: IdentityHashMap::default(),
        }
    }

    // Returns an immutable reference to the declarations map
    #[must_use]
    pub fn declarations(&self) -> &IdentityHashMap<DeclarationId, Declaration> {
        &self.declarations
    }

    pub fn add_declaration(&mut self, fully_qualified_name: String, definition_id: DefinitionId) -> DeclarationId {
        let declaration_id = DeclarationId::from(&fully_qualified_name);

        let declaration = self
            .declarations
            .entry(declaration_id)
            .or_insert_with(|| Declaration::new(fully_qualified_name));

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
            Definition::Constant(it) => it.str_id(),
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
        declaration_id: &DeclarationId,
        member_declaration_id: DeclarationId,
        member_name: &str,
    ) {
        if let Some(declaration) = self.declarations.get_mut(declaration_id) {
            let name_id = StringId::from(member_name);
            declaration.add_member(name_id, member_declaration_id);
        }
    }

    /// Attempts to resolve a constant reference against the graph. Returns the fully qualified declaration ID that the
    /// reference is related to or `None`
    ///
    /// # Panics
    ///
    /// This will panic if the nesting is invalid
    #[must_use]
    pub fn resolve_constant(&self, name: &Name) -> Option<DeclarationId> {
        if let Some(parent_scope) = name.parent_scope().and_then(|id| self.names.get(&id)) {
            // This is a constant path and we must first recurse to resolve the parent scope
            let declaration_id = match parent_scope {
                NameRef::Resolved(name) => *name.declaration_id(),
                NameRef::Unresolved(name) => self.resolve_constant(name)?,
            };

            let declaration = self.declarations.get(&declaration_id)?;
            declaration.members().get(name.str()).copied()
        } else {
            // First search lexical scopes
            let mut current_name = name;

            while let Some(nesting_id) = current_name.nesting() {
                let nesting_name_ref = self.names.get(nesting_id).unwrap();
                let (nesting_name, declaration_id) = match nesting_name_ref {
                    NameRef::Resolved(resolved) => (resolved.name(), *resolved.declaration_id()),
                    NameRef::Unresolved(name) => (&**name, self.resolve_constant(name)?),
                };

                if let Some(declaration) = self.declarations.get(&declaration_id)
                    && let Some(member) = declaration.members().get(name.str())
                {
                    return Some(*member);
                }

                current_name = nesting_name;
            }

            // TODO: Then search inheritance chain
            // Fall back to top level (member of Object). Note: temporary while we're missing RBS indexing
            let name = self.strings.get(name.str())?;
            Some(DeclarationId::from(name))
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

    /// Resolve all unresolved references and add them to their corresponding declarations
    ///
    /// # Panics
    ///
    /// This will panic if we resolve a reference to a declaration ID that does not exist in the graph
    pub fn resolve_references(&mut self) {
        let ref_ids = self.constant_references.keys().copied().collect::<Vec<_>>();

        for id in ref_ids {
            let reference = self.constant_references.get(&id).unwrap();
            let name_id = reference.name_id();

            match self.names.get(name_id).unwrap() {
                NameRef::Resolved(resolved) => {
                    if let Some(declaration) = self.declarations.get_mut(resolved.declaration_id()) {
                        declaration.add_reference(id);
                    }
                }
                NameRef::Unresolved(name) => {
                    if let Some(declaration_id) = self.resolve_constant(name)
                        && let Some(declaration) = self.declarations.get_mut(&declaration_id)
                    {
                        declaration.add_reference(id);

                        let name_ref = self.names.remove(name_id).unwrap();
                        let removed_name = name_ref.into_unresolved().unwrap();

                        self.names.insert(
                            *name_id,
                            NameRef::Resolved(Box::new(ResolvedName::new(removed_name, declaration_id))),
                        );
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{model::comment::Comment, test_utils::GraphTest};

    /// Asserts that a declaration has a constant reference at the specified location
    ///
    /// This macro:
    /// 1. Parses the location string into `(uri, start_offset, end_offset)`
    /// 2. Finds the declaration by name
    /// 3. Finds a constant reference to that declaration at the given uri and start offset
    /// 4. Asserts the end offset matches
    ///
    /// Location format: "uri:start_line:start_column-end_line:end_column"
    /// Example: `<file:///foo.rb:3:0-3:5>`
    macro_rules! assert_constant_reference_to {
        ($context:expr, $declaration_name:expr, $location:expr) => {
            let (uri, start, end) = $context.parse_location($location);

            let declaration = $context
                .graph
                .declarations
                .get(&DeclarationId::from($declaration_name))
                .expect(&format!("Declaration '{}' not found in graph", $declaration_name));

            let constant = declaration
                .references()
                .iter()
                .filter_map(|r| {
                    let reference = $context
                        .graph
                        .constant_references
                        .get(r)
                        .expect("Reference should exist");
                    if let NameRef::Resolved(_) = $context
                        .graph
                        .names
                        .get(reference.name_id())
                        .expect("Name should exist")
                    {
                        Some(reference)
                    } else {
                        None
                    }
                })
                .find(|c| c.uri_id() == UriId::from(&uri) && c.offset().start() == start)
                .expect(&format!(
                    "Declaration '{}' does not have a reference at {} starting at offset {}",
                    $declaration_name, $location, start
                ));

            $context.assert_offset_matches(
                &uri,
                constant.offset(),
                start,
                end,
                &format!("reference to '{}'", $declaration_name),
                $location,
            );
        };
    }

    #[test]
    fn deleting_a_uri() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        context.delete_uri("file:///foo.rb");
        context.resolve();

        assert!(context.graph.documents.is_empty());
        assert!(context.graph.definitions.is_empty());
        assert!(context.graph.declarations.is_empty());

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
        assert!(context.graph.declarations.is_empty());
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
    fn resolving_top_level_references() {
        let mut context = GraphTest::new();
        context.index_uri("file:///bar.rb", {
            r"
            class Bar; end

            ::Bar
            Bar
            "
        });
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              ::Bar
            end
            "
        });
        context.resolve();
        context.graph.resolve_references();
        assert_constant_reference_to!(context, "Bar", "file:///bar.rb:2:2-2:5");
        assert_constant_reference_to!(context, "Bar", "file:///bar.rb:3:0-3:3");
        assert_constant_reference_to!(context, "Bar", "file:///foo.rb:1:4-1:7");
    }

    #[test]
    fn resolving_nested_reference() {
        let mut context = GraphTest::new();
        context.index_uri("file:///bar.rb", {
            r"
            module Foo
              CONST = 123

              class Bar
                CONST
                Foo::CONST
              end
            end
            "
        });
        context.resolve();
        context.graph.resolve_references();
        assert_constant_reference_to!(context, "Foo::CONST", "file:///bar.rb:4:4-4:9");
        assert_constant_reference_to!(context, "Foo::CONST", "file:///bar.rb:5:9-5:14");
    }

    #[test]
    fn resolving_nested_reference_that_refer_to_top_level_constant() {
        let mut context = GraphTest::new();
        context.index_uri("file:///bar.rb", {
            r"
            class Baz; end

            module Foo
              class Bar
                Baz
              end
            end
            "
        });
        context.resolve();
        context.graph.resolve_references();
        assert_constant_reference_to!(context, "Baz", "file:///bar.rb:4:4-4:7");
    }

    #[test]
    fn resolving_constant_path_references_at_top_level() {
        let mut context = GraphTest::new();
        context.index_uri("file:///bar.rb", {
            r"
            module Foo
              class Bar; end
            end

            Foo::Bar
            "
        });
        context.resolve();
        context.graph.resolve_references();
        assert_constant_reference_to!(context, "Foo::Bar", "file:///bar.rb:4:5-4:8");
    }

    #[test]
    fn resolving_reference_for_non_existing_declaration() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            Foo
            "
        });
        context.resolve();
        context.graph.resolve_references();

        let reference = context.graph.constant_references.values().next().unwrap();

        match context.graph.names().get(reference.name_id()) {
            Some(NameRef::Unresolved(_)) => {}
            _ => panic!("expected unresolved constant reference"),
        }
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
}
