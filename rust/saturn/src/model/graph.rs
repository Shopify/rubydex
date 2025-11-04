use crate::model::declaration::Declaration;
use crate::model::definitions::Definition;
use crate::model::document::Document;
use crate::model::identity_maps::IdentityHashMap;
use crate::model::ids::{DeclarationId, DefinitionId, NameId, ReferenceId, UriId};
use crate::model::integrity::IntegrityChecker;
use crate::model::references::{ConstantRef, ConstantReference, MethodRef, UnresolvedConstantRef};
use crate::stats;
use std::collections::hash_map::Entry;

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
    // Map of unqualified names
    names: IdentityHashMap<NameId, String>,
    // Map of constant references
    constant_references: IdentityHashMap<ReferenceId, ConstantReference>,
    // Map of method references that still need to be resolved
    method_references: IdentityHashMap<ReferenceId, MethodRef>,
}

impl Graph {
    #[must_use]
    pub fn new() -> Self {
        let mut declarations = IdentityHashMap::default();
        // Insert the magic top level self <main> object into the graph, so that we can associate global variables or
        // definitions made at the top level with it
        let main_id = DeclarationId::from("<main>");
        declarations.insert(main_id, Declaration::new(String::from("<main>"), main_id));

        Self {
            declarations,
            definitions: IdentityHashMap::default(),
            documents: IdentityHashMap::default(),
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

    // Returns an immutable reference to the definitions map
    #[must_use]
    pub fn definitions(&self) -> &IdentityHashMap<DefinitionId, Definition> {
        &self.definitions
    }

    // Returns an immutable reference to the names map
    #[must_use]
    pub fn names(&self) -> &IdentityHashMap<NameId, String> {
        &self.names
    }

    // Returns an immutable reference to the URI pool map
    #[must_use]
    pub fn documents(&self) -> &IdentityHashMap<UriId, Document> {
        &self.documents
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
    pub(crate) fn get_definition_mut(&mut self, definition_id: DefinitionId) -> Option<&mut Definition> {
        self.definitions.get_mut(&definition_id)
    }

    // Registers a URI into the graph and returns the generated ID. This happens once when starting to index the URI and
    // then all definitions discovered in it get associated to the ID
    pub fn add_uri(&mut self, uri: String) -> UriId {
        let uri_id = UriId::from(&uri);
        self.documents.entry(uri_id).or_insert_with(|| Document::new(uri));
        uri_id
    }

    /// Register an unqualified name into the graph
    pub fn add_name(&mut self, name: String) -> NameId {
        let name_id = NameId::from(&name);
        self.names.entry(name_id).or_insert(name);
        name_id
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
            let name_id = NameId::from(member_name);
            declaration.add_member(name_id, member_declaration_id);
        }
    }

    // Registers a definition into the `Graph`, automatically creating all relationships
    pub fn add_definition(&mut self, name: String, definition: Definition, owner_id: &DeclarationId) -> DefinitionId {
        let uri_id = *definition.uri_id();
        let definition_id = DefinitionId::from(&format!("{uri_id}{}{}", definition.start(), &name));
        let declaration_id = *definition.declaration_id();

        self.declarations
            .entry(declaration_id)
            .or_insert_with(|| Declaration::new(name, *owner_id))
            .add_definition(definition_id);
        self.definitions.insert(definition_id, definition);
        self.documents
            .entry(uri_id)
            .and_modify(|doc| doc.add_definition(definition_id));

        definition_id
    }

    // Register an unresolved constant reference
    pub fn add_constant_reference(&mut self, reference: ConstantReference) -> ReferenceId {
        let reference_id = reference.id();
        self.constant_references.insert(reference_id, reference);
        reference_id
    }

    // Register an unresolved method reference
    pub fn add_method_reference(&mut self, reference: MethodRef) -> ReferenceId {
        let reference_id = reference.id();
        self.method_references.insert(reference_id, reference);
        reference_id
    }

    /// Attempts to resolve a constant reference against the graph. Returns the fully qualified declaration ID that the
    /// reference is related to or `None`
    pub fn resolve_constant(&self, reference: &UnresolvedConstantRef) -> Option<&Declaration> {
        if let Some(nesting) = reference.nesting() {
            let mut current_nesting = Some(nesting.as_ref());

            while let Some(nesting) = current_nesting {
                let declaration_id = nesting.declaration_id();
                if let Some(declaration) = self.declarations.get(declaration_id)
                    && let Some(member) = declaration.members().get(reference.name_id())
                {
                    return Some(self.declarations.get(member))?;
                }
                current_nesting = nesting.parent().as_ref().map(std::convert::AsRef::as_ref);
            }

            None
        } else {
            // Top level reference

            // Note: this code is temporary. Once we have RBS indexing, we can simply enter the graph by looking
            // up `Object` and then we search its members for the top level constant
            let name = self.names.get(reference.name_id())?;
            let declaration_id = DeclarationId::from(name);
            self.declarations.get(&declaration_id)
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
    pub fn extend(&mut self, incomplete_index: Graph) {
        self.definitions.extend(incomplete_index.definitions);
        self.names.extend(incomplete_index.names);
        self.constant_references.extend(incomplete_index.constant_references);
        self.method_references.extend(incomplete_index.method_references);

        for (declaration_id, declaration) in incomplete_index.declarations {
            match self.declarations.entry(declaration_id) {
                Entry::Vacant(entry) => {
                    entry.insert(declaration);
                }
                Entry::Occupied(mut entry) => {
                    entry.get_mut().extend(declaration);
                }
            }
        }

        for (uri_id, document) in incomplete_index.documents {
            match self.documents.entry(uri_id) {
                Entry::Vacant(entry) => {
                    entry.insert(document);
                }
                Entry::Occupied(mut entry) => {
                    let existing_doc = entry.get_mut();
                    for def_id in document.definitions() {
                        existing_doc.add_definition(*def_id);
                    }
                }
            }
        }
    }

    /// Updates the global representation with the information contained in `other`, handling deletions, insertions and
    /// updates to existing entries
    pub fn update(&mut self, other: Graph) {
        // For each URI that was indexed through `other`, check what was discovered and update our current global
        // representation
        for uri_id in other.documents.keys() {
            self.remove_definitions_for_uri(*uri_id);
        }

        self.extend(other);
    }

    // Removes all nodes and relationships associated to the given URI. This is used to clean up stale data when a
    // document (identified by `uri_id`) changes or when a document is closed and we need to clean up the memory
    fn remove_definitions_for_uri(&mut self, uri_id: UriId) {
        // Vector of (owner_declaration_id, member_name_id) to delete after processing all definitions
        let mut members_to_delete: Vec<(DeclarationId, NameId)> = Vec::new();

        if let Some(document) = self.documents.remove(&uri_id) {
            for def_id in document.definitions() {
                if let Some(definition) = self.definitions.remove(def_id)
                    && let Some(declaration) = self.declarations.get_mut(definition.declaration_id())
                    && declaration.remove_definition(def_id)
                    && declaration.has_no_definitions()
                {
                    let unqualified_name_id = NameId::from(&declaration.unqualified_name());
                    members_to_delete.push((*declaration.owner_id(), unqualified_name_id));
                    self.declarations.remove(definition.declaration_id());
                }
            }
        }

        // Clean up any members that pointed to declarations that were removed
        for (owner_id, member_name_id) in members_to_delete {
            // Remove the `if` and use `unwrap` once we are indexing RBS files to have `Object`
            if let Some(owner) = self.declarations.get_mut(&owner_id) {
                owner.remove_member(&member_name_id);
            }
        }
    }

    /// Asserts that the index is in a valid state.
    #[cfg(test)]
    pub fn assert_integrity(&self) {
        Self::integrity_checker().assert_integrity(self);
    }

    #[allow(clippy::too_many_lines)]
    #[must_use]
    pub fn integrity_checker() -> IntegrityChecker {
        let mut checker = IntegrityChecker::new();

        checker.add_rule("Each `declaration` has at least one definition", |index, errors| {
            for declaration in index.declarations().values() {
                if declaration.name() != "<main>" && declaration.definitions().is_empty() {
                    errors.push(format!(
                        "Declaration '{}' exists in `declarations`, but is not associated to any definitions",
                        declaration.name()
                    ));
                }
            }
        });

        checker.add_rule(
            "Each `definition` declaration_id is registered in `declarations`",
            |index, errors| {
                for definition in index.definitions().values() {
                    let declaration_id = definition.declaration_id();
                    if !index.declarations().contains_key(declaration_id) {
                        errors.push(format!(
                            "Name '{declaration_id}' is referenced by a definition but not present in `declarations`"
                        ));
                    }
                }
            },
        );

        checker.add_rule(
            "Each `declaration` definition is registered in `definitions`",
            |index, errors| {
                for declaration in index.declarations().values() {
                    for definition_id in declaration.definitions() {
                        if !index.definitions().contains_key(definition_id) {
                            errors.push(format!(
                                "Definition '{definition_id}' exists in `declarations` but not in `definitions`"
                            ));
                        }
                    }
                }
            },
        );

        checker.add_rule("Each `definition` URI is registered in `documents`", |index, errors| {
            for definition in index.definitions().values() {
                let uri_id = definition.uri_id();
                if !index.documents().contains_key(uri_id) {
                    errors.push(format!(
                        "URI id '{uri_id}' is registered in `definitions` but not in `documents`"
                    ));
                }
            }
        });

        checker.add_rule(
            "Each `document` definition is registered in `definitions`",
            |index, errors| {
                for document in index.documents().values() {
                    for definition_id in document.definitions() {
                        if !index.definitions().contains_key(definition_id) {
                            errors.push(format!(
                                "Definition '{definition_id}' is registered in `uris_to_definitions` but not in `definitions`"
                            ));
                        }
                    }
                }
            },
        );

        checker.add_rule("Each `definitions` URI is registered in `uri_pool`", |index, errors| {
            for definition in index.definitions().values() {
                let uri_id = definition.uri_id();
                if !index.documents().contains_key(uri_id) {
                    errors.push(format!(
                        "URI id '{uri_id}' is referenced by a definition but not present in `uri_pool`"
                    ));
                }
            }
        });

        checker
    }

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
        let reference_map = std::mem::take(&mut self.constant_references);

        for (id, reference) in reference_map {
            match reference {
                ConstantReference::Unresolved(constant_ref) => {
                    if let Some(declaration) = self.resolve_constant(&constant_ref) {
                        let declaration_id = DeclarationId::from(declaration.name());
                        let decl = self
                            .declarations
                            .get_mut(&declaration_id)
                            .expect("Resolved declaration must exist");

                        decl.add_reference(id);
                        self.constant_references.insert(
                            id,
                            ConstantReference::Resolved(Box::new(ConstantRef::new(*constant_ref, declaration_id))),
                        );
                    } else {
                        // Keep unresolved references
                        self.constant_references
                            .insert(id, ConstantReference::Unresolved(constant_ref));
                    }
                }
                ConstantReference::Resolved(_) => {
                    // Already resolved, nothing to do
                    self.constant_references.insert(id, reference);
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
                    if let ConstantReference::Resolved(c) = $context.graph.constant_references.get(r).unwrap() {
                        Some(c)
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

    /// Asserts that an unresolved constant reference has specific properties
    ///
    /// The `$nesting` parameter should be:
    /// - `None` for top-level references like `::Foo`
    /// - `Some("Foo::Bar")` for references within a specific lexical scope
    ///
    /// Location format: "uri:start_line:start_column-end_line:end_column"
    /// Example: `<file:///foo.rb:3:0-3:5>`
    macro_rules! assert_unresolved_constant {
        ($context:expr, $name:expr, $nesting:expr, $location:expr) => {
            let (uri, start, end) = $context.parse_location($location);
            let expected_nesting: Option<&str> = $nesting;

            let constant = $context
                .graph
                .constant_references
                .values()
                .filter_map(|r| {
                    if let ConstantReference::Unresolved(c) = r {
                        Some(c)
                    } else {
                        None
                    }
                })
                .find(|c| {
                    let name_matches = $context.graph.names.get(c.name_id()) == Some(&String::from($name));
                    let nesting_matches = match (c.nesting(), expected_nesting) {
                        (None, None) => true,
                        (Some(nesting), Some(expected_decl_id)) => {
                            nesting.declaration_id() == &DeclarationId::from(expected_decl_id)
                        }
                        _ => false,
                    };
                    name_matches && nesting_matches
                })
                .expect(&format!(
                    "{} with nesting {:?} is not found in unresolved references",
                    $name, expected_nesting
                ));

            if constant.uri_id() != UriId::from(&uri) {
                panic!(
                    "URI mismatch for unresolved constant '{}'\n  actual:   {:?}\n  expected: {}",
                    $name,
                    constant.uri_id(),
                    uri
                );
            }

            $context.assert_offset_matches(
                &uri,
                constant.offset(),
                start,
                end,
                &format!("unresolved constant '{}'", $name),
                $location,
            );
        };
    }

    #[test]
    fn deleting_a_uri() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        context.delete_uri("file:///foo.rb");

        assert!(context.graph.definitions.is_empty());
        // Only <main> remains
        assert!(context.graph.declarations.len() == 1);
        assert!(context.graph.documents.is_empty());

        context.graph.assert_integrity();
    }

    #[test]
    fn updating_index_with_deleted_definitions() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        // Update with empty content to remove definitions but keep the URI
        context.index_uri("file:///foo.rb", "");

        assert!(context.graph.definitions.is_empty());
        // Only <main> remains
        assert!(context.graph.declarations.len() == 1);
        // URI remains if the file was not deleted, but definitions got erased
        assert_eq!(context.graph.documents.len(), 1);

        context.graph.assert_integrity();
    }

    #[test]
    fn updating_index_with_new_definitions() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");

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
        assert_eq!(definitions[0].start(), 6);

        context.graph.assert_integrity();
    }

    #[test]
    fn adding_another_definition_from_a_different_uri() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        context.index_uri("file:///foo2.rb", "\n\n\n\n\nmodule Foo; end");

        let definitions = context.graph.get("Foo").unwrap();
        let mut offsets = definitions.iter().map(|d| d.start()).collect::<Vec<_>>();
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

        let definitions = context.graph.get("Foo").unwrap();
        assert_eq!(definitions.len(), 2);

        let mut offsets = definitions.iter().map(|d| [d.start(), d.end()]).collect::<Vec<_>>();
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
    fn resolving_constants() {
        let mut context = GraphTest::new();

        context.index_uri("file:///bar.rb", {
            r"
            class Bar
              ::Foo::Bar # should be resolved to Foo::Bar from file:///foo.rb
            end
            "
        });

        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              CONST = 1
              class Bar
                CONST # should be resolved to Foo::CONST
                ::Bar # should be resolved to Bar from file:///bar.rb
                ::Baz # should not be resolved because it doesn't exist
                String # should not be resolved because it's not top-level
                ::Object # should not be resolved because ::Object does not exist in the graph yet
              end
            end

            Foo::CONST # should be resolved to Foo::CONST and Foo should be resolved to Foo
            "
        });

        let mut constant_references = context.graph.constant_references().values().collect::<Vec<_>>();

        constant_references.sort_by_key(|r| match r {
            ConstantReference::Unresolved(constant) => (
                context
                    .graph
                    .documents()
                    .get(&constant.uri_id())
                    .unwrap()
                    .uri()
                    .to_string(),
                constant.offset().start(),
                constant.offset().end(),
            ),
            ConstantReference::Resolved(constant) => (
                context
                    .graph
                    .documents()
                    .get(&constant.uri_id())
                    .unwrap()
                    .uri()
                    .to_string(),
                constant.offset().start(),
                constant.offset().end(),
            ),
        });

        context.graph.resolve_references();

        let foo_declaration = context.graph.declarations.get(&DeclarationId::from("Foo")).unwrap();
        assert_eq!(foo_declaration.references().len(), 2, "Foo should have 2 references");
        assert_constant_reference_to!(context, "Foo", "file:///foo.rb:11:0-11:3");
        assert_constant_reference_to!(context, "Foo", "file:///bar.rb:1:2-1:7");

        let foo_const_declaration = context
            .graph
            .declarations
            .get(&DeclarationId::from("Foo::CONST"))
            .unwrap();
        assert_eq!(
            foo_const_declaration.references().len(),
            2,
            "Foo::CONST should have 2 references"
        );
        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:3:4-3:9");
        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:11:0-11:10");

        let bar_declaration = context.graph.declarations.get(&DeclarationId::from("Bar")).unwrap();
        assert_eq!(bar_declaration.references().len(), 1, "Bar should have 1 reference");
        assert_constant_reference_to!(context, "Bar", "file:///foo.rb:4:4-4:9");

        let foo_bar_declaration = context
            .graph
            .declarations
            .get(&DeclarationId::from("Foo::Bar"))
            .unwrap();
        assert_eq!(
            foo_bar_declaration.references().len(),
            1,
            "Foo::Bar should have 1 reference"
        );
        assert_constant_reference_to!(context, "Foo::Bar", "file:///bar.rb:1:2-1:12");

        assert_eq!(
            context
                .graph
                .constant_references
                .iter()
                .filter(|(_, r)| matches!(r, ConstantReference::Unresolved(_)))
                .count(),
            3,
            "Should have 3 unresolved references"
        );
        assert_unresolved_constant!(context, "Baz", None, "file:///foo.rb:5:4-5:9");
        assert_unresolved_constant!(context, "String", Some("Foo::Bar"), "file:///foo.rb:6:4-6:10");
        assert_unresolved_constant!(context, "Object", None, "file:///foo.rb:7:4-7:12");
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
        let foo = context.graph.declarations.get(&DeclarationId::from("Foo")).unwrap();
        assert!(foo.members().contains_key(&NameId::from("Bar")));

        // Delete `Bar`
        context.index_uri("file:///foo2.rb", {
            r"
            module Foo
            end
            "
        });

        let foo = context.graph.declarations.get(&DeclarationId::from("Foo")).unwrap();
        assert!(!foo.members().contains_key(&NameId::from("Bar")));
    }
}
