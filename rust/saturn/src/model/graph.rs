use std::collections::hash_map::Entry;
use std::error::Error;

use crate::model::db::Db;
use crate::model::declaration::Declaration;
use crate::model::definitions::Definition;
use crate::model::document::Document;
use crate::model::identity_maps::IdentityHashMap;
use crate::model::ids::{DeclarationId, DefinitionId, NameId, UriId};
use crate::model::integrity::IntegrityChecker;
use crate::model::references::UnresolvedReference;

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
    // List of references that still need to be resolved
    unresolved_references: Vec<UnresolvedReference>,
    db: Db,
}

impl Graph {
    #[must_use]
    pub fn new() -> Self {
        let mut declarations = IdentityHashMap::default();
        // Insert the magic top level self <main> object into the graph, so that we can associate global variables or
        // definitions made at the top level with it
        declarations.insert(DeclarationId::from("<main>"), Declaration::new(String::from("<main>")));

        Self {
            declarations,
            definitions: IdentityHashMap::default(),
            documents: IdentityHashMap::default(),
            names: IdentityHashMap::default(),
            unresolved_references: Vec::new(),
            db: Db::new(),
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

    // Returns an immutable reference to the URI pool map
    #[must_use]
    pub fn documents(&self) -> &IdentityHashMap<UriId, Document> {
        &self.documents
    }

    // Returns an immutable reference to the list of unresolved references
    #[must_use]
    pub fn unresolved_references(&self) -> &Vec<UnresolvedReference> {
        &self.unresolved_references
    }

    /// # Errors
    ///
    /// May error if we fail to initialize the database connection at the specified path
    pub fn set_configuration(&mut self, db_path: String) -> Result<(), Box<dyn Error>> {
        self.db.initialize_connection(Some(db_path))
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

    // Registers a URI into the graph and returns the generated ID. This happens once when starting to index the URI and
    // then all definitions discovered in it get associated to the ID
    pub fn add_uri(&mut self, uri: String, content_hash: u16) -> UriId {
        let uri_id = UriId::from(&uri);
        self.documents
            .entry(uri_id)
            .or_insert_with(|| Document::new(uri, content_hash));
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

    /// Handles when a document (identified by `uri`) is deleted. This removes the URI from the graph along with all
    /// relationships and definitions that had been discovered in it
    ///
    /// # Errors
    ///
    /// Any database errors will prevent the data from being deleted
    pub fn delete_uri(&mut self, uri: &str) -> Result<(), Box<dyn Error>> {
        let removed_ids = self.unload_uri(uri);
        let uri_id = UriId::from(uri);
        self.db.delete_data_for_uri(uri_id, &removed_ids)?;
        Ok(())
    }

    // Registers a definition into the `Graph`, automatically creating all relationships
    pub fn add_definition(&mut self, name: String, definition: Definition) {
        let uri_id = *definition.uri_id();
        let definition_id = DefinitionId::from(&format!("{uri_id}{}{}", definition.start(), &name));
        let declaration_id = *definition.declaration_id();

        self.declarations
            .entry(declaration_id)
            .or_insert_with(|| Declaration::new(name))
            .add_definition(definition_id);
        self.definitions.insert(definition_id, definition);
        self.documents
            .entry(uri_id)
            .and_modify(|doc| doc.add_definition(definition_id));
    }

    // Register an unresolved reference to something (e.g.: constant, method, variable), which has to be resolved later
    pub fn add_unresolved_reference(&mut self, reference: UnresolvedReference) {
        self.unresolved_references.push(reference);
    }

    /// Attempts to resolve a reference against the graph. Returns the fully qualified declaration ID that the reference
    /// is related to or `None`
    pub fn resolve_reference(&self, reference: &UnresolvedReference) -> Option<&Declaration> {
        match reference {
            UnresolvedReference::Constant(constant) => {
                if let Some(_nesting) = constant.nesting() {
                    // Not implemented yet
                    None
                } else {
                    // Top level reference

                    // Note: this code is temporary. Once we have RBS indexing, we can simply enter the graph by looking
                    // up `Object` and then we search its members for the top level constant
                    let name = self.names.get(constant.name_id())?;
                    let declaration_id = DeclarationId::from(name);
                    self.declarations.get(&declaration_id)
                }
            }
        }
    }

    /// # Errors
    ///
    /// Any database errors will prevent the data from being loaded
    pub fn load_uri(&mut self, uri: String) -> Result<(), Box<dyn Error>> {
        let uri_id = UriId::from(&uri);
        let loaded_data = self.db.load_uri(uri_id)?;
        self.add_uri(uri, loaded_data.content_hash);

        for load_result in loaded_data.definitions {
            let declaration_id = load_result.declaration_id;
            let definition_id = load_result.definition_id;

            self.declarations
                .entry(declaration_id)
                .or_insert_with(|| Declaration::new(load_result.name))
                .add_definition(definition_id);

            self.definitions.insert(definition_id, load_result.definition);
            self.documents
                .entry(uri_id)
                .and_modify(|doc| doc.add_definition(definition_id));
        }

        Ok(())
    }

    /// Removes all data related to the given URI from memory. This method should only be used for when a document is
    /// closed. If the file is deleted, we also need to update the database
    pub fn unload_uri(&mut self, uri: &str) -> RemovedIds {
        let uri_id = UriId::from(uri);
        let removed = self.remove_definitions_for_uri(uri_id);
        self.documents.remove(&uri_id);
        removed
    }

    /// Get a list of uris and their content hashes from db.
    /// This is used for synchronization where we will compare the content hashes
    /// with the current documents to determine if a document needs updating.
    ///
    /// # Errors
    ///
    /// Any database errors will prevent the data from being loaded
    pub fn get_all_cached_content_hashes(&self) -> Result<IdentityHashMap<UriId, u16>, Box<dyn Error>> {
        self.db.get_all_content_hashes()
    }

    /// Merges everything in `other` into this Graph. This method is meant to merge all graph representations from
    /// different threads, but not meant to handle updates to the existing global representation
    pub fn extend(&mut self, incomplete_index: Graph) {
        self.definitions.extend(incomplete_index.definitions);
        self.names.extend(incomplete_index.names);
        self.unresolved_references
            .extend(incomplete_index.unresolved_references);

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
    fn remove_definitions_for_uri(&mut self, uri_id: UriId) -> RemovedIds {
        let mut removed = RemovedIds {
            definition_ids: Vec::new(),
            declaration_ids: Vec::new(),
        };

        if let Some(document) = self.documents.remove(&uri_id) {
            for def_id in document.definitions() {
                removed.definition_ids.push(*def_id);
                if let Some(definition) = self.definitions.remove(def_id)
                    && let Some(declaration) = self.declarations.get_mut(definition.declaration_id())
                    && declaration.remove_definition(def_id)
                    && declaration.has_no_definitions()
                {
                    self.declarations.remove(definition.declaration_id());
                    removed.declaration_ids.push(*definition.declaration_id());
                }
            }
        }

        removed
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

    /// # Errors
    /// This method will return an error if batch inserting to the DB fails.
    pub fn save_to_database(&self) -> Result<(), Box<dyn Error>> {
        self.db.save_full_graph(self)
    }

    /// Clears all data from the database
    ///
    /// # Errors
    ///
    /// This method will return an error if clearing the database fails.
    pub fn clear_database(&self) -> Result<(), Box<dyn Error>> {
        self.db.clear_database()
    }

    // Clear graph data from memory
    pub fn clear_graph_data(&mut self) {
        self.declarations = IdentityHashMap::default();
        self.definitions = IdentityHashMap::default();
        self.documents = IdentityHashMap::default();
        self.names = IdentityHashMap::default();
        self.unresolved_references = Vec::new();
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
        println!();
        let total_declarations = self.declarations().len();
        println!("Total declarations:         {total_declarations}");
        println!(
            "With documentation:         {} ({:.1}%)",
            declarations_with_docs,
            (declarations_with_docs as f64 / total_declarations as f64) * 100.0
        );
        println!(
            "Without documentation:      {} ({:.1}%)",
            total_declarations - declarations_with_docs,
            ((total_declarations - declarations_with_docs) as f64 / total_declarations as f64) * 100.0
        );
        println!("Total documentation size:   {total_doc_size} bytes");
        println!(
            "Multi-definition names:     {} ({:.1}%)",
            multi_definition_count,
            (f64::from(multi_definition_count) / total_declarations as f64) * 100.0
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
    use crate::test_utils::GraphTest;

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
    fn saving_graph_to_database() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", "module Foo; end");
        context
            .graph
            .set_configuration(String::from("file:save_graph_test?mode=memory&cache=shared"))
            .unwrap();
        context.graph.save_to_database().unwrap();
        context.graph.clear_graph_data();

        assert!(context.graph.definitions.is_empty());
        assert!(context.graph.declarations.is_empty());
        assert!(context.graph.documents.is_empty());
    }

    #[test]
    fn loading_a_document() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", "module Foo; end");
        context
            .graph
            .set_configuration(String::from("file:load_uri_test?mode=memory&cache=shared"))
            .unwrap();
        context.graph.save_to_database().unwrap();
        context.graph.clear_graph_data();

        assert!(context.graph.definitions.is_empty());
        assert!(context.graph.declarations.is_empty());
        assert!(context.graph.documents.is_empty());

        assert!(context.graph.get("Foo").is_none());

        context.graph.load_uri("file:///foo.rb".to_string()).unwrap();
        assert_eq!(context.graph.definitions.len(), 1);
        assert_eq!(context.graph.declarations.len(), 1);
        assert_eq!(context.graph.documents.len(), 1);
    }

    #[test]
    fn unloading_a_document() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", "module Foo; end");
        context
            .graph
            .set_configuration(String::from("file:unload_uri_test?mode=memory&cache=shared"))
            .unwrap();
        context.graph.save_to_database().unwrap();
        context.graph.clear_graph_data();
        assert!(context.graph.get("Foo").is_none());

        context.graph.load_uri("file:///foo.rb".to_string()).unwrap();
        assert_eq!(context.graph.definitions.len(), 1);
        assert_eq!(context.graph.declarations.len(), 1);
        assert_eq!(context.graph.documents.len(), 1);

        let definitions = context.graph.get("Foo").unwrap();
        assert_eq!(definitions.len(), 1);

        context.graph.unload_uri("file:///foo.rb");
        assert!(context.graph.definitions.is_empty());
        assert!(context.graph.declarations.is_empty());
        assert!(context.graph.documents.is_empty());
        assert!(context.graph.get("Foo").is_none());
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
            def.comments()
                .iter()
                .map(|c| c.string().to_string())
                .collect::<Vec<String>>(),
            vec!["# This is a class comment", "# Multi-line comment"]
        );

        let definitions = context.graph.get("CommentedModule").unwrap();
        let def = definitions.first().unwrap();
        assert_eq!(
            def.comments()
                .iter()
                .map(|c| c.string().to_string())
                .collect::<Vec<String>>(),
            vec!["# Module comment"]
        );

        let definitions = context.graph.get("NoCommentClass").unwrap();
        let def = definitions.first().unwrap();
        assert!(def.comments().is_empty());
    }

    #[test]
    fn resolving_top_level_constants() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              class Bar
                ::Bar # should be resolved to Bar from file:///bar.rb
                ::Baz # should not be resolved because it doesn't exist
                String # should not be resolved because it's not top-level
                ::Object # should not be resolved because ::Object does not exist in the graph yet
              end
            end
            "
        });

        context.index_uri("file:///bar.rb", {
            r"
            class Bar
              ::Foo::Bar # should be resolved to Foo::Bar from file:///foo.rb
            end
            "
        });

        // ::Bar should be resolved to the Bar declaration
        let const_ref = context.graph.unresolved_references.remove(0);
        assert_eq!(
            context.graph.resolve_reference(&const_ref).unwrap().name(),
            String::from("Bar")
        );

        // ::Baz doesn't exist
        let const_ref = context.graph.unresolved_references.remove(0);
        assert!(context.graph.resolve_reference(&const_ref).is_none());

        // String is unresolved until we implement following lexical scopes
        let const_ref = context.graph.unresolved_references.remove(0);
        assert!(context.graph.resolve_reference(&const_ref).is_none());

        // Object is unresolved until we implement RBS indexing
        let const_ref = context.graph.unresolved_references.remove(0);
        assert!(context.graph.resolve_reference(&const_ref).is_none());

        // ::Foo::Bar should be resolved to the Foo::Bar declaration
        let const_ref = context.graph.unresolved_references.remove(0);
        assert_eq!(
            context.graph.resolve_reference(&const_ref).unwrap().name(),
            String::from("Foo::Bar")
        );
    }
}
