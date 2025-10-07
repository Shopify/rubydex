use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::error::Error;

use crate::model::db::Db;
use crate::model::declaration::Declaration;
use crate::model::definitions::Definition;
use crate::model::document::Document;
use crate::model::identity_maps::{IdentityHashBuilder, IdentityHashMap};
use crate::model::ids::{DefinitionId, NameId, UriId};
use crate::model::integrity::IntegrityChecker;

// The `Graph` is the global representation of the entire Ruby codebase. It contains all declarations and their
// relationships
#[derive(Default, Debug)]
pub struct Graph {
    // Map of declaration nodes
    declarations: IdentityHashMap<NameId, Declaration>,
    // Map of document nodes
    documents: IdentityHashMap<UriId, Document>,
    // Map of definition nodes
    definitions: IdentityHashMap<DefinitionId, Definition>,
    db: Db,
}

impl Graph {
    #[must_use]
    pub fn new() -> Self {
        Self {
            declarations: HashMap::with_hasher(IdentityHashBuilder),
            definitions: HashMap::with_hasher(IdentityHashBuilder),
            documents: HashMap::with_hasher(IdentityHashBuilder),
            db: Db::new(),
        }
    }

    // Returns an immutable reference to the declarations map
    #[must_use]
    pub fn declarations(&self) -> &IdentityHashMap<NameId, Declaration> {
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

    /// # Errors
    ///
    /// May error if we fail to initialize the database connection at the specified path
    pub fn set_configuration(&mut self, db_path: String) -> Result<(), Box<dyn Error>> {
        self.db.initialize_connection(Some(db_path))
    }

    #[must_use]
    pub fn get(&self, name: &str) -> Option<Vec<&Definition>> {
        let name_id = NameId::from(name);
        let declaration = self.declarations.get(&name_id)?;

        Some(
            declaration
                .definitions()
                .iter()
                .filter_map(|id| self.definitions.get(id))
                .collect(),
        )
    }

    #[must_use]
    pub fn get_documentation(&self, name: &str) -> Option<String> {
        let definitions = self.get(name)?;
        let comments: Vec<&str> = definitions
            .iter()
            .map(|def| def.comments())
            .filter(|comments| !comments.is_empty())
            .collect();

        if comments.is_empty() {
            None
        } else {
            Some(comments.join("\n"))
        }
    }

    // Registers a URI into the graph and returns the generated ID. This happens once when starting to index the URI and
    // then all definitions discovered in it get associated to the ID
    pub fn add_uri(&mut self, uri: String) -> UriId {
        let uri_id = UriId::from(&uri);
        self.documents.entry(uri_id).or_insert_with(|| Document::new(uri));
        uri_id
    }

    /// Handles when a document (identified by `uri`) is deleted. This removes the URI from the graph along with all
    /// relationships and definitions that had been discovered in it
    ///
    /// # Errors
    ///
    /// Any database errors will prevent the data from being deleted
    pub fn delete_uri(&mut self, uri: &str) -> Result<(), Box<dyn Error>> {
        // Delete the data from memory
        self.unload_uri(uri);
        // Delete the data from the database
        let uri_id = UriId::from(uri);
        self.db.delete_data_for_uri(uri_id)?;
        Ok(())
    }

    // Registers a definition into the `Graph`, automatically creating all relationships
    pub fn add_definition(&mut self, name: String, definition: Definition) {
        let uri_id = *definition.uri_id();
        let definition_id = DefinitionId::from(&format!("{uri_id}{}{}", definition.start(), &name));
        let name_id = *definition.name_id();

        self.declarations
            .entry(name_id)
            .or_insert_with(|| Declaration::new(name))
            .add_definition(definition_id);
        self.definitions.insert(definition_id, definition);
        self.documents
            .entry(uri_id)
            .and_modify(|doc| doc.add_definition(definition_id));
    }

    /// # Errors
    ///
    /// Any database errors will prevent the data from being loaded
    pub fn load_uri(&mut self, uri: String) -> Result<(), Box<dyn Error>> {
        let uri_id = self.add_uri(uri);
        let loaded_data = self.db.load_uri(uri_id)?;

        for load_result in loaded_data {
            let name_id = load_result.name_id;
            let definition_id = load_result.definition_id;

            self.declarations
                .entry(name_id)
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
    pub fn unload_uri(&mut self, uri: &str) {
        let uri_id = UriId::from(uri);
        self.remove_definitions_for_uri(uri_id);
        self.documents.remove(&uri_id);
    }

    /// Merges everything in `other` into this Graph. This method is meant to merge all graph representations from
    /// different threads, but not meant to handle updates to the existing global representation
    pub fn extend(&mut self, incomplete_index: Graph) {
        self.definitions.extend(incomplete_index.definitions);

        for (name_id, declaration) in incomplete_index.declarations {
            match self.declarations.entry(name_id) {
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
        if let Some(document) = self.documents.remove(&uri_id) {
            for def_id in document.definitions() {
                if let Some(definition) = self.definitions.remove(def_id)
                    && let Some(declaration) = self.declarations.get_mut(definition.name_id())
                    && declaration.remove_definition(def_id)
                    && declaration.is_empty()
                {
                    self.declarations.remove(definition.name_id());
                }
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
                if declaration.definitions().is_empty() {
                    errors.push(format!(
                        "Declaration '{}' exists in `declarations`, but is not associated to any definitions",
                        declaration.name()
                    ));
                }
            }
        });

        checker.add_rule(
            "Each `definition` name_id is registered in `declarations`",
            |index, errors| {
                for definition in index.definitions().values() {
                    let name_id = definition.name_id();
                    if !index.declarations().contains_key(name_id) {
                        errors.push(format!(
                            "Name '{name_id}' is referenced by a definition but not present in `declarations`"
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

    // Clear graph data from memory
    pub fn clear_graph_data(&mut self) {
        self.declarations = HashMap::with_hasher(IdentityHashBuilder);
        self.definitions = HashMap::with_hasher(IdentityHashBuilder);
        self.documents = HashMap::with_hasher(IdentityHashBuilder);
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
        assert!(context.graph.declarations.is_empty());
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
        assert!(context.graph.declarations.is_empty());
        // URI remains if the file was not deleted, but definitions got erased
        assert_eq!(context.graph.documents.len(), 1);

        context.graph.assert_integrity();
    }

    #[test]
    fn updating_index_with_new_definitions() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");

        assert_eq!(context.graph.definitions.len(), 1);
        let declaration = context.graph.declarations.get(&NameId::from("Foo")).unwrap();
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
        let declaration = context.graph.declarations.get(&NameId::from("Foo")).unwrap();
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

        let doc = context.graph.get_documentation("CommentedClass");
        assert_eq!(doc, Some("This is a class comment\nMulti-line comment".to_string()));

        let doc = context.graph.get_documentation("CommentedModule");
        assert_eq!(doc, Some("Module comment".to_string()));

        let doc = context.graph.get_documentation("NoCommentClass");
        assert_eq!(doc, None);

        let doc = context.graph.get_documentation("NonExistent");
        assert_eq!(doc, None);
    }
}
