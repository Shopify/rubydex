use std::collections::HashMap;
use std::error::Error;

use crate::model::db::Db;
use crate::model::definitions::Definition;
use crate::model::identity_maps::{IdentityHashBuilder, IdentityHashMap, IdentityHashSet};
use crate::model::ids::{DefinitionId, NameId, UriId};
use crate::model::integrity::IntegrityChecker;

// The `Graph` is the global representation of the entire Ruby codebase. It contains all declarations and their
// relationships
#[derive(Default, Debug)]
pub struct Graph {
    // *** Graph nodes: the following represent possible nodes in our graph ***

    // Map of fully qualified names. These represent global declarations, like `Foo`, `Foo#bar` or `Foo.baz`
    names: IdentityHashMap<NameId, String>,
    // Map of URIs currently loaded in memory
    uri_pool: IdentityHashMap<UriId, String>,
    // Map of definitions
    definitions: IdentityHashMap<DefinitionId, Definition>,

    // *** Graph edges: the following represent relationships between nodes ***

    // Map of a fully qualified name to all definitions we discovered for it
    name_to_definitions: IdentityHashMap<NameId, IdentityHashSet<DefinitionId>>,
    // Map of URI to all definitions discovered in that document
    uris_to_definitions: IdentityHashMap<UriId, IdentityHashSet<DefinitionId>>,
    db: Db,
}

impl Graph {
    #[must_use]
    pub fn new() -> Self {
        Self {
            names: HashMap::with_hasher(IdentityHashBuilder),
            definitions: HashMap::with_hasher(IdentityHashBuilder),
            uri_pool: HashMap::with_hasher(IdentityHashBuilder),
            name_to_definitions: HashMap::with_hasher(IdentityHashBuilder),
            uris_to_definitions: HashMap::with_hasher(IdentityHashBuilder),
            db: Db::new(),
        }
    }

    // Returns an immutable reference to the names map
    #[must_use]
    pub fn names(&self) -> &IdentityHashMap<NameId, String> {
        &self.names
    }

    // Returns an immutable reference to the definitions map
    #[must_use]
    pub fn definitions(&self) -> &IdentityHashMap<DefinitionId, Definition> {
        &self.definitions
    }

    // Returns an immutable reference to the URI pool map
    #[must_use]
    pub fn uri_pool(&self) -> &IdentityHashMap<UriId, String> {
        &self.uri_pool
    }

    // Returns an immutable reference to the name to definitions map
    #[must_use]
    pub fn name_to_definitions(&self) -> &IdentityHashMap<NameId, IdentityHashSet<DefinitionId>> {
        &self.name_to_definitions
    }

    // Returns an immutable reference to the URI to definitions map
    #[must_use]
    pub fn uris_to_definitions(&self) -> &IdentityHashMap<UriId, IdentityHashSet<DefinitionId>> {
        &self.uris_to_definitions
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
        let owned_definitions = self.name_to_definitions.get(&name_id)?;

        Some(
            owned_definitions
                .iter()
                .filter_map(|id| self.definitions.get(id))
                .collect(),
        )
    }

    // Registers a URI into the graph and returns the generated ID. This happens once when starting to index the URI and
    // then all definitions discovered in it get associated to the ID
    pub fn add_uri(&mut self, uri: String) -> UriId {
        let uri_id = UriId::from(&uri);
        self.uri_pool.insert(uri_id, uri);
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
    pub fn add_definition(&mut self, uri_id: UriId, name: String, definition: Definition) {
        let definition_id = DefinitionId::from(&format!("{uri_id}{}", definition.start()));
        let name_id = *definition.name_id();

        self.names.insert(name_id, name);
        self.definitions.insert(definition_id, definition);
        self.name_to_definitions
            .entry(name_id)
            .or_default()
            .insert(definition_id);
        self.uris_to_definitions
            .entry(uri_id)
            .or_default()
            .insert(definition_id);
    }

    /// # Errors
    ///
    /// Any database errors will prevent the data from being loaded
    pub fn load_uri(&mut self, uri: String) -> Result<(), Box<dyn Error>> {
        let uri_id = self.add_uri(uri);
        let loaded_data = self.db.load_uri(uri_id.to_string())?;

        for load_result in loaded_data {
            let name_id = NameId::new(load_result.name_id);
            let definition_id = DefinitionId::new(load_result.definition_id);

            self.names.insert(name_id, load_result.name);
            self.definitions.insert(definition_id, load_result.definition);
            self.name_to_definitions
                .entry(name_id)
                .or_default()
                .insert(definition_id);
            self.uris_to_definitions
                .entry(uri_id)
                .or_default()
                .insert(definition_id);
        }

        Ok(())
    }

    /// Removes all data related to the given URI from memory. This method should only be used for when a document is
    /// closed. If the file is deleted, we also need to update the database
    pub fn unload_uri(&mut self, uri: &str) {
        let uri_id = UriId::from(uri);
        self.remove_definitions_for_uri(uri_id);
        self.uri_pool.remove(&uri_id);
    }

    /// Merges everything in `other` into this Graph. This method is meant to merge all graph representations from
    /// different threads, but not meant to handle updates to the existing global representation
    pub fn extend(&mut self, incomplete_index: Graph) {
        self.names.extend(incomplete_index.names);
        self.definitions.extend(incomplete_index.definitions);
        self.uri_pool.extend(incomplete_index.uri_pool);
        self.uris_to_definitions.extend(incomplete_index.uris_to_definitions);

        for (name_id, definition_ids) in incomplete_index.name_to_definitions {
            self.name_to_definitions
                .entry(name_id)
                .or_default()
                .extend(definition_ids);
        }
    }

    /// Updates the global representation with the information contained in `other`, handling deletions, insertions and
    /// updates to existing entries
    pub fn update(&mut self, other: Graph) {
        // For each URI that was indexed through `other`, check what was discovered and update our current global
        // representation
        for uri_id in other.uri_pool.keys() {
            self.remove_definitions_for_uri(*uri_id);
        }

        self.extend(other);
    }

    // Removes all nodes and relationships associated to the given URI. This is used to clean up stale data when a
    // document (identified by `uri_id`) changes or when a document is closed and we need to clean up the memory
    fn remove_definitions_for_uri(&mut self, uri_id: UriId) {
        if let Some(definitions) = self.uris_to_definitions.remove(&uri_id) {
            for def_id in definitions {
                if let Some(definition) = self.definitions.remove(&def_id)
                    && let Some(name_definitions) = self.name_to_definitions.get_mut(definition.name_id())
                {
                    name_definitions.remove(&def_id);

                    if name_definitions.is_empty() {
                        self.names.remove(definition.name_id());
                        self.name_to_definitions.remove(definition.name_id());
                    }
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

        checker.add_rule(
            "Each `names` name has at least one definition in `name_to_definitions`",
            |index, errors| {
                for name_id in index.names().keys() {
                    if let Some(definitions) = index.name_to_definitions().get(name_id) {
                        if definitions.is_empty() {
                            let name = index.names().get(name_id).map_or("<unknown>", String::as_str);
                            errors.push(format!("Name '{name}' has no definitions in `name_to_definitions`"));
                        }
                    } else {
                        let name = index.names().get(name_id).map_or("<unknown>", String::as_str);
                        errors.push(format!("Name '{name}' has no entries in `name_to_definitions`"));
                    }
                }
            },
        );

        checker.add_rule(
            "Each `definition name` name is registered in `names`",
            |index, errors| {
                for definition in index.definitions().values() {
                    let name = definition.name_id();
                    if !index.names().contains_key(name) {
                        errors.push(format!(
                            "Definition name '{name}' exists in `definition_to_name` but not in `names`"
                        ));
                    }
                }
            },
        );

        checker.add_rule(
            "Each `definition_to_name` name is registered in `name_to_definitions`",
            |index, errors| {
                for definition in index.definitions().values() {
                    let name_id = definition.name_id();
                    if !index.name_to_definitions().contains_key(name_id) {
                        errors.push(format!(
                            "Name '{name_id}' exists in `definition_to_name` but not in `name_to_definitions`"
                        ));
                    }
                }
            },
        );

        checker.add_rule(
            "Each `name_to_definitions` name is registered in `names`",
            |index, errors| {
                for name_id in index.name_to_definitions().keys() {
                    if !index.names().contains_key(name_id) {
                        errors.push(format!(
                            "Name '{name_id}' exists in `name_to_definitions` but not in `names`"
                        ));
                    }
                }
            },
        );

        checker.add_rule(
            "Each `name_to_definitions` definition is registered in `definitions`",
            |index, errors| {
                for definition_ids in index.name_to_definitions().values() {
                    for definition_id in definition_ids {
                        if !index.definitions().contains_key(definition_id) {
                            errors.push(format!(
                                "Definition '{definition_id}' exists in `name_to_definitions` but not in `definitions`"
                            ));
                        }
                    }
                }
            },
        );

        checker.add_rule(
            "Each `name_to_definitions` definition is registered in `definitions`",
            |index, errors| {
                for (name_id, definition_ids) in index.name_to_definitions() {
                    for definition_id in definition_ids {
                        if index.definitions().get(definition_id).map(super::definitions::Definition::name_id) != Some(name_id) {
                            errors.push(format!(
                                "Definition '{definition_id}' exists in `name_to_definitions` but not in `definition_to_name`"
                            ));
                        }
                    }
                }
            },
        );

        checker.add_rule(
            "Each `uris_to_definitions` URI is registered in `uri_pool`",
            |index, errors| {
                for uri_id in index.uris_to_definitions().keys() {
                    if !index.uri_pool().contains_key(uri_id) {
                        errors.push(format!(
                            "URI id '{uri_id}' is registered in `uris_to_definitions` but not in `uri_pool`"
                        ));
                    }
                }
            },
        );

        checker.add_rule(
            "Each `uris_to_definitions` definition is registered in `definitions`",
            |index, errors| {
                for definition_ids in index.uris_to_definitions().values() {
                    for definition_id in definition_ids {
                        if !index.definitions().contains_key(definition_id) {
                            errors.push(format!(
                                "Definition '{definition_id}' is registered in `uris_to_definitions` but not in `definitions`"
                            ));
                        }
                    }
                }
            },
        );

        checker.add_rule(
            "Each `definitions` definition has at least one entry in `uris_to_definitions`",
            |index, errors| {
                for definition_id in index.definitions().keys() {
                    if !index
                        .uris_to_definitions()
                        .values()
                        .any(|ids| ids.contains(definition_id))
                    {
                        errors.push(format!(
                            "Definition '{definition_id}' exists in `definitions` but not in `uris_to_definitions`"
                        ));
                    }
                }
            },
        );

        checker.add_rule("Each `definitions` URI is registered in `uri_pool`", |index, errors| {
            for definition in index.definitions().values() {
                let uri_id = definition.uri_id();
                if !index.uri_pool().contains_key(uri_id) {
                    errors.push(format!(
                        "URI id '{uri_id}' is registered in `definition_to_uri` but not in `uri_pool`"
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
        self.names = HashMap::with_hasher(IdentityHashBuilder);
        self.definitions = HashMap::with_hasher(IdentityHashBuilder);
        self.uri_pool = HashMap::with_hasher(IdentityHashBuilder);
        self.name_to_definitions = HashMap::with_hasher(IdentityHashBuilder);
        self.uris_to_definitions = HashMap::with_hasher(IdentityHashBuilder);
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
        assert!(context.graph.names.is_empty());
        assert!(context.graph.name_to_definitions.is_empty());
        assert!(context.graph.uris_to_definitions.is_empty());
        assert!(context.graph.uri_pool.is_empty());

        context.graph.assert_integrity();
    }

    #[test]
    fn updating_index_with_deleted_definitions() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        // Update with empty content to remove definitions but keep the URI
        context.index_uri("file:///foo.rb", "");

        assert!(context.graph.definitions.is_empty());
        assert!(context.graph.names.is_empty());
        assert!(context.graph.name_to_definitions.is_empty());
        assert!(context.graph.uris_to_definitions.is_empty());
        // URI remains if the file was not deleted, but definitions got erased
        assert_eq!(context.graph.uri_pool.len(), 1);

        context.graph.assert_integrity();
    }

    #[test]
    fn updating_index_with_new_definitions() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");

        assert_eq!(context.graph.definitions.len(), 1);
        assert_eq!(context.graph.names.get(&NameId::from("Foo")).unwrap(), "Foo");
        assert_eq!(
            context.graph.uri_pool.get(&UriId::from("file:///foo.rb")).unwrap(),
            "file:///foo.rb"
        );
        assert_eq!(
            context
                .graph
                .name_to_definitions
                .get(&NameId::from("Foo"))
                .unwrap()
                .len(),
            1
        );
        assert_eq!(
            context
                .graph
                .uris_to_definitions
                .get(&UriId::from("file:///foo.rb"))
                .unwrap()
                .len(),
            1
        );

        context.graph.assert_integrity();
    }

    #[test]
    fn updating_existing_definitions() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        // Update with the same definition but at a different position (with content before it)
        context.index_uri("file:///foo.rb", "\n\n\n\n\n\nmodule Foo; end");

        assert_eq!(context.graph.definitions.len(), 1);
        assert_eq!(context.graph.names.get(&NameId::from("Foo")).unwrap(), "Foo");
        assert_eq!(
            context.graph.uri_pool.get(&UriId::from("file:///foo.rb")).unwrap(),
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
        assert!(context.graph.names.is_empty());
        assert!(context.graph.uri_pool.is_empty());
        assert!(context.graph.name_to_definitions.is_empty());
        assert!(context.graph.uris_to_definitions.is_empty());
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
        assert!(context.graph.names.is_empty());
        assert!(context.graph.uri_pool.is_empty());
        assert!(context.graph.name_to_definitions.is_empty());
        assert!(context.graph.uris_to_definitions.is_empty());

        assert!(context.graph.get("Foo").is_none());

        context.graph.load_uri("file:///foo.rb".to_string()).unwrap();
        assert_eq!(context.graph.definitions.len(), 1);
        assert_eq!(context.graph.names.len(), 1);
        assert_eq!(context.graph.uri_pool.len(), 1);
        assert_eq!(context.graph.name_to_definitions.len(), 1);
        assert_eq!(context.graph.uris_to_definitions.len(), 1);
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
        assert_eq!(context.graph.names.len(), 1);
        assert_eq!(context.graph.uri_pool.len(), 1);
        assert_eq!(context.graph.name_to_definitions.len(), 1);
        assert_eq!(context.graph.uris_to_definitions.len(), 1);

        context.graph.unload_uri("file:///foo.rb");
        assert!(context.graph.definitions.is_empty());
        assert!(context.graph.names.is_empty());
        assert!(context.graph.uri_pool.is_empty());
        assert!(context.graph.name_to_definitions.is_empty());
        assert!(context.graph.uris_to_definitions.is_empty());
    }
}
