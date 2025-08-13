use std::collections::{HashMap, HashSet};

use crate::model::definitions::Definition;
use crate::model::ids::{DefinitionId, NameId, UriId};
use crate::model::local_index::LocalIndex;

// The `Index` is the global graph representation of the entire Ruby codebase. It contains all declarations and their
// relationships
#[derive(Default, Debug)]
pub struct Index {
    // *** Graph nodes: the following represent possible nodes in our graph ***
    // Map of fully qualified names. These represent global declarations, like `Foo`, `Foo#bar` or `Foo.baz`
    names: HashMap<NameId, String>,
    // Map of URIs currently loaded in memory
    uri_pool: HashMap<UriId, String>,
    // Map of definitions
    definitions: HashMap<DefinitionId, Definition>,

    // *** Graph edges: the following represent relationships between nodes ***
    // Map of a fully qualified name to all definitions we discovered for it
    name_to_definitions: HashMap<NameId, HashSet<DefinitionId>>,
    // Reverse map of a definition to its unique fully qualified name
    definition_to_name: HashMap<DefinitionId, NameId>,
    // Map of URI to all definitions discovered in that document
    uris_to_definitions: HashMap<UriId, HashSet<DefinitionId>>,
}

impl Index {
    #[must_use]
    pub fn new() -> Self {
        Self {
            names: HashMap::new(),
            definitions: HashMap::new(),
            uri_pool: HashMap::new(),
            name_to_definitions: HashMap::new(),
            definition_to_name: HashMap::new(),
            uris_to_definitions: HashMap::new(),
        }
    }

    #[must_use]
    pub fn get(&self, name: &str) -> Option<Vec<&Definition>> {
        let name_id = NameId::new(name);
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
        let uri_id = UriId::new(&uri);
        self.uri_pool.insert(uri_id, uri);
        uri_id
    }

    // Handles when a document (identified by `uri`) is deleted. This removes the URI from the graph along with all
    // relationships and definitions that had been discovered in it
    pub fn delete_uri(&mut self, uri: &str) {
        let uri_id = UriId::new(uri);
        self.remove_definitions_for_uri(uri_id);
        self.uri_pool.remove(&uri_id);
    }

    // Registers a definition into the `Index`, automatically creating all relationships in the graph
    pub fn add_definition(&mut self, uri_id: UriId, name: String, definition: Definition) {
        let name_id = NameId::new(&name);
        let definition_id = DefinitionId::new(uri_id, definition.start_offset());

        self.names.insert(name_id, name);
        self.definitions.insert(definition_id, definition);
        self.name_to_definitions
            .entry(name_id)
            .or_default()
            .insert(definition_id);
        self.definition_to_name.insert(definition_id, name_id);
        self.uris_to_definitions
            .entry(uri_id)
            .or_default()
            .insert(definition_id);
    }

    /// Merges a `LocalIndex` into this global Index. This is the primary way to add definitions
    /// collected during single-file indexing into the global state.
    pub fn extend_from_local(&mut self, local_index: LocalIndex) {
        self.names.extend(local_index.names);
        self.definitions.extend(local_index.definitions);
        self.definition_to_name.extend(local_index.definition_to_name);
        self.uris_to_definitions.extend(local_index.uris_to_definitions);

        // Merge name_to_definitions, handling potential collisions
        for (name_id, definition_ids) in local_index.name_to_definitions {
            self.name_to_definitions
                .entry(name_id)
                .or_default()
                .extend(definition_ids);
        }
    }


    /// Updates the global representation with a LocalIndex, handling deletions, insertions and
    /// updates to existing entries for the URI contained in the LocalIndex
    pub fn update_from_local(&mut self, local_index: LocalIndex) {
        let uri_id = local_index.uri_id();
        self.remove_definitions_for_uri(uri_id);
        self.extend_from_local(local_index);
    }


    // Removes all nodes and relationships associated to the given URI. This is used to clean up stale data when a
    // document (identified by `uri_id`) changes
    fn remove_definitions_for_uri(&mut self, uri_id: UriId) {
        if let Some(definitions) = self.uris_to_definitions.remove(&uri_id) {
            for def_id in definitions {
                if let Some(name_id) = self.definition_to_name.remove(&def_id)
                    && let Some(definitions) = self.name_to_definitions.get_mut(&name_id)
                {
                    definitions.remove(&def_id);
                    if definitions.is_empty() {
                        self.names.remove(&name_id);
                        self.name_to_definitions.remove(&name_id);
                    }
                }

                self.definitions.remove(&def_id);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::indexing::ruby_indexer::RubyIndexer;

    // Index the given source and URI and return the resulting `Index`
    fn index_source(uri: &str, source: &str) -> Index {
        let mut indexer = RubyIndexer::new(uri.to_string());
        indexer.index(source);
        let (local_index, _errors) = indexer.into_parts();

        let mut index = Index::new();
        index.add_uri(uri.to_string());
        index.extend_from_local(local_index);
        index
    }

    #[test]
    fn deleting_a_uri() {
        let mut global = index_source("file:///foo.rb", "module Foo; end");
        global.delete_uri("file:///foo.rb");

        assert!(global.definitions.is_empty());
        assert!(global.names.is_empty());
        assert!(global.definition_to_name.is_empty());
        assert!(global.name_to_definitions.is_empty());
        assert!(global.uris_to_definitions.is_empty());
        assert!(global.uri_pool.is_empty());
    }

    #[test]
    fn updating_index_with_deleted_definitions() {
        let mut global = index_source("file:///foo.rb", "module Foo; end");
        
        let mut indexer = RubyIndexer::new("file:///foo.rb".to_string());
        indexer.index("");
        let (local_index, _errors) = indexer.into_parts();
        global.update_from_local(local_index);

        assert!(global.definitions.is_empty());
        assert!(global.names.is_empty());
        assert!(global.definition_to_name.is_empty());
        assert!(global.name_to_definitions.is_empty());
        assert!(global.uris_to_definitions.is_empty());
        // URI remains if the file was not deleted, but definitions got erased
        assert_eq!(global.uri_pool.len(), 1);
    }

    #[test]
    fn updating_index_with_new_definitions() {
        let mut global = Index::new();
        global.add_uri("file:///foo.rb".to_string());
        
        let mut indexer = RubyIndexer::new("file:///foo.rb".to_string());
        indexer.index("module Foo; end");
        let (local_index, _errors) = indexer.into_parts();
        global.update_from_local(local_index);

        assert_eq!(global.definitions.len(), 1);
        assert_eq!(global.names.get(&NameId::new("Foo")).unwrap(), "Foo");
        assert_eq!(
            global.uri_pool.get(&UriId::new("file:///foo.rb")).unwrap(),
            "file:///foo.rb"
        );
        assert_eq!(global.name_to_definitions.get(&NameId::new("Foo")).unwrap().len(), 1);
        assert_eq!(
            global
                .uris_to_definitions
                .get(&UriId::new("file:///foo.rb"))
                .unwrap()
                .len(),
            1
        );
    }

    #[test]
    fn updating_existing_definitions() {
        let mut global = index_source("file:///foo.rb", "module Foo; end");
        
        let mut indexer = RubyIndexer::new("file:///foo.rb".to_string());
        indexer.index("\n\n\n\n\n\nmodule Foo; end");
        let (local_index, _errors) = indexer.into_parts();
        global.update_from_local(local_index);

        assert_eq!(global.definitions.len(), 1);
        assert_eq!(global.names.get(&NameId::new("Foo")).unwrap(), "Foo");
        assert_eq!(
            global.uri_pool.get(&UriId::new("file:///foo.rb")).unwrap(),
            "file:///foo.rb"
        );

        let definitions = global.get("Foo").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start_offset(), 6);
    }

    #[test]
    fn adding_another_definition_from_a_different_uri() {
        let mut global = index_source("file:///foo.rb", "module Foo; end");
        global.add_uri("file:///foo2.rb".to_string());
        
        let mut indexer = RubyIndexer::new("file:///foo2.rb".to_string());
        indexer.index("\n\n\n\n\nmodule Foo; end");
        let (local_index, _errors) = indexer.into_parts();
        global.update_from_local(local_index);

        let definitions = global.get("Foo").unwrap();
        let mut offsets = definitions.iter().map(|d| d.start_offset()).collect::<Vec<_>>();
        offsets.sort_unstable();
        assert_eq!(definitions.len(), 2);
        assert_eq!(vec![0, 5], offsets);
    }

    #[test]
    fn adding_a_second_definition_from_the_same_uri() {
        let mut global = index_source("file:///foo.rb", "module Foo; end");
        
        let mut indexer = RubyIndexer::new("file:///foo.rb".to_string());
        indexer.index("module Foo; end\n\n\nmodule Foo; end");
        let (local_index, _errors) = indexer.into_parts();
        global.update_from_local(local_index);

        let definitions = global.get("Foo").unwrap();
        let mut offsets = definitions.iter().map(|d| d.start_offset()).collect::<Vec<_>>();
        offsets.sort_unstable();
        assert_eq!(definitions.len(), 2);
        assert_eq!(vec![0, 18], offsets);
    }
}
