use dashmap::mapref::one::Ref;
use dashmap::{DashMap, DashSet};

use crate::model::definitions::Definition;
use crate::model::ids::{DefinitionId, NameId, UriId};

// The `Graph` is the global representation of the entire Ruby codebase. It contains all declarations and their
// relationships
#[derive(Default, Debug)]
pub struct Graph {
    // *** Graph nodes: the following represent possible nodes in our graph ***
    // Map of fully qualified names. These represent global declarations, like `Foo`, `Foo#bar` or `Foo.baz`
    names: DashMap<NameId, String>,
    // Map of URIs currently loaded in memory
    uri_pool: DashMap<UriId, String>,
    // Map of definitions
    definitions: DashMap<DefinitionId, Definition>,

    // *** Graph edges: the following represent relationships between nodes ***
    // Map of a fully qualified name to all definitions we discovered for it
    name_to_definitions: DashMap<NameId, DashSet<DefinitionId>>,
    // Reverse map of a definition to its unique fully qualified name
    definition_to_name: DashMap<DefinitionId, NameId>,
    // Map of URI to all definitions discovered in that document
    uris_to_definitions: DashMap<UriId, DashSet<DefinitionId>>,
}

impl Graph {
    #[must_use]
    pub fn new() -> Self {
        Self {
            names: DashMap::new(),
            definitions: DashMap::new(),
            uri_pool: DashMap::new(),
            name_to_definitions: DashMap::new(),
            definition_to_name: DashMap::new(),
            uris_to_definitions: DashMap::new(),
        }
    }

    #[must_use]
    pub fn get(&self, name: &str) -> Option<Vec<Ref<'_, DefinitionId, Definition>>> {
        let name_id = NameId::new(name);
        let owned_definitions = self.name_to_definitions.get(&name_id)?;

        Some(
            owned_definitions
                .iter()
                .filter_map(|id| self.definitions.get(&id))
                .collect(),
        )
    }

    // Registers a URI into the graph and returns the generated ID. This happens once when starting to index the URI and
    // then all definitions discovered in it get associated to the ID
    #[must_use]
    pub fn add_uri(&self, uri: String) -> UriId {
        let uri_id = UriId::new(&uri);
        self.uri_pool.insert(uri_id, uri);
        uri_id
    }

    // Handles when a document (identified by `uri`) is deleted. This removes the URI from the graph along with all
    // relationships and definitions that had been discovered in it
    pub fn delete_uri(&self, uri: &str) {
        let uri_id = UriId::new(uri);
        self.remove_definitions_for_uri(uri_id);
        self.uri_pool.remove(&uri_id);
    }

    // Registers a definition into the `Graph`, automatically creating all relationships
    pub fn add_definition(&self, uri_id: UriId, name: String, definition: Definition) {
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

    // Removes all nodes and relationships associated to the given URI. This is used to clean up stale data when a
    // document (identified by `uri_id`) changes
    pub fn remove_definitions_for_uri(&self, uri_id: UriId) {
        if let Some((_, definitions)) = self.uris_to_definitions.remove(&uri_id) {
            for def_id in definitions {
                if let Some((_, name_id)) = self.definition_to_name.remove(&def_id) {
                    // We need to scope the borrow of `name_to_definitions` to avoid a dead lock when trying to remove
                    // the name below
                    let should_remove_name = {
                        if let Some(definitions) = self.name_to_definitions.get_mut(&name_id) {
                            definitions.remove(&def_id);
                            definitions.is_empty()
                        } else {
                            false
                        }
                    };

                    if should_remove_name {
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
    use crate::test_utils::GraphTest;

    #[test]
    fn deleting_a_uri() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        context.delete_uri("file:///foo.rb");

        assert!(context.graph.definitions.is_empty());
        assert!(context.graph.names.is_empty());
        assert!(context.graph.definition_to_name.is_empty());
        assert!(context.graph.name_to_definitions.is_empty());
        assert!(context.graph.uris_to_definitions.is_empty());
        assert!(context.graph.uri_pool.is_empty());
    }

    #[test]
    fn updating_index_with_deleted_definitions() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        // Update with empty content to remove definitions but keep the URI
        context.index_uri("file:///foo.rb", "");

        assert!(context.graph.definitions.is_empty());
        assert!(context.graph.names.is_empty());
        assert!(context.graph.definition_to_name.is_empty());
        assert!(context.graph.name_to_definitions.is_empty());
        assert!(context.graph.uris_to_definitions.is_empty());
        // URI remains if the file was not deleted, but definitions got erased
        assert_eq!(context.graph.uri_pool.len(), 1);
    }

    #[test]
    fn updating_index_with_new_definitions() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");

        assert_eq!(context.graph.definitions.len(), 1);
        assert_eq!(context.graph.names.get(&NameId::new("Foo")).unwrap().value(), "Foo");
        assert_eq!(
            context
                .graph
                .uri_pool
                .get(&UriId::new("file:///foo.rb"))
                .unwrap()
                .value(),
            "file:///foo.rb"
        );
        assert_eq!(
            context
                .graph
                .name_to_definitions
                .get(&NameId::new("Foo"))
                .unwrap()
                .len(),
            1
        );
        assert_eq!(
            context
                .graph
                .uris_to_definitions
                .get(&UriId::new("file:///foo.rb"))
                .unwrap()
                .len(),
            1
        );
    }

    #[test]
    fn updating_existing_definitions() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        // Update with the same definition but at a different position (with content before it)
        context.index_uri("file:///foo.rb", "\n\n\n\n\n\nmodule Foo; end");

        assert_eq!(context.graph.definitions.len(), 1);
        assert_eq!(context.graph.names.get(&NameId::new("Foo")).unwrap().value(), "Foo");
        assert_eq!(
            context
                .graph
                .uri_pool
                .get(&UriId::new("file:///foo.rb"))
                .unwrap()
                .value(),
            "file:///foo.rb"
        );

        let definitions = context.graph.get("Foo").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].start_offset(), 6);
    }

    #[test]
    fn adding_another_definition_from_a_different_uri() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        context.index_uri("file:///foo2.rb", "\n\n\n\n\nmodule Foo; end");

        let definitions = context.graph.get("Foo").unwrap();
        let mut offsets = definitions.iter().map(|d| d.start_offset()).collect::<Vec<_>>();
        offsets.sort_unstable();
        assert_eq!(definitions.len(), 2);
        assert_eq!(vec![0, 5], offsets);
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

        let mut offsets = definitions
            .iter()
            .map(|d| [d.start_offset(), d.end_offset()])
            .collect::<Vec<_>>();
        offsets.sort_unstable();
        assert_eq!([0, 15], offsets[0]);
        assert_eq!([18, 33], offsets[1]);
    }
}
