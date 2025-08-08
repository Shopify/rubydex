use std::collections::HashMap;
use std::collections::hash_map::Entry;

use crate::indexing::indexed_data::IndexingThreadData;
use crate::model::declaration::Declaration;
use crate::model::definitions::Definition;
use crate::model::ids::{DeclarationId, UriId};

// The `Index` is the global graph representation of the entire Ruby codebase. It contains all declarations and their
// relationships
pub struct Index {
    declarations: HashMap<DeclarationId, Declaration>,
    uri_pool: HashMap<UriId, String>,
}

impl Index {
    #[must_use]
    pub fn new() -> Self {
        Self {
            declarations: HashMap::new(),
            uri_pool: HashMap::new(),
        }
    }

    fn add_declaration(&mut self, id: DeclarationId, name: String, definitions: Vec<Definition>) {
        match self.declarations.entry(id) {
            Entry::Occupied(existing_entry) => {
                let existing_declaration = existing_entry.get();

                // Check for hash collision: same hash but different name
                assert_eq!(
                    existing_declaration.name, name,
                    "Hash collision detected! DeclarationId {:?} maps to both '{}' and '{}'",
                    id, existing_declaration.name, name
                );

                // Same name, so this is a legitimate duplicate declaration
                // todo synchronize modifications to the existing declarations
            }
            Entry::Vacant(vac) => {
                vac.insert(Declaration::new(name, Some(definitions)));
            }
        }
    }

    pub fn merge_definitions(&mut self, indexed_data: IndexingThreadData) {
        let (declarations, uris) = indexed_data.into_parts();

        for (name, declarations) in declarations {
            let (id, definitions) = declarations.into();
            self.add_declaration(id, name, definitions);
        }

        self.uri_pool.extend(uris);
    }
}

impl Default for Index {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic(expected = "Hash collision detected!")]
    fn test_hash_collision_detection() {
        let mut index = Index::new();

        // Create the same DeclarationId for two different names to simulate a hash collision scenario
        let collision_id = DeclarationId::new("SameName");

        index.add_declaration(collision_id, "SameName".to_string(), vec![]);
        index.add_declaration(collision_id, "DifferentName".to_string(), vec![]);
    }

    #[test]
    fn test_same_name_duplicate_declaration() {
        let mut index = Index::new();

        let id = DeclarationId::new("TestName");

        index.add_declaration(id, "TestName".to_string(), vec![]);
        index.add_declaration(id, "TestName".to_string(), vec![]);

        assert!(index.declarations.contains_key(&id));
    }
}
