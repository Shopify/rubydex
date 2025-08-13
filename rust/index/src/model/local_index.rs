use std::collections::{HashMap, HashSet};

use crate::model::definitions::Definition;
use crate::model::ids::{DefinitionId, NameId, UriId};

/// A lightweight index for collecting definitions during single-file indexing.
/// Used by RubyIndexer to accumulate definitions before merging into the global Index.
/// Contains the same hashmaps as the global Index for consistent structure.
#[derive(Debug)]
pub struct LocalIndex {
    pub uri_id: UriId,
    // Same hashmaps as global Index
    pub names: HashMap<NameId, String>,
    pub definitions: HashMap<DefinitionId, Definition>,
    pub name_to_definitions: HashMap<NameId, HashSet<DefinitionId>>,
    pub definition_to_name: HashMap<DefinitionId, NameId>,
    pub uris_to_definitions: HashMap<UriId, HashSet<DefinitionId>>,
}

impl LocalIndex {
    #[must_use]
    pub fn new(uri_id: UriId) -> Self {
        Self {
            uri_id,
            names: HashMap::new(),
            definitions: HashMap::new(),
            name_to_definitions: HashMap::new(),
            definition_to_name: HashMap::new(),
            uris_to_definitions: HashMap::new(),
        }
    }

    pub fn add_definition(&mut self, name: String, definition: Definition) {
        let name_id = NameId::new(&name);
        let definition_id = DefinitionId::new(self.uri_id, definition.start_offset());

        self.names.insert(name_id, name);
        self.definitions.insert(definition_id, definition);
        self.name_to_definitions
            .entry(name_id)
            .or_default()
            .insert(definition_id);
        self.definition_to_name.insert(definition_id, name_id);
        self.uris_to_definitions
            .entry(self.uri_id)
            .or_default()
            .insert(definition_id);
    }
}
