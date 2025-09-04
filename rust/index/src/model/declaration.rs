use std::collections::HashSet;

use crate::model::{
    identity_maps::{IdentityHashBuilder, IdentityHashSet},
    ids::DefinitionId,
};

#[derive(Debug)]
pub struct Declaration {
    name: String,
    definition_ids: IdentityHashSet<DefinitionId>,
}

impl Declaration {
    #[must_use]
    pub fn new(name: String) -> Self {
        Self {
            name,
            definition_ids: HashSet::with_hasher(IdentityHashBuilder),
        }
    }

    // Extend this declaration with more definitions by moving `other.definition_ids` inside
    pub fn extend(&mut self, other: Declaration) {
        self.definition_ids.extend(other.definition_ids);
    }

    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[must_use]
    pub fn definitions(&self) -> &IdentityHashSet<DefinitionId> {
        &self.definition_ids
    }

    // Deletes a definition from this declaration. Returns `true` if this declaration is now empty, which indicates that
    // it must be removed from the graph
    pub fn remove_definition(&mut self, definition_id: &DefinitionId) -> bool {
        self.definition_ids.remove(definition_id);
        self.definition_ids.is_empty()
    }

    pub fn add_definition(&mut self, definition_id: DefinitionId) {
        self.definition_ids.insert(definition_id);
    }
}
