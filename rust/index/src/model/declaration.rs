use crate::model::ids::DefinitionId;

#[derive(Debug)]
pub struct Declaration {
    name: String,
    definition_ids: Vec<DefinitionId>,
}

impl Declaration {
    #[must_use]
    pub fn new(name: String) -> Self {
        Self {
            name,
            definition_ids: Vec::new(),
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
    pub fn definitions(&self) -> &[DefinitionId] {
        &self.definition_ids
    }

    // Deletes a definition from this declaration. Returns `true` if this declaration is now empty, which indicates that
    // it must be removed from the graph
    pub fn remove_definition(&mut self, definition_id: &DefinitionId) -> bool {
        if let Some(pos) = self.definition_ids.iter().position(|id| id == definition_id) {
            self.definition_ids.swap_remove(pos);
            self.definition_ids.shrink_to_fit();
        }

        self.definition_ids.is_empty()
    }

    pub fn add_definition(&mut self, definition_id: DefinitionId) {
        self.definition_ids.push(definition_id);
    }
}
