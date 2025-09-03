use crate::model::ids::DefinitionId;

// Represents a document currently loaded into memory. Identified by its unique URI, it holds the edges to all
// definitions discovered in it
#[derive(Debug)]
pub struct Document {
    uri: String,
    definition_ids: Vec<DefinitionId>,
}

impl Document {
    #[must_use]
    pub fn new(uri: String) -> Self {
        Self {
            uri,
            definition_ids: Vec::new(),
        }
    }

    #[must_use]
    pub fn uri(&self) -> &str {
        &self.uri
    }

    #[must_use]
    pub fn definitions(&self) -> &[DefinitionId] {
        &self.definition_ids
    }

    pub fn add_definition(&mut self, definition_id: DefinitionId) {
        self.definition_ids.push(definition_id);
    }
}
