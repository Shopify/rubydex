use crate::model::ids::DefinitionId;
use serde::{Deserialize, Serialize};

// Represents a document currently loaded into memory. Identified by its unique URI, it holds the edges to all
// definitions discovered in it
#[derive(Debug, Serialize, Deserialize)]
pub struct Document {
    uri: String,
    definition_ids: Vec<DefinitionId>,
    content_hash: u16,
}

impl Document {
    #[must_use]
    pub fn new(uri: String, content_hash: u16) -> Self {
        Self {
            uri,
            content_hash,
            definition_ids: Vec::new(),
        }
    }

    #[must_use]
    pub fn uri(&self) -> &str {
        &self.uri
    }

    #[must_use]
    pub fn content_hash(&self) -> u16 {
        self.content_hash
    }

    #[must_use]
    pub fn definitions(&self) -> &[DefinitionId] {
        &self.definition_ids
    }

    pub fn add_definition(&mut self, definition_id: DefinitionId) {
        debug_assert!(
            !self.definition_ids.contains(&definition_id),
            "Cannot add the same exact definition to a document twice. Duplicate definition IDs"
        );

        self.definition_ids.push(definition_id);
    }

    /// Serializes the document into the byte vector we store in the database
    ///
    /// # Panics
    ///
    /// This method will only panic if serialization fails, which should never happen
    #[must_use]
    pub fn serialize(&self) -> Vec<u8> {
        rmp_serde::to_vec(self).expect("Serializing document should always succeed")
    }

    /// # Panics
    ///
    /// This method will only panic if serialization fails, which should never happen unless there's corrupt data stored
    /// in the database
    #[must_use]
    pub fn deserialize(data: &[u8]) -> Self {
        rmp_serde::from_slice(data).expect("Deserializing document should always succeed")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic(expected = "Cannot add the same exact definition to a document twice. Duplicate definition IDs")]
    fn inserting_duplicate_definitions() {
        let mut document = Document::new("file:///foo.rb".to_string(), 123u16);
        let def_id = DefinitionId::new(123);

        document.add_definition(def_id);
        document.add_definition(def_id);

        assert_eq!(document.definitions().len(), 1);
    }
}
