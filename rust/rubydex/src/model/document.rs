use line_index::LineIndex;

use crate::model::ids::{DefinitionId, ReferenceId};

// Represents a document currently loaded into memory. Identified by its unique URI, it holds the edges to all
// definitions and references discovered in it
#[derive(Debug)]
pub struct Document {
    uri: String,
    line_index: LineIndex,
    definition_ids: Vec<DefinitionId>,
    method_reference_ids: Vec<ReferenceId>,
    constant_reference_ids: Vec<ReferenceId>,
}

impl Document {
    #[must_use]
    pub fn new(uri: String, source: &str) -> Self {
        Self {
            uri,
            line_index: LineIndex::new(source),
            definition_ids: Vec::new(),
            method_reference_ids: Vec::new(),
            constant_reference_ids: Vec::new(),
        }
    }

    #[must_use]
    pub fn uri(&self) -> &str {
        &self.uri
    }

    #[must_use]
    pub fn line_index(&self) -> &LineIndex {
        &self.line_index
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

    #[must_use]
    pub fn method_references(&self) -> &[ReferenceId] {
        &self.method_reference_ids
    }

    pub fn add_method_reference(&mut self, reference_id: ReferenceId) {
        self.method_reference_ids.push(reference_id);
    }

    #[must_use]
    pub fn constant_references(&self) -> &[ReferenceId] {
        &self.constant_reference_ids
    }

    pub fn add_constant_reference(&mut self, reference_id: ReferenceId) {
        self.constant_reference_ids.push(reference_id);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic(expected = "Cannot add the same exact definition to a document twice. Duplicate definition IDs")]
    fn inserting_duplicate_definitions() {
        let mut document = Document::new("file:///foo.rb".to_string(), "class Foo; end");
        let def_id = DefinitionId::new(123);

        document.add_definition(def_id);
        document.add_definition(def_id);

        assert_eq!(document.definitions().len(), 1);
    }

    #[test]
    fn tracking_references() {
        let mut document = Document::new("file:///foo.rb".to_string(), "class Foo; end");
        let method_ref = ReferenceId::new(1);
        let constant_ref = ReferenceId::new(2);

        document.add_method_reference(method_ref);
        document.add_constant_reference(constant_ref);

        assert_eq!(document.method_references(), &[method_ref]);
        assert_eq!(document.constant_references(), &[constant_ref]);
    }
}
