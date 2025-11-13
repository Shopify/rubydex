use crate::model::definitions::Definition;
use crate::model::document::Document;
use crate::model::identity_maps::IdentityHashMap;
use crate::model::ids::{DefinitionId, NameId, ReferenceId, UriId};
use crate::model::references::{ConstantReference, MethodRef};

type LocalGraphParts = (
    UriId,
    Document,
    IdentityHashMap<DefinitionId, Definition>,
    IdentityHashMap<NameId, String>,
    IdentityHashMap<ReferenceId, ConstantReference>,
    IdentityHashMap<ReferenceId, MethodRef>,
);

#[derive(Debug)]
pub struct LocalGraph {
    uri_id: UriId,
    document: Document,
    definitions: IdentityHashMap<DefinitionId, Definition>,
    names: IdentityHashMap<NameId, String>,
    constant_references: IdentityHashMap<ReferenceId, ConstantReference>,
    method_references: IdentityHashMap<ReferenceId, MethodRef>,
}

impl LocalGraph {
    #[must_use]
    pub fn new(uri_id: UriId, document: Document) -> Self {
        Self {
            uri_id,
            document,
            definitions: IdentityHashMap::default(),
            names: IdentityHashMap::default(),
            constant_references: IdentityHashMap::default(),
            method_references: IdentityHashMap::default(),
        }
    }

    #[must_use]
    pub fn uri_id(&self) -> UriId {
        self.uri_id
    }

    // Definitions

    #[must_use]
    pub fn definitions(&self) -> &IdentityHashMap<DefinitionId, Definition> {
        &self.definitions
    }

    #[must_use]
    pub fn get_definition_mut(&mut self, definition_id: DefinitionId) -> Option<&mut Definition> {
        self.definitions.get_mut(&definition_id)
    }

    pub fn add_definition(&mut self, definition: Definition) -> DefinitionId {
        let definition_id = definition.id();
        self.definitions.insert(definition_id, definition);
        self.document.add_definition(definition_id);

        definition_id
    }

    // Names

    #[must_use]
    pub fn names(&self) -> &IdentityHashMap<NameId, String> {
        &self.names
    }

    pub fn add_name(&mut self, name: String) -> NameId {
        let name_id = NameId::from(&name);
        self.names.insert(name_id, name);
        name_id
    }

    // Constant references

    #[must_use]
    pub fn constant_references(&self) -> &IdentityHashMap<ReferenceId, ConstantReference> {
        &self.constant_references
    }

    pub fn add_constant_reference(&mut self, reference: ConstantReference) -> ReferenceId {
        let reference_id = reference.id();
        self.constant_references.insert(reference_id, reference);
        reference_id
    }

    // Method references

    #[must_use]
    pub fn method_references(&self) -> &IdentityHashMap<ReferenceId, MethodRef> {
        &self.method_references
    }

    pub fn add_method_reference(&mut self, reference: MethodRef) -> ReferenceId {
        let reference_id = reference.id();
        self.method_references.insert(reference_id, reference);
        reference_id
    }

    // Into parts

    #[must_use]
    pub fn into_parts(self) -> LocalGraphParts {
        (
            self.uri_id,
            self.document,
            self.definitions,
            self.names,
            self.constant_references,
            self.method_references,
        )
    }
}
