use std::collections::hash_map::Entry;

use crate::diagnostic::{Diagnostic, Rule};
use crate::model::definitions::Definition;
use crate::model::document::Document;
use crate::model::graph::NameDependent;
use crate::model::identity_maps::IdentityHashMap;
use crate::model::ids::{DefinitionId, NameId, ReferenceId, StringId, UriId};
use crate::model::name::{Name, NameRef};
use crate::model::references::{ConstantReference, MethodRef};
use crate::model::string_ref::StringRef;
use crate::offset::Offset;

type LocalGraphParts = (
    UriId,
    Document,
    IdentityHashMap<DefinitionId, Definition>,
    IdentityHashMap<StringId, StringRef>,
    IdentityHashMap<NameId, NameRef>,
    IdentityHashMap<ReferenceId, ConstantReference>,
    IdentityHashMap<ReferenceId, MethodRef>,
    IdentityHashMap<NameId, Vec<NameDependent>>,
);

#[derive(Debug)]
pub struct LocalGraph {
    uri_id: UriId,
    document: Document,
    definitions: IdentityHashMap<DefinitionId, Definition>,
    strings: IdentityHashMap<StringId, StringRef>,
    names: IdentityHashMap<NameId, NameRef>,
    constant_references: IdentityHashMap<ReferenceId, ConstantReference>,
    method_references: IdentityHashMap<ReferenceId, MethodRef>,
    name_dependents: IdentityHashMap<NameId, Vec<NameDependent>>,
}

impl LocalGraph {
    #[must_use]
    pub fn new(uri_id: UriId, document: Document) -> Self {
        Self {
            uri_id,
            document,
            definitions: IdentityHashMap::default(),
            strings: IdentityHashMap::default(),
            names: IdentityHashMap::default(),
            constant_references: IdentityHashMap::default(),
            method_references: IdentityHashMap::default(),
            name_dependents: IdentityHashMap::default(),
        }
    }

    #[must_use]
    pub fn uri_id(&self) -> UriId {
        self.uri_id
    }

    #[must_use]
    pub fn document(&self) -> &Document {
        &self.document
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

        if let Some(name_id) = definition.name_id() {
            self.name_dependents
                .entry(*name_id)
                .or_default()
                .push(NameDependent::Definition(definition_id));
        }

        if self.definitions.insert(definition_id, definition).is_some() {
            debug_assert!(false, "DefinitionId collision in local graph");
        }

        self.document.add_definition(definition_id);
        definition_id
    }

    // Strings

    #[must_use]
    pub fn strings(&self) -> &IdentityHashMap<StringId, StringRef> {
        &self.strings
    }

    pub fn intern_string(&mut self, string: String) -> StringId {
        let string_id = StringId::from(&string);

        match self.strings.entry(string_id) {
            Entry::Occupied(mut entry) => {
                debug_assert!(string == **entry.get(), "StringId collision in local graph");
                entry.get_mut().increment_ref_count(1);
            }
            Entry::Vacant(entry) => {
                entry.insert(StringRef::new(string));
            }
        }

        string_id
    }

    // Names

    #[must_use]
    pub fn names(&self) -> &IdentityHashMap<NameId, NameRef> {
        &self.names
    }

    pub fn add_name(&mut self, name: Name) -> NameId {
        let name_id = name.id();
        let nesting_id = name.nesting().as_ref().copied();
        let parent_scope_id = name.parent_scope().as_ref().copied();

        match self.names.entry(name_id) {
            Entry::Occupied(mut entry) => {
                debug_assert!(*entry.get() == name, "NameId collision in local graph");
                entry.get_mut().increment_ref_count(1);
            }
            Entry::Vacant(entry) => {
                entry.insert(NameRef::Unresolved(Box::new(name)));

                // Track nesting and parent_scope dependents for new names
                let dep = NameDependent::Name(name_id);
                for dep_owner_id in [nesting_id, parent_scope_id].into_iter().flatten() {
                    let deps = self.name_dependents.entry(dep_owner_id).or_default();
                    if !deps.contains(&dep) {
                        deps.push(dep);
                    }
                }
            }
        }

        name_id
    }

    // Constant references

    #[must_use]
    pub fn constant_references(&self) -> &IdentityHashMap<ReferenceId, ConstantReference> {
        &self.constant_references
    }

    pub fn add_constant_reference(&mut self, reference: ConstantReference) -> ReferenceId {
        let reference_id = reference.id();

        self.name_dependents
            .entry(*reference.name_id())
            .or_default()
            .push(NameDependent::Reference(reference_id));

        if self.constant_references.insert(reference_id, reference).is_some() {
            debug_assert!(false, "ReferenceId collision in local graph");
        }

        self.document.add_constant_reference(reference_id);
        reference_id
    }

    // Method references

    #[must_use]
    pub fn method_references(&self) -> &IdentityHashMap<ReferenceId, MethodRef> {
        &self.method_references
    }

    pub fn add_method_reference(&mut self, reference: MethodRef) -> ReferenceId {
        let reference_id = reference.id();

        if self.method_references.insert(reference_id, reference).is_some() {
            debug_assert!(false, "ReferenceId collision in local graph");
        }

        self.document.add_method_reference(reference_id);
        reference_id
    }

    // Diagnostics

    #[must_use]
    pub fn diagnostics(&self) -> &[Diagnostic] {
        self.document.diagnostics()
    }

    pub fn add_diagnostic(&mut self, rule: Rule, offset: Offset, message: String) {
        let diagnostic = Diagnostic::new(rule, self.uri_id, offset, message);
        self.document.add_diagnostic(diagnostic);
    }

    // Into parts

    #[must_use]
    pub fn into_parts(self) -> LocalGraphParts {
        (
            self.uri_id,
            self.document,
            self.definitions,
            self.strings,
            self.names,
            self.constant_references,
            self.method_references,
            self.name_dependents,
        )
    }
}
