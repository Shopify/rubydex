use std::collections::hash_map::Entry;
use std::sync::LazyLock;

use crate::diagnostic::Diagnostic;
use crate::indexing::local_graph::LocalGraph;
use crate::model::declaration::{Ancestor, Declaration, Namespace};
use crate::model::definitions::{Definition, Mixin};
use crate::model::document::Document;
use crate::model::encoding::Encoding;
use crate::model::identity_maps::{IdentityHashMap, IdentityHashSet};
use crate::model::ids::{DeclarationId, DefinitionId, NameId, ReferenceId, StringId, UriId};
use crate::model::name::{Name, NameRef, ParentScope, ResolvedName};
use crate::model::references::{ConstantReference, MethodRef};
use crate::model::string_ref::StringRef;
use crate::stats;

pub static OBJECT_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("Object"));
pub static MODULE_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("Module"));
pub static CLASS_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("Class"));

/// A definition or reference that depends on a particular Name for resolution.
/// Registered along the nesting chain so we can follow dependencies when invalidating.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NameDependent {
    Definition(DefinitionId),
    Reference(ReferenceId),
}

// The `Graph` is the global representation of the entire Ruby codebase. It contains all declarations and their
// relationships
#[derive(Default, Debug)]
pub struct Graph {
    // Map of declaration nodes
    declarations: IdentityHashMap<DeclarationId, Declaration>,
    // Map of document nodes
    documents: IdentityHashMap<UriId, Document>,
    // Map of definition nodes
    definitions: IdentityHashMap<DefinitionId, Definition>,

    // Map of unqualified names
    strings: IdentityHashMap<StringId, StringRef>,
    // Map of names
    names: IdentityHashMap<NameId, NameRef>,
    // Map of constant references
    constant_references: IdentityHashMap<ReferenceId, ConstantReference>,
    // Map of method references that still need to be resolved
    method_references: IdentityHashMap<ReferenceId, MethodRef>,

    /// Dependency tracking: maps each `NameId` to the definitions and references whose
    /// resolution depends on it. Registered along the nesting chain so we can follow
    /// dependencies when invalidating.
    name_dependents: IdentityHashMap<NameId, Vec<NameDependent>>,

    /// The position encoding used for LSP line/column locations. Not related to the actual encoding of the file
    position_encoding: Encoding,
}

impl Graph {
    #[must_use]
    pub fn new() -> Self {
        Self {
            declarations: IdentityHashMap::default(),
            definitions: IdentityHashMap::default(),
            documents: IdentityHashMap::default(),
            strings: IdentityHashMap::default(),
            names: IdentityHashMap::default(),
            constant_references: IdentityHashMap::default(),
            method_references: IdentityHashMap::default(),
            name_dependents: IdentityHashMap::default(),
            position_encoding: Encoding::default(),
        }
    }

    // Returns an immutable reference to the declarations map
    #[must_use]
    pub fn declarations(&self) -> &IdentityHashMap<DeclarationId, Declaration> {
        &self.declarations
    }

    /// Returns a mutable reference to the declarations map
    #[must_use]
    pub fn declarations_mut(&mut self) -> &mut IdentityHashMap<DeclarationId, Declaration> {
        &mut self.declarations
    }

    pub fn add_declaration<F>(
        &mut self,
        definition_id: DefinitionId,
        fully_qualified_name: String,
        constructor: F,
    ) -> DeclarationId
    where
        F: FnOnce(String) -> Declaration,
    {
        let declaration_id = DeclarationId::from(&fully_qualified_name);

        match self.declarations.entry(declaration_id) {
            Entry::Vacant(vacant_entry) => {
                vacant_entry
                    .insert(constructor(fully_qualified_name))
                    .add_definition(definition_id);
            }
            Entry::Occupied(mut occupied_entry) => {
                debug_assert!(
                    occupied_entry.get().name() == fully_qualified_name,
                    "DeclarationId collision in global graph"
                );
                occupied_entry.get_mut().add_definition(definition_id);
            }
        }

        declaration_id
    }

    pub fn clear_declarations(&mut self) {
        self.declarations.clear();
    }

    // Returns an immutable reference to the definitions map
    #[must_use]
    pub fn definitions(&self) -> &IdentityHashMap<DefinitionId, Definition> {
        &self.definitions
    }

    /// Returns the ID of the unqualified name of a definition
    ///
    /// # Panics
    ///
    /// This will panic if there's inconsistent data in the graph
    #[must_use]
    pub fn definition_string_id(&self, definition: &Definition) -> StringId {
        let id = match definition {
            Definition::Class(it) => {
                let name = self.names.get(it.name_id()).unwrap();
                name.str()
            }
            Definition::SingletonClass(it) => {
                let name = self.names.get(it.name_id()).unwrap();
                name.str()
            }
            Definition::Module(it) => {
                let name = self.names.get(it.name_id()).unwrap();
                name.str()
            }
            Definition::Constant(it) => {
                let name = self.names.get(it.name_id()).unwrap();
                name.str()
            }
            Definition::ConstantAlias(it) => {
                let name = self.names.get(it.name_id()).unwrap();
                name.str()
            }
            Definition::GlobalVariable(it) => it.str_id(),
            Definition::InstanceVariable(it) => it.str_id(),
            Definition::ClassVariable(it) => it.str_id(),
            Definition::AttrAccessor(it) => it.str_id(),
            Definition::AttrReader(it) => it.str_id(),
            Definition::AttrWriter(it) => it.str_id(),
            Definition::Method(it) => it.str_id(),
            Definition::MethodAlias(it) => it.new_name_str_id(),
            Definition::GlobalVariableAlias(it) => it.new_name_str_id(),
        };

        *id
    }

    // Returns an immutable reference to the strings map
    #[must_use]
    pub fn strings(&self) -> &IdentityHashMap<StringId, StringRef> {
        &self.strings
    }

    // Returns an immutable reference to the URI pool map
    #[must_use]
    pub fn documents(&self) -> &IdentityHashMap<UriId, Document> {
        &self.documents
    }

    /// # Panics
    ///
    /// Panics if the definition is not found
    #[must_use]
    pub fn definition_id_to_declaration_id(&self, definition_id: DefinitionId) -> Option<&DeclarationId> {
        self.definition_to_declaration_id(self.definitions.get(&definition_id).unwrap())
    }

    /// # Panics
    ///
    /// Panics if the definition is not found
    #[must_use]
    pub fn definition_to_declaration_id(&self, definition: &Definition) -> Option<&DeclarationId> {
        let (nesting_name_id, member_str_id) = match definition {
            Definition::Class(it) => {
                return self.name_id_to_declaration_id(*it.name_id());
            }
            Definition::SingletonClass(it) => {
                return self.name_id_to_declaration_id(*it.name_id());
            }
            Definition::Module(it) => {
                return self.name_id_to_declaration_id(*it.name_id());
            }
            Definition::Constant(it) => {
                return self.name_id_to_declaration_id(*it.name_id());
            }
            Definition::ConstantAlias(it) => {
                return self.name_id_to_declaration_id(*it.name_id());
            }
            Definition::GlobalVariable(it) => (
                self.find_enclosing_namespace_name_id(it.lexical_nesting_id().as_ref()),
                it.str_id(),
            ),
            Definition::GlobalVariableAlias(it) => (
                self.find_enclosing_namespace_name_id(it.lexical_nesting_id().as_ref()),
                it.new_name_str_id(),
            ),
            Definition::InstanceVariable(it) => (
                self.find_enclosing_namespace_name_id(it.lexical_nesting_id().as_ref()),
                it.str_id(),
            ),
            Definition::ClassVariable(it) => (
                self.find_enclosing_namespace_name_id(it.lexical_nesting_id().as_ref()),
                it.str_id(),
            ),
            Definition::AttrAccessor(it) => (
                self.find_enclosing_namespace_name_id(it.lexical_nesting_id().as_ref()),
                it.str_id(),
            ),
            Definition::AttrReader(it) => (
                self.find_enclosing_namespace_name_id(it.lexical_nesting_id().as_ref()),
                it.str_id(),
            ),
            Definition::AttrWriter(it) => (
                self.find_enclosing_namespace_name_id(it.lexical_nesting_id().as_ref()),
                it.str_id(),
            ),
            Definition::Method(it) => (
                self.find_enclosing_namespace_name_id(it.lexical_nesting_id().as_ref()),
                it.str_id(),
            ),
            Definition::MethodAlias(it) => (
                self.find_enclosing_namespace_name_id(it.lexical_nesting_id().as_ref()),
                it.new_name_str_id(),
            ),
        };

        let nesting_declaration_id = match nesting_name_id {
            Some(name_id) => self.name_id_to_declaration_id(*name_id),
            None => Some(&*OBJECT_ID),
        }?;

        self.declarations
            .get(nesting_declaration_id)?
            .as_namespace()?
            .member(member_str_id)
    }

    /// Finds the closest namespace name ID to connect a definition to its declaration
    fn find_enclosing_namespace_name_id(&self, starting_id: Option<&DefinitionId>) -> Option<&NameId> {
        let mut current = starting_id;

        while let Some(id) = current {
            let def = self.definitions.get(id).unwrap();

            if let Some(name_id) = def.name_id() {
                return Some(name_id);
            }

            current = def.lexical_nesting_id().as_ref();
        }

        None
    }

    #[must_use]
    pub fn name_id_to_declaration_id(&self, name_id: NameId) -> Option<&DeclarationId> {
        let name = self.names.get(&name_id);

        match name {
            Some(NameRef::Resolved(resolved)) => Some(resolved.declaration_id()),
            Some(NameRef::Unresolved(_)) | None => None,
        }
    }

    // Returns an immutable reference to the constant references map
    #[must_use]
    pub fn constant_references(&self) -> &IdentityHashMap<ReferenceId, ConstantReference> {
        &self.constant_references
    }

    // Returns an immutable reference to the method references map
    #[must_use]
    pub fn method_references(&self) -> &IdentityHashMap<ReferenceId, MethodRef> {
        &self.method_references
    }

    #[must_use]
    pub fn all_diagnostics(&self) -> Vec<&Diagnostic> {
        let document_diagnostics = self.documents.values().flat_map(Document::all_diagnostics);
        let declaration_diagnostics = self.declarations.values().flat_map(Declaration::diagnostics);

        document_diagnostics.chain(declaration_diagnostics).collect()
    }

    /// Interns a string in the graph unless already interned. This method is only used to back the
    /// `Graph#resolve_constant` Ruby API because every string must be interned in the graph to properly resolve.
    pub fn intern_string(&mut self, string: String) -> StringId {
        let string_id = StringId::from(&string);
        match self.strings.entry(string_id) {
            Entry::Occupied(mut entry) => {
                entry.get_mut().increment_ref_count(1);
            }
            Entry::Vacant(entry) => {
                entry.insert(StringRef::new(string));
            }
        }
        string_id
    }

    /// Registers a name in the graph unless already registered. In regular indexing, this only happens in the local
    /// graph. This method is only used to back the `Graph#resolve_constant` Ruby API because every name must be
    /// registered in the graph to properly resolve
    pub fn add_name(&mut self, name: Name) -> NameId {
        let name_id = name.id();

        match self.names.entry(name_id) {
            Entry::Occupied(mut entry) => {
                entry.get_mut().increment_ref_count(1);
            }
            Entry::Vacant(entry) => {
                entry.insert(NameRef::Unresolved(Box::new(name)));
            }
        }

        name_id
    }

    #[must_use]
    pub fn get(&self, name: &str) -> Option<Vec<&Definition>> {
        let declaration_id = DeclarationId::from(name);
        let declaration = self.declarations.get(&declaration_id)?;

        Some(
            declaration
                .definitions()
                .iter()
                .filter_map(|id| self.definitions.get(id))
                .collect(),
        )
    }

    /// Returns all target declaration IDs for a constant alias.
    ///
    /// A constant alias can have multiple definitions (e.g., conditional assignment in different files),
    /// each potentially pointing to a different target. This method collects all resolved targets.
    ///
    /// Returns `None` if the declaration doesn't exist or is not a constant alias.
    /// Returns `Some(vec![])` if no targets have been resolved yet.
    #[must_use]
    pub fn alias_targets(&self, declaration_id: &DeclarationId) -> Option<Vec<DeclarationId>> {
        let declaration = self.declarations.get(declaration_id)?;

        let Declaration::ConstantAlias(_) = declaration else {
            return None;
        };

        let mut targets = Vec::new();
        for definition_id in declaration.definitions() {
            let Some(Definition::ConstantAlias(alias_def)) = self.definitions.get(definition_id) else {
                continue;
            };

            let target_name_id = alias_def.target_name_id();
            let Some(name_ref) = self.names.get(target_name_id) else {
                continue;
            };

            if let NameRef::Resolved(resolved) = name_ref {
                let target_id = *resolved.declaration_id();
                if !targets.contains(&target_id) {
                    targets.push(target_id);
                }
            }
        }

        Some(targets)
    }

    #[must_use]
    pub fn names(&self) -> &IdentityHashMap<NameId, NameRef> {
        &self.names
    }

    /// Decrements the ref count for a name and removes it if the count reaches zero.
    ///
    /// This does not recursively untrack `parent_scope` or `nesting` names.
    pub fn untrack_name(&mut self, name_id: NameId) {
        if let Some(name_ref) = self.names.get_mut(&name_id) {
            let string_id = *name_ref.str();
            if !name_ref.decrement_ref_count() {
                self.names.remove(&name_id);
            }
            self.untrack_string(string_id);
        }
    }

    fn untrack_string(&mut self, string_id: StringId) {
        if let Some(string_ref) = self.strings.get_mut(&string_id)
            && !string_ref.decrement_ref_count()
        {
            self.strings.remove(&string_id);
        }
    }

    fn untrack_definition_strings(&mut self, definition: &Definition) {
        match definition {
            Definition::Class(_)
            | Definition::SingletonClass(_)
            | Definition::Module(_)
            | Definition::Constant(_)
            | Definition::ConstantAlias(_) => {}
            Definition::Method(d) => self.untrack_string(*d.str_id()),
            Definition::AttrAccessor(d) => self.untrack_string(*d.str_id()),
            Definition::AttrReader(d) => self.untrack_string(*d.str_id()),
            Definition::AttrWriter(d) => self.untrack_string(*d.str_id()),
            Definition::GlobalVariable(d) => self.untrack_string(*d.str_id()),
            Definition::InstanceVariable(d) => self.untrack_string(*d.str_id()),
            Definition::ClassVariable(d) => self.untrack_string(*d.str_id()),
            Definition::MethodAlias(d) => {
                self.untrack_string(*d.new_name_str_id());
                self.untrack_string(*d.old_name_str_id());
            }
            Definition::GlobalVariableAlias(d) => {
                self.untrack_string(*d.new_name_str_id());
                self.untrack_string(*d.old_name_str_id());
            }
        }
    }

    /// Decrements the ref count for a name and removes it if the count reaches zero.
    ///
    /// This recursively untracks `parent_scope` and `nesting` names.
    pub fn untrack_name_recursive(&mut self, name_id: NameId) {
        let Some(name_ref) = self.names.get(&name_id) else {
            return;
        };

        let parent_scope = name_ref.parent_scope();
        let nesting = *name_ref.nesting();

        if let ParentScope::Some(parent_scope_id) = parent_scope {
            self.untrack_name_recursive(*parent_scope_id);
        }

        if let Some(nesting_id) = nesting {
            self.untrack_name_recursive(nesting_id);
        }

        self.untrack_name(name_id);
    }

    /// Register a member relationship from a declaration to another declaration through its unqualified name id. For example, in
    ///
    /// ```ruby
    /// module Foo
    ///   class Bar; end
    ///   def baz; end
    /// end
    /// ```
    ///
    /// `Foo` has two members:
    /// ```ruby
    /// {
    ///   NameId(Bar) => DeclarationId(Bar)
    ///   NameId(baz) => DeclarationId(baz)
    /// }
    /// ```
    ///
    /// # Panics
    ///
    /// Will panic if the declaration ID passed doesn't belong to a namespace declaration
    pub fn add_member(
        &mut self,
        owner_id: &DeclarationId,
        member_declaration_id: DeclarationId,
        member_str_id: StringId,
    ) {
        if let Some(declaration) = self.declarations.get_mut(owner_id) {
            match declaration {
                Declaration::Namespace(Namespace::Class(it)) => it.add_member(member_str_id, member_declaration_id),
                Declaration::Namespace(Namespace::Module(it)) => it.add_member(member_str_id, member_declaration_id),
                Declaration::Namespace(Namespace::SingletonClass(it)) => {
                    it.add_member(member_str_id, member_declaration_id);
                }
                Declaration::Constant(_) => {
                    // TODO: temporary hack to avoid crashing on `Struct.new`, `Class.new` and `Module.new`
                }
                _ => panic!("Tried to add member to a declaration that isn't a namespace"),
            }
        }
    }

    /// # Panics
    ///
    /// This function will panic when trying to record a resolve name for a name ID that does not exist
    pub fn record_resolved_name(&mut self, name_id: NameId, declaration_id: DeclarationId) {
        match self.names.entry(name_id) {
            Entry::Occupied(entry) => match entry.get() {
                NameRef::Unresolved(_) => {
                    if let NameRef::Unresolved(unresolved) = entry.remove() {
                        let resolved_name = NameRef::Resolved(Box::new(ResolvedName::new(*unresolved, declaration_id)));
                        self.names.insert(name_id, resolved_name);
                    }
                }
                NameRef::Resolved(_) => {
                    // TODO: consider if this is a valid scenario with the resolution phase design. Either collect
                    // metrics here or panic if it's never supposed to occur
                }
            },
            Entry::Vacant(_) => panic!("Trying to record resolved name for a name ID that does not exist"),
        }
    }

    pub fn record_resolved_reference(&mut self, reference_id: ReferenceId, declaration_id: DeclarationId) {
        if let Some(declaration) = self.declarations.get_mut(&declaration_id) {
            declaration.add_reference(reference_id);
        }
    }

    //// Handles the deletion of a document identified by `uri`
    pub fn delete_uri(&mut self, uri: &str) {
        let uri_id = UriId::from(uri);
        // Note: document already removed by remove_definitions_for_uri
        self.remove_definitions_for_uri(uri_id);
    }

    /// Merges everything in `other` into this Graph. This method is meant to merge all graph representations from
    /// different threads, but not meant to handle updates to the existing global representation
    pub fn extend(&mut self, local_graph: LocalGraph) {
        let (uri_id, document, definitions, strings, names, constant_references, method_references) =
            local_graph.into_parts();

        if self.documents.insert(uri_id, document).is_some() {
            debug_assert!(false, "UriId collision in global graph");
        }

        for (string_id, string_ref) in strings {
            match self.strings.entry(string_id) {
                Entry::Occupied(mut entry) => {
                    debug_assert!(*string_ref == **entry.get(), "StringId collision in global graph");
                    entry.get_mut().increment_ref_count(string_ref.ref_count());
                }
                Entry::Vacant(entry) => {
                    entry.insert(string_ref);
                }
            }
        }

        for (name_id, name_ref) in names {
            match self.names.entry(name_id) {
                Entry::Occupied(mut entry) => {
                    debug_assert!(*entry.get() == name_ref, "NameId collision in global graph");
                    entry.get_mut().increment_ref_count(name_ref.ref_count());
                }
                Entry::Vacant(entry) => {
                    entry.insert(name_ref);
                }
            }
        }

        for (definition_id, definition) in definitions {
            if self.definitions.insert(definition_id, definition).is_some() {
                debug_assert!(false, "DefinitionId collision in global graph");
            }
        }

        for (constant_ref_id, constant_ref) in constant_references {
            if self.constant_references.insert(constant_ref_id, constant_ref).is_some() {
                debug_assert!(false, "Constant ReferenceId collision in global graph");
            }
        }

        for (method_ref_id, method_ref) in method_references {
            if self.method_references.insert(method_ref_id, method_ref).is_some() {
                debug_assert!(false, "Method ReferenceId collision in global graph");
            }
        }
    }

    /// Updates the global representation with the information contained in `other`, handling deletions, insertions and
    /// updates to existing entries
    pub fn update(&mut self, other: LocalGraph) {
        let uri_id = other.uri_id();

        // ── Pre-analysis: collect mixin declaration IDs while names are still Resolved ──
        let mut mixin_declaration_ids: Vec<DeclarationId> = Vec::new();
        let mut extend_mixin_declaration_ids: Vec<DeclarationId> = Vec::new();

        for def in other.definitions().values() {
            let (name_id, mixins) = match def {
                Definition::Class(c) if !c.mixins().is_empty() => (c.name_id(), c.mixins()),
                Definition::Module(m) if !m.mixins().is_empty() => (m.name_id(), m.mixins()),
                Definition::SingletonClass(s) if !s.mixins().is_empty() => (s.name_id(), s.mixins()),
                _ => continue,
            };

            if let Some(NameRef::Resolved(resolved)) = self.names.get(name_id) {
                let decl_id = *resolved.declaration_id();

                let has_include_prepend = mixins
                    .iter()
                    .any(|m| matches!(m, Mixin::Include(_) | Mixin::Prepend(_)));
                let has_extend = mixins.iter().any(|m| matches!(m, Mixin::Extend(_)));

                if has_include_prepend {
                    mixin_declaration_ids.push(decl_id);
                }
                if has_extend {
                    extend_mixin_declaration_ids.push(decl_id);
                }
            }
        }

        // ── Remove old data ──
        self.remove_definitions_for_uri(uri_id);

        // ── Detect new constant names (before extend, after remove) ──
        let new_constant_name_ids: Vec<NameId> = other
            .definitions()
            .values()
            .filter_map(|def| {
                let name_id = def.name_id()?;
                if !def.is_constant_like() {
                    return None;
                }
                if self.names.contains_key(name_id) {
                    None
                } else {
                    Some(*name_id)
                }
            })
            .collect();

        // ── Extend: merge new data ──
        self.extend(other);

        // ── Register dependents (AFTER extend, so nesting chain walks work) ──
        self.register_dependents_for_uri(uri_id);

        // ── Phase A: Invalidate for new mixins ──

        // For extend mixins, also target the singleton class
        for decl_id in &extend_mixin_declaration_ids {
            if let Some(Declaration::Namespace(ns)) = self.declarations.get(decl_id)
                && let Some(singleton_id) = ns.singleton_class()
            {
                mixin_declaration_ids.push(*singleton_id);
            }
        }

        if !mixin_declaration_ids.is_empty() {
            let affected_declaration_ids =
                self.invalidate_ancestor_chains_collecting(mixin_declaration_ids);
            self.unresolve_references_for_declarations_fixpoint(&affected_declaration_ids);
        }

        // ── Phase B: Invalidate for new constants ──
        if !new_constant_name_ids.is_empty() {
            self.unresolve_references_for_new_constants_fixpoint(&new_constant_name_ids);
        }
    }

    // Removes all nodes and relationships associated to the given URI. This is used to clean up stale data when a
    // document (identified by `uri_id`) changes or when a document is closed and we need to clean up the memory
    fn remove_definitions_for_uri(&mut self, uri_id: UriId) {
        // Unregister dependents BEFORE removing definitions (while names are still walkable).
        // This only removes entries for the current URI from name_dependents. References from
        // OTHER files remain registered and will be found by unresolve_references_for_declarations_fixpoint
        // below. References from this URI don't need invalidation since they are being removed entirely.
        self.unregister_dependents_for_uri(uri_id);

        let Some(document) = self.documents.remove(&uri_id) else {
            return;
        };

        // TODO: Remove method references from method declarations once method inference is implemented
        for ref_id in document.method_references() {
            if let Some(method_ref) = self.method_references.remove(ref_id) {
                self.untrack_string(*method_ref.str());
            }
        }

        for ref_id in document.constant_references() {
            if let Some(constant_ref) = self.constant_references.remove(ref_id) {
                if let Some(NameRef::Resolved(resolved)) = self.names.get(constant_ref.name_id())
                    && let Some(declaration) = self.declarations.get_mut(resolved.declaration_id())
                {
                    declaration.remove_reference(ref_id);
                }
                self.untrack_name(*constant_ref.name_id());
            }
        }

        // Vector of (owner_declaration_id, member_name_id) to delete after processing all definitions
        let mut members_to_delete: Vec<(DeclarationId, StringId)> = Vec::new();
        let mut definitions_to_delete: Vec<DefinitionId> = Vec::new();
        let mut declarations_to_delete: Vec<DeclarationId> = Vec::new();
        let mut declarations_to_invalidate_ancestor_chains: Vec<DeclarationId> = Vec::new();

        for def_id in document.definitions() {
            definitions_to_delete.push(*def_id);

            if let Some(declaration_id) = self.definition_id_to_declaration_id(*def_id).copied()
                && let Some(declaration) = self.declarations.get_mut(&declaration_id)
                && declaration.remove_definition(def_id)
            {
                declaration.clear_diagnostics();
                if declaration.as_namespace().is_some() {
                    declarations_to_invalidate_ancestor_chains.push(declaration_id);
                }

                if declaration.has_no_definitions() {
                    let unqualified_str_id = StringId::from(&declaration.unqualified_name());
                    members_to_delete.push((*declaration.owner_id(), unqualified_str_id));
                    declarations_to_delete.push(declaration_id);

                    if let Some(namespace) = declaration.as_namespace()
                        && let Some(singleton_id) = namespace.singleton_class()
                    {
                        declarations_to_delete.push(*singleton_id);
                    }
                }
            }

            if let Some(name_id) = self.definitions.get(def_id).unwrap().name_id() {
                self.untrack_name(*name_id);
            }
        }

        // Invalidate ancestor chains and unresolve dependent references
        let affected_declaration_ids =
            self.invalidate_ancestor_chains_collecting(declarations_to_invalidate_ancestor_chains);
        self.unresolve_references_for_declarations_fixpoint(&affected_declaration_ids);

        for declaration_id in declarations_to_delete {
            self.declarations.remove(&declaration_id);
        }

        // Clean up any members that pointed to declarations that were removed
        for (owner_id, member_str_id) in members_to_delete {
            // Remove the `if` and use `unwrap` once we are indexing RBS files to have `Object`
            if let Some(owner) = self.declarations.get_mut(&owner_id) {
                match owner {
                    Declaration::Namespace(Namespace::Class(owner)) => {
                        owner.remove_member(&member_str_id);
                    }
                    Declaration::Namespace(Namespace::SingletonClass(owner)) => {
                        owner.remove_member(&member_str_id);
                    }
                    Declaration::Namespace(Namespace::Module(owner)) => {
                        owner.remove_member(&member_str_id);
                    }
                    _ => {} // Nothing happens
                }
            }
        }

        for def_id in definitions_to_delete {
            let definition = self.definitions.remove(&def_id).unwrap();
            self.untrack_definition_strings(&definition);
        }
    }

    /// Collects all constant references and constant-like definitions for the given URI
    /// as (id, NameId) pairs. Used by both register and unregister paths.
    fn collect_dependents_for_uri(&self, uri_id: UriId) -> (Vec<(ReferenceId, NameId)>, Vec<(DefinitionId, NameId)>) {
        let Some(doc) = self.documents.get(&uri_id) else {
            return (Vec::new(), Vec::new());
        };

        let ref_entries = doc
            .constant_references()
            .iter()
            .filter_map(|ref_id| {
                let cr = self.constant_references.get(ref_id)?;
                Some((*ref_id, *cr.name_id()))
            })
            .collect();

        let def_entries = doc
            .definitions()
            .iter()
            .filter_map(|def_id| {
                let def = self.definitions.get(def_id)?;
                if def.is_constant_like() {
                    Some((*def_id, *def.name_id()?))
                } else {
                    None
                }
            })
            .collect();

        (ref_entries, def_entries)
    }

    /// Registers all constant references and constant-like definitions from the given URI
    /// into the `name_dependents` side table. Must be called AFTER `extend()` so that
    /// nesting chain walks can follow links through the global name table.
    fn register_dependents_for_uri(&mut self, uri_id: UriId) {
        let (ref_entries, def_entries) = self.collect_dependents_for_uri(uri_id);

        for (ref_id, name_id) in ref_entries {
            self.register_dependent_on_nesting_chain(name_id, NameDependent::Reference(ref_id));
        }
        for (def_id, name_id) in def_entries {
            self.register_dependent_on_nesting_chain(name_id, NameDependent::Definition(def_id));
        }
    }

    fn register_dependent_on_nesting_chain(&mut self, name_id: NameId, dependent: NameDependent) {
        let mut current = Some(name_id);
        while let Some(id) = current {
            self.name_dependents.entry(id).or_default().push(dependent);
            current = self.names.get(&id).and_then(|n| *n.nesting());
        }
    }

    /// Unregisters all constant references and constant-like definitions for the given URI
    /// from the `name_dependents` side table. Must be called BEFORE removing definitions,
    /// while names are still walkable.
    fn unregister_dependents_for_uri(&mut self, uri_id: UriId) {
        let (ref_entries, def_entries) = self.collect_dependents_for_uri(uri_id);

        for (ref_id, name_id) in ref_entries {
            self.unregister_dependent_from_nesting_chain(name_id, &NameDependent::Reference(ref_id));
        }
        for (def_id, name_id) in def_entries {
            self.unregister_dependent_from_nesting_chain(name_id, &NameDependent::Definition(def_id));
        }
    }

    fn unregister_dependent_from_nesting_chain(&mut self, name_id: NameId, dependent: &NameDependent) {
        let mut current = Some(name_id);
        while let Some(id) = current {
            if let Some(deps) = self.name_dependents.get_mut(&id) {
                deps.retain(|d| d != dependent);
                if deps.is_empty() {
                    self.name_dependents.remove(&id);
                }
            }
            current = self.names.get(&id).and_then(|n| *n.nesting());
        }
    }

    /// Invalidates ancestor chains for the given declaration IDs (and their descendants).
    /// Returns the set of all declaration IDs whose ancestors were invalidated.
    fn invalidate_ancestor_chains_collecting(
        &mut self,
        initial_ids: Vec<DeclarationId>,
    ) -> IdentityHashSet<DeclarationId> {
        let mut queue = initial_ids;
        let mut visited = IdentityHashSet::<DeclarationId>::default();

        while let Some(declaration_id) = queue.pop() {
            if !visited.insert(declaration_id) {
                continue;
            }

            let Some(namespace) = self
                .declarations
                .get_mut(&declaration_id)
                .and_then(|d| d.as_namespace_mut())
            else {
                continue;
            };

            for ancestor in &namespace.clone_ancestors() {
                if let Ancestor::Complete(ancestor_id) = ancestor
                    && let Some(anc_ns) = self
                        .declarations
                        .get_mut(ancestor_id)
                        .and_then(|d| d.as_namespace_mut())
                {
                    anc_ns.remove_descendant(&declaration_id);
                }
            }

            let namespace = self
                .declarations
                .get_mut(&declaration_id)
                .unwrap()
                .as_namespace_mut()
                .unwrap();

            namespace.for_each_descendant(|descendant_id| {
                queue.push(*descendant_id);
            });

            namespace.clear_ancestors();
            namespace.clear_descendants();
        }

        visited
    }

    /// Process a batch of (`ReferenceId`, `NameId`) pairs: remove references from declarations,
    /// unresolve names, and detect mixin cascade owners.
    ///
    /// Batching is important because multiple references can share the same `NameId`. By removing
    /// all references from declarations BEFORE unresolving any names, we ensure every reference
    /// is properly cleaned up regardless of shared `NameId`s.
    fn batch_unresolve_references(
        &mut self,
        refs_to_unresolve: &[(ReferenceId, NameId)],
    ) -> Vec<DeclarationId> {
        // Step 1: Remove references from their resolved declarations.
        // Do this while names are still Resolved so all refs can find their declaration.
        let mut names_to_unresolve = IdentityHashSet::<NameId>::default();
        for (ref_id, name_id) in refs_to_unresolve {
            if let Some(NameRef::Resolved(resolved)) = self.names.get(name_id) {
                let declaration_id = *resolved.declaration_id();
                if let Some(declaration) = self.declarations.get_mut(&declaration_id) {
                    declaration.remove_reference(ref_id);
                }
                names_to_unresolve.insert(*name_id);
            }
        }

        // Step 2: Unresolve names (Resolved → Unresolved)
        for name_id in &names_to_unresolve {
            if let Some(NameRef::Resolved(resolved)) = self.names.remove(name_id) {
                let name = resolved.into_name();
                self.names.insert(*name_id, NameRef::Unresolved(Box::new(name)));
            }
        }

        // Step 3: Detect mixin owners for cascade
        let mut mixin_owner_declarations = Vec::new();
        for (ref_id, _) in refs_to_unresolve {
            if let Some(decl_id) = self.find_mixin_owner_for_reference(*ref_id) {
                mixin_owner_declarations.push(decl_id);
            }
        }

        mixin_owner_declarations
    }

    /// Given a reference ID, find if any definition has a mixin that uses this reference.
    /// Returns the declaration that owns the definition with the mixin.
    ///
    /// Walks the nesting chain upward from the reference's `NameId` because the owning
    /// definition (e.g. `class Baz`) is registered at a different `NameId` than the mixin
    /// reference (e.g. `include Bar`). They share nesting ancestor `NameId`s.
    fn find_mixin_owner_for_reference(&self, ref_id: ReferenceId) -> Option<DeclarationId> {
        let const_ref = self.constant_references.get(&ref_id)?;
        let mut current = Some(*const_ref.name_id());

        while let Some(id) = current {
            if let Some(dependents) = self.name_dependents.get(&id) {
                for dependent in dependents {
                    if let NameDependent::Definition(def_id) = dependent {
                        let Some(definition) = self.definitions.get(def_id) else {
                            continue;
                        };
                        let mixins = match definition {
                            Definition::Class(c) => c.mixins(),
                            Definition::Module(m) => m.mixins(),
                            Definition::SingletonClass(s) => s.mixins(),
                            _ => continue,
                        };

                        for mixin in mixins {
                            if *mixin.constant_reference_id() == ref_id {
                                return self.definition_id_to_declaration_id(*def_id).copied();
                            }
                        }
                    }
                }
            }
            current = self.names.get(&id).and_then(|n| *n.nesting());
        }
        None
    }

    /// Given a set of affected declarations (whose ancestor chains changed), look up their
    /// `NameId`s in `name_dependents` to find all nested references and unresolve them.
    /// Uses a fixed-point loop to handle cascading mixin invalidation.
    fn unresolve_references_for_declarations_fixpoint(
        &mut self,
        initial_affected_ids: &IdentityHashSet<DeclarationId>,
    ) {
        let mut affected_declaration_ids: IdentityHashSet<DeclarationId> =
            initial_affected_ids.iter().copied().collect();
        let mut globally_visited = IdentityHashSet::<DeclarationId>::default();

        loop {
            let current_ids = std::mem::take(&mut affected_declaration_ids);
            if current_ids.is_empty() {
                break;
            }

            let mut refs_to_unresolve: Vec<(ReferenceId, NameId)> = Vec::new();

            for decl_id in current_ids {
                if !globally_visited.insert(decl_id) {
                    continue;
                }

                let Some(declaration) = self.declarations.get(&decl_id) else {
                    continue;
                };

                // Get all definition NameIds for this declaration
                let name_ids: Vec<NameId> = declaration
                    .definitions()
                    .iter()
                    .filter_map(|def_id| {
                        let def = self.definitions.get(def_id)?;
                        def.name_id().copied()
                    })
                    .collect();

                for name_id in name_ids {
                    let Some(dependents) = self.name_dependents.get(&name_id) else {
                        continue;
                    };

                    for dependent in dependents {
                        if let NameDependent::Reference(ref_id) = dependent
                            && let Some(const_ref) = self.constant_references.get(ref_id)
                        {
                            let ref_name_id = *const_ref.name_id();
                            if matches!(self.names.get(&ref_name_id), Some(NameRef::Resolved(_))) {
                                refs_to_unresolve.push((*ref_id, ref_name_id));
                            }
                        }
                    }
                }
            }

            if refs_to_unresolve.is_empty() {
                break;
            }

            let mixin_owner_declarations = self.batch_unresolve_references(&refs_to_unresolve);

            if mixin_owner_declarations.is_empty() {
                break;
            }

            // Cascade: invalidate ancestor chains of mixin owners, producing the next wave
            affected_declaration_ids =
                self.invalidate_ancestor_chains_collecting(mixin_owner_declarations);
        }
    }

    /// Sub-phase B.1: Lexical shadowing. Walk UP the nesting chain of the new constant,
    /// checking dependents at each level for bare references with matching str.
    fn collect_lexical_shadow_refs(
        &self,
        new_name_ref: &NameRef,
        new_str: StringId,
    ) -> Vec<(ReferenceId, NameId)> {
        let mut refs = Vec::new();
        let mut current_nesting = *new_name_ref.nesting();

        while let Some(nesting_id) = current_nesting {
            if let Some(dependents) = self.name_dependents.get(&nesting_id) {
                for dependent in dependents {
                    if let NameDependent::Reference(ref_id) = dependent
                        && let Some(const_ref) = self.constant_references.get(ref_id)
                        && let Some(ref_name) = self.names.get(const_ref.name_id())
                        && ref_name.parent_scope().is_none()
                        && *ref_name.str() == new_str
                        && matches!(ref_name, NameRef::Resolved(_))
                    {
                        refs.push((*ref_id, *const_ref.name_id()));
                    }
                }
            }

            current_nesting = self.names.get(&nesting_id).and_then(|n| *n.nesting());
        }

        refs
    }

    /// Sub-phase B.2: Ancestor member addition. Walk DOWN through descendants of the
    /// new constant's parent namespace, checking for references with matching str.
    fn collect_ancestor_member_refs(
        &self,
        new_name_ref: &NameRef,
        new_str: StringId,
    ) -> Vec<(ReferenceId, NameId)> {
        let mut refs = Vec::new();

        let parent_decl_id = match new_name_ref.nesting() {
            Some(nesting_id) => match self.names.get(nesting_id) {
                Some(NameRef::Resolved(resolved)) => Some(*resolved.declaration_id()),
                _ => None,
            },
            None => Some(*OBJECT_ID),
        };

        let Some(parent_decl_id) = parent_decl_id else {
            return refs;
        };

        let descendant_ids: Vec<DeclarationId> =
            if let Some(Declaration::Namespace(ns)) = self.declarations.get(&parent_decl_id) {
                ns.descendants().iter().copied().collect()
            } else {
                Vec::new()
            };

        for descendant_id in descendant_ids {
            let Some(declaration) = self.declarations.get(&descendant_id) else {
                continue;
            };

            let name_ids: Vec<NameId> = declaration
                .definitions()
                .iter()
                .filter_map(|def_id| {
                    let def = self.definitions.get(def_id)?;
                    def.name_id().copied()
                })
                .collect();

            for name_id in name_ids {
                let Some(dependents) = self.name_dependents.get(&name_id) else {
                    continue;
                };

                for dependent in dependents {
                    if let NameDependent::Reference(ref_id) = dependent
                        && let Some(const_ref) = self.constant_references.get(ref_id)
                        && let Some(ref_name) = self.names.get(const_ref.name_id())
                        && *ref_name.str() == new_str
                        && matches!(ref_name, NameRef::Resolved(_))
                    {
                        refs.push((*ref_id, *const_ref.name_id()));
                    }
                }
            }
        }

        refs
    }

    /// When new constant definitions appear, check if they can shadow existing resolved references.
    /// Two sub-phases:
    /// B.1: Lexical shadowing - walk UP the nesting chain
    /// B.2: Ancestor member addition - walk DOWN through descendants of parent namespace
    fn unresolve_references_for_new_constants_fixpoint(&mut self, new_constant_name_ids: &[NameId]) {
        let mut refs_to_unresolve: Vec<(ReferenceId, NameId)> = Vec::new();

        for new_name_id in new_constant_name_ids {
            let Some(new_name_ref) = self.names.get(new_name_id) else {
                continue;
            };
            let new_str = *new_name_ref.str();

            refs_to_unresolve.extend(self.collect_lexical_shadow_refs(new_name_ref, new_str));
            refs_to_unresolve.extend(self.collect_ancestor_member_refs(new_name_ref, new_str));
        }

        // Perform unresolving + mixin cascade
        let mixin_owner_declarations = self.batch_unresolve_references(&refs_to_unresolve);

        if !mixin_owner_declarations.is_empty() {
            let affected = self.invalidate_ancestor_chains_collecting(mixin_owner_declarations);
            self.unresolve_references_for_declarations_fixpoint(&affected);
        }
    }

    /// Sets the encoding that should be used for transforming byte offsets into LSP code unit line/column positions
    pub fn set_encoding(&mut self, encoding: Encoding) {
        self.position_encoding = encoding;
    }

    #[must_use]
    pub fn encoding(&self) -> &Encoding {
        &self.position_encoding
    }

    #[allow(clippy::cast_precision_loss)]
    pub fn print_query_statistics(&self) {
        use std::collections::{HashMap, HashSet};

        let mut declarations_with_docs = 0;
        let mut total_doc_size = 0;
        let mut multi_definition_count = 0;
        let mut declarations_types: HashMap<&str, usize> = HashMap::new();
        let mut linked_definition_types: HashMap<&str, usize> = HashMap::new();
        let mut linked_definition_ids: HashSet<&DefinitionId> = HashSet::new();

        for declaration in self.declarations.values() {
            // Check documentation
            if let Some(definitions) = self.get(declaration.name()) {
                let has_docs = definitions.iter().any(|def| !def.comments().is_empty());
                if has_docs {
                    declarations_with_docs += 1;
                    let doc_size: usize = definitions.iter().map(|def| def.comments().len()).sum();
                    total_doc_size += doc_size;
                }
            }

            *declarations_types.entry(declaration.kind()).or_insert(0) += 1;

            // Count definitions by type
            let definition_count = declaration.definitions().len();
            if definition_count > 1 {
                multi_definition_count += 1;
            }

            for def_id in declaration.definitions() {
                linked_definition_ids.insert(def_id);
                if let Some(def) = self.definitions().get(def_id) {
                    *linked_definition_types.entry(def.kind()).or_insert(0) += 1;
                }
            }
        }

        // Count ALL definitions by type (including unlinked)
        let mut all_definition_types: HashMap<&str, usize> = HashMap::new();
        for def in self.definitions.values() {
            *all_definition_types.entry(def.kind()).or_insert(0) += 1;
        }

        println!();
        println!("Query statistics");
        let total_declarations = self.declarations.len();
        println!("  Total declarations:         {total_declarations}");
        println!(
            "  With documentation:         {} ({:.1}%)",
            declarations_with_docs,
            stats::percentage(declarations_with_docs, total_declarations)
        );
        println!(
            "  Without documentation:      {} ({:.1}%)",
            total_declarations - declarations_with_docs,
            stats::percentage(total_declarations - declarations_with_docs, total_declarations)
        );
        println!("  Total documentation size:   {total_doc_size} bytes");
        println!(
            "  Multi-definition names:     {} ({:.1}%)",
            multi_definition_count,
            stats::percentage(multi_definition_count, total_declarations)
        );

        println!();
        println!("Declaration breakdown:");
        let mut types: Vec<_> = declarations_types.iter().collect();
        types.sort_by_key(|(_, count)| std::cmp::Reverse(**count));
        for (kind, count) in types {
            println!("  {kind:20} {count:6}");
        }

        // Combined definition breakdown: total, linked, orphan
        println!();
        println!("Definition breakdown:");
        println!("  {:20} {:>8} {:>8} {:>8}", "Type", "Total", "Linked", "Orphan");
        println!("  {:20} {:>8} {:>8} {:>8}", "----", "-----", "------", "------");

        let mut definition_types: Vec<_> = all_definition_types.iter().collect();
        definition_types.sort_by_key(|(_, total)| std::cmp::Reverse(**total));

        for (kind, total) in definition_types {
            let linked = linked_definition_types.get(kind).unwrap_or(&0);
            let orphan = total.saturating_sub(*linked);
            println!("  {kind:20} {total:>8} {linked:>8} {orphan:>8}");
        }

        // Definition linkage summary
        let total_definitions = self.definitions.len();
        let linked_count = linked_definition_ids.len();
        let unlinked_count = total_definitions - linked_count;
        println!("  {:20} {:>8} {:>8} {:>8}", "----", "-----", "------", "------");
        println!(
            "  {:20} {:>8} {:>8} {:>8}",
            "TOTAL", total_definitions, linked_count, unlinked_count
        );
        println!(
            "  Orphan rate: {:.1}%",
            stats::percentage(unlinked_count, total_definitions)
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::comment::Comment;
    use crate::model::declaration::Ancestors;
    use crate::test_utils::GraphTest;
    use crate::{
        assert_alias_targets_contain, assert_ancestors_eq, assert_constant_reference_to,
        assert_declaration_references_count_eq, assert_descendants, assert_members_eq, assert_no_constant_alias_target,
        assert_no_diagnostics, assert_no_members,
    };

    #[test]
    fn deleting_a_uri() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        context.delete_uri("file:///foo.rb");
        context.resolve();

        assert!(context.graph().documents.is_empty());
        assert!(context.graph().definitions.is_empty());
        // Object is left
        assert!(
            context
                .graph()
                .declarations()
                .get(&DeclarationId::from("Foo"))
                .is_none()
        );
    }

    #[test]
    fn updating_index_with_deleted_definitions() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");

        assert_eq!(context.graph().definitions.len(), 1);
        assert_eq!(context.graph().documents.len(), 1);

        // Update with empty content to remove definitions but keep the URI
        context.index_uri("file:///foo.rb", "");

        assert!(context.graph().definitions.is_empty());

        // URI remains if the file was not deleted, but definitions got erased
        assert_eq!(context.graph().documents.len(), 1);
    }

    #[test]
    fn updating_index_with_deleted_definitions_after_resolution() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        context.resolve();

        assert_eq!(context.graph().definitions.len(), 1);
        assert_eq!(context.graph().documents.len(), 1);

        {
            assert!(
                context
                    .graph()
                    .declarations()
                    .get(&DeclarationId::from("Foo"))
                    .is_some()
            );
        }

        // Update with empty content to remove definitions but keep the URI
        context.index_uri("file:///foo.rb", "");

        assert!(context.graph().definitions.is_empty());
        // URI remains if the file was not deleted, but definitions and declarations got erased
        assert_eq!(context.graph().documents.len(), 1);

        {
            assert!(
                context
                    .graph()
                    .declarations()
                    .get(&DeclarationId::from("Foo"))
                    .is_none()
            );
        }
    }

    #[test]
    fn updating_index_with_deleted_references() {
        let mut context = GraphTest::new();

        context.index_uri("file:///definition.rb", "module Foo; end");
        context.index_uri(
            "file:///references.rb",
            r"
            Foo
            bar
            BAZ
            ",
        );
        context.resolve();

        assert_eq!(context.graph().documents.len(), 2);
        assert_eq!(context.graph().method_references.len(), 1);
        assert_eq!(context.graph().constant_references.len(), 2);
        {
            let declaration = context.graph().declarations().get(&DeclarationId::from("Foo")).unwrap();
            assert_eq!(declaration.references().len(), 1);
        }

        // Update with empty content to remove definitions but keep the URI
        context.index_uri("file:///references.rb", "");

        // URI remains if the file was not deleted, but references got erased
        assert_eq!(context.graph().documents.len(), 2);
        assert!(context.graph().method_references.is_empty());
        assert!(context.graph().constant_references.is_empty());
        {
            let declaration = context.graph().declarations().get(&DeclarationId::from("Foo")).unwrap();
            assert!(declaration.references().is_empty());
        }
    }

    #[test]
    fn invalidating_ancestor_chains_when_document_changes() {
        let mut context = GraphTest::new();

        context.index_uri("file:///a.rb", "class Foo; include Bar; def method_name; end; end");
        context.index_uri("file:///b.rb", "class Foo; end");
        context.index_uri("file:///c.rb", "module Bar; end");
        context.index_uri("file:///d.rb", "class Baz < Foo; end");
        context.resolve();

        let foo_declaration = context.graph().declarations().get(&DeclarationId::from("Foo")).unwrap();
        assert!(matches!(
            foo_declaration.as_namespace().unwrap().ancestors(),
            Ancestors::Complete(_)
        ));

        let baz_declaration = context.graph().declarations().get(&DeclarationId::from("Baz")).unwrap();
        assert!(matches!(
            baz_declaration.as_namespace().unwrap().ancestors(),
            Ancestors::Complete(_)
        ));

        {
            let Declaration::Namespace(Namespace::Module(_bar)) =
                context.graph().declarations().get(&DeclarationId::from("Bar")).unwrap()
            else {
                panic!("Expected Bar to be a module");
            };
            assert_descendants!(context, "Bar", ["Foo"]);
        }
        assert_descendants!(context, "Foo", ["Baz"]);

        context.index_uri("file:///a.rb", "");

        {
            let Declaration::Namespace(Namespace::Class(foo)) =
                context.graph().declarations().get(&DeclarationId::from("Foo")).unwrap()
            else {
                panic!("Expected Foo to be a class");
            };
            assert!(matches!(foo.ancestors(), Ancestors::Partial(a) if a.is_empty()));
            assert!(foo.descendants().is_empty());

            let Declaration::Namespace(Namespace::Class(baz)) =
                context.graph().declarations().get(&DeclarationId::from("Baz")).unwrap()
            else {
                panic!("Expected Baz to be a class");
            };
            assert!(matches!(baz.ancestors(), Ancestors::Partial(a) if a.is_empty()));
            assert!(baz.descendants().is_empty());

            let Declaration::Namespace(Namespace::Module(bar)) =
                context.graph().declarations().get(&DeclarationId::from("Bar")).unwrap()
            else {
                panic!("Expected Bar to be a module");
            };
            assert!(!bar.descendants().contains(&DeclarationId::from("Foo")));
        }

        context.resolve();

        let baz_declaration = context.graph().declarations().get(&DeclarationId::from("Baz")).unwrap();
        assert!(matches!(
            baz_declaration.as_namespace().unwrap().clone_ancestors(),
            Ancestors::Complete(_)
        ));

        assert_descendants!(context, "Foo", ["Baz"]);
    }

    #[test]
    fn name_count_increments_for_duplicates() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        context.index_uri("file:///foo2.rb", "module Foo; end");
        context.index_uri("file:///foo3.rb", "Foo");
        context.resolve();

        assert_eq!(context.graph().names().len(), 1);
        let name_ref = context.graph().names().values().next().unwrap();
        assert_eq!(name_ref.ref_count(), 3);
    }

    #[test]
    fn string_ref_count_increments_for_duplicate_definitions() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            def method_name; end
            attr_accessor :accessor_name
            attr_reader :reader_name
            attr_writer :writer_name
            $global_var = 1
            @@class_var = 1
            class Foo
              def initialize
                @instance_var = 1
              end
            end
            def old_method; end
            alias_method :new_method, :old_method
            $old_global = 1
            alias $new_global $old_global
            ",
        );

        context.resolve();

        let strings = context.graph().strings();
        assert_eq!(strings.get(&StringId::from("method_name()")).unwrap().ref_count(), 1);
        assert_eq!(strings.get(&StringId::from("accessor_name()")).unwrap().ref_count(), 1);
        assert_eq!(strings.get(&StringId::from("reader_name()")).unwrap().ref_count(), 1);
        assert_eq!(strings.get(&StringId::from("writer_name()")).unwrap().ref_count(), 1);
        assert_eq!(strings.get(&StringId::from("$global_var")).unwrap().ref_count(), 1);
        assert_eq!(strings.get(&StringId::from("@@class_var")).unwrap().ref_count(), 1);
        assert_eq!(strings.get(&StringId::from("@instance_var")).unwrap().ref_count(), 1);
        assert_eq!(strings.get(&StringId::from("old_method()")).unwrap().ref_count(), 2);
        assert_eq!(strings.get(&StringId::from("new_method()")).unwrap().ref_count(), 1);
        assert_eq!(strings.get(&StringId::from("$old_global")).unwrap().ref_count(), 2);
        assert_eq!(strings.get(&StringId::from("$new_global")).unwrap().ref_count(), 1);
    }

    #[test]
    fn updating_index_with_deleted_names() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        context.index_uri("file:///bar.rb", "Foo");
        context.resolve();

        assert_eq!(context.graph().names().len(), 1);
        assert_eq!(context.graph().names().values().next().unwrap().ref_count(), 2);

        context.delete_uri("file:///foo.rb");
        assert_eq!(context.graph().names().len(), 1);
        assert_eq!(context.graph().names().values().next().unwrap().ref_count(), 1);

        context.delete_uri("file:///bar.rb");
        assert!(context.graph().names().is_empty());
    }

    #[test]
    fn updating_index_with_deleted_strings() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            Foo
            foo.method_call
            def method_name; end
            ",
        );
        context.resolve();

        let strings = context.graph().strings();
        assert!(strings.get(&StringId::from("Foo")).is_some());
        assert!(strings.get(&StringId::from("method_call")).is_some());
        assert!(strings.get(&StringId::from("method_name()")).is_some());

        context.delete_uri("file:///foo.rb");
        let strings = context.graph().strings();
        assert!(strings.get(&StringId::from("Foo")).is_none());
        assert!(strings.get(&StringId::from("method_call")).is_none());
        assert!(strings.get(&StringId::from("method_name()")).is_none());
    }

    #[test]
    fn updating_index_with_new_definitions() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        context.resolve();

        assert_eq!(context.graph().definitions.len(), 1);
        let declaration = context.graph().declarations().get(&DeclarationId::from("Foo")).unwrap();
        assert_eq!(declaration.name(), "Foo");
        let document = context.graph().documents.get(&UriId::from("file:///foo.rb")).unwrap();
        assert_eq!(document.uri(), "file:///foo.rb");
        assert_eq!(declaration.definitions().len(), 1);
        assert_eq!(document.definitions().len(), 1);
    }

    #[test]
    fn updating_existing_definitions() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        // Update with the same definition but at a different position (with content before it)
        context.index_uri("file:///foo.rb", "\n\n\n\n\n\nmodule Foo; end");
        context.resolve();

        assert_eq!(context.graph().definitions.len(), 1);
        let declaration = context.graph().declarations().get(&DeclarationId::from("Foo")).unwrap();
        assert_eq!(declaration.name(), "Foo");
        assert_eq!(
            context
                .graph()
                .documents()
                .get(&UriId::from("file:///foo.rb"))
                .unwrap()
                .uri(),
            "file:///foo.rb"
        );

        let definitions = context.graph().get("Foo").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].offset().start(), 6);
    }

    #[test]
    fn adding_another_definition_from_a_different_uri() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        context.index_uri("file:///foo2.rb", "\n\n\n\n\nmodule Foo; end");
        context.resolve();

        let definitions = context.graph().get("Foo").unwrap();
        let mut offsets = definitions.iter().map(|d| d.offset().start()).collect::<Vec<_>>();
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

        context.resolve();

        let definitions = context.graph().get("Foo").unwrap();
        assert_eq!(definitions.len(), 2);

        let mut offsets = definitions
            .iter()
            .map(|d| [d.offset().start(), d.offset().end()])
            .collect::<Vec<_>>();
        offsets.sort_unstable();
        assert_eq!([0, 15], offsets[0]);
        assert_eq!([18, 33], offsets[1]);
    }

    #[test]
    fn get_documentation() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            # This is a class comment
            # Multi-line comment
            class CommentedClass; end

            # Module comment
            module CommentedModule; end

            class NoCommentClass; end
            "
        });

        context.resolve();

        let definitions = context.graph().get("CommentedClass").unwrap();
        let def = definitions.first().unwrap();
        assert_eq!(
            def.comments().iter().map(Comment::string).collect::<Vec<&String>>(),
            ["# This is a class comment", "# Multi-line comment"]
        );

        let definitions = context.graph().get("CommentedModule").unwrap();
        let def = definitions.first().unwrap();
        assert_eq!(
            def.comments().iter().map(Comment::string).collect::<Vec<&String>>(),
            ["# Module comment"]
        );

        let definitions = context.graph().get("NoCommentClass").unwrap();
        let def = definitions.first().unwrap();
        assert!(def.comments().is_empty());
    }

    #[test]
    fn members_are_updated_when_definitions_get_deleted() {
        let mut context = GraphTest::new();
        // Initially, have `Foo` defined twice with a member called `Bar`
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
            end
            "
        });
        context.index_uri("file:///foo2.rb", {
            r"
            module Foo
              class Bar; end
            end
            "
        });
        context.resolve();

        assert_members_eq!(context, "Foo", ["Bar"]);

        // Delete `Bar`
        context.index_uri("file:///foo2.rb", {
            r"
            module Foo
            end
            "
        });
        context.resolve();

        assert_no_members!(context, "Foo");
    }

    #[test]
    fn updating_index_with_deleted_diagnostics() {
        let mut context = GraphTest::new();

        // TODO: Add resolution error to test diagnostics attached to declarations
        context.index_uri("file:///foo.rb", "class Foo");
        assert!(!context.graph().all_diagnostics().is_empty());

        context.index_uri("file:///foo.rb", "class Foo; end");
        assert_no_diagnostics!(&context);
    }

    #[test]
    fn diagnostics_are_collected() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo1.rb", {
            r"
            class Foo
            "
        });

        context.index_uri("file:///foo2.rb", {
            r"
            foo = 42
            "
        });

        let mut diagnostics: Vec<String> = context
            .graph()
            .all_diagnostics()
            .iter()
            .map(|d| {
                format!(
                    "{}: {} ({})",
                    d.rule(),
                    d.message(),
                    context.graph().documents().get(d.uri_id()).unwrap().uri()
                )
            })
            .collect();

        diagnostics.sort();

        assert_eq!(
            vec![
                "parse-error: expected an `end` to close the `class` statement (file:///foo1.rb)",
                "parse-error: unexpected end-of-input, assuming it is closing the parent top level context (file:///foo1.rb)",
                "parse-warning: assigned but unused variable - foo (file:///foo2.rb)",
            ],
            diagnostics,
        );
    }

    #[test]
    fn removing_method_def_with_conflicting_constant_name() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            "
            class Foo
              class Array; end
            end
            "
        });
        context.index_uri("file:///foo2.rb", {
            "
            class Foo
              def Array; end
            end
            "
        });

        context.resolve();
        // Removing the method should not remove the constant
        context.index_uri("file:///foo2.rb", "");

        let foo = context
            .graph()
            .declarations()
            .get(&DeclarationId::from("Foo"))
            .unwrap()
            .as_namespace()
            .unwrap();

        assert!(foo.member(&StringId::from("Array")).is_some());
        assert!(foo.member(&StringId::from("Array()")).is_none());
    }

    #[test]
    fn removing_constant_with_conflicting_method_name() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            "
            class Foo
              class Array; end
            end
            "
        });
        context.index_uri("file:///foo2.rb", {
            "
            class Foo
              def Array; end
            end
            "
        });

        context.resolve();
        // Removing the method should not remove the constant
        context.index_uri("file:///foo.rb", "");

        let foo = context
            .graph()
            .declarations()
            .get(&DeclarationId::from("Foo"))
            .unwrap()
            .as_namespace()
            .unwrap();
        assert!(foo.member(&StringId::from("Array()")).is_some());
        assert!(foo.member(&StringId::from("Array")).is_none());
    }

    #[test]
    fn deleting_class_also_deletes_singleton_class() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def self.hello; end
            end
            "
        });
        context.resolve();

        assert!(context.graph().get("Foo").is_some());
        assert!(context.graph().get("Foo::<Foo>").is_some());

        context.delete_uri("file:///foo.rb");

        assert!(context.graph().get("Foo").is_none());
        assert!(context.graph().get("Foo::<Foo>").is_none());
    }

    #[test]
    fn deleting_module_also_deletes_singleton_class() {
        let mut context = GraphTest::new();

        context.index_uri("file:///bar.rb", {
            r"
            module Bar
              def self.greet; end
            end
            "
        });
        context.resolve();

        assert!(context.graph().get("Bar").is_some());
        assert!(context.graph().get("Bar::<Bar>").is_some());

        context.delete_uri("file:///bar.rb");

        assert!(context.graph().get("Bar").is_none());
        assert!(context.graph().get("Bar::<Bar>").is_none());
    }

    #[test]
    fn deleting_nested_class_also_deletes_singleton_class() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///nested.rb",
            r"
            class Outer
              class Inner
                def self.method; end
              end
            end
            ",
        );
        context.resolve();

        assert!(context.graph().get("Outer").is_some());
        assert!(context.graph().get("Outer::Inner").is_some());
        assert!(context.graph().get("Outer::Inner::<Inner>").is_some());

        context.delete_uri("file:///nested.rb");

        assert!(context.graph().get("Outer").is_none());
        assert!(context.graph().get("Outer::Inner").is_none());
        assert!(context.graph().get("Outer::Inner::<Inner>").is_none());
    }

    #[test]
    fn deleting_singleton_class_also_deletes_its_singleton_class() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            r"
            class Foo
              class << self
                def self.hello; end
              end
            end
            ",
        );
        context.resolve();

        assert!(context.graph().get("Foo").is_some());
        assert!(context.graph().get("Foo::<Foo>").is_some());
        assert!(context.graph().get("Foo::<Foo>::<<Foo>>").is_some());

        context.delete_uri("file:///foo.rb");

        assert!(context.graph().get("Foo").is_none());
        assert!(context.graph().get("Foo::<Foo>").is_none());
        assert!(context.graph().get("Foo::<Foo>::<<Foo>>").is_none());
    }

    #[test]
    fn indexing_the_same_document_twice() {
        let mut context = GraphTest::new();
        let source = "
          module Bar; end

          $global_var_1 = 1
          alias $global_alias_1 $global_var_1
          ALIAS_CONST_1 = Bar

          class Foo
            alias $global_alias_2 $global_var_1
            attr_reader :attr_1
            attr_writer :attr_2
            attr_accessor :attr_3
            ALIAS_CONST_2 = Bar

            $global_var_2 = 1
            @ivar_1 = 1
            @@class_var_1 = 1

            def method_1
              $global_var_3 = 1
              @ivar_2 = 1
              @@class_var_2 = 1
              ALIAS_CONST_3 = Bar
            end
            alias_method :aliased_method_1, :method_1

            def self.method_2
              $global_var_4 = 1
              @ivar_3 = 1
              @@class_var_3 = 1
              ALIAS_CONST_4 = Bar
            end

            class << self
              alias $global_alias_3 $global_var_1
              attr_reader :attr_4
              attr_writer :attr_5
              attr_accessor :attr_6
              ALIAS_CONST_5 = Bar

              $global_var_3 = 1
              @ivar_4 = 1
              @@class_var_4 = 1

              def method_3
                $global_var_4 = 1
                @ivar_5 = 1
                @@class_var_5 = 1
                ALIAS_CONST_6 = Bar
              end
              alias_method :aliased_method_1, :method_1

              def self.method_4
                $global_var_5 = 1
                @ivar_6 = 1
                @@class_var_6 = 1
                ALIAS_CONST_7 = Bar
              end
            end
          end
        ";

        context.index_uri("file:///foo.rb", source);
        assert_eq!(44, context.graph().definitions.len());
        assert_eq!(7, context.graph().constant_references.len());
        assert_eq!(2, context.graph().method_references.len());
        assert_eq!(1, context.graph().documents.len());
        assert_eq!(12, context.graph().names.len());
        assert_eq!(41, context.graph().strings.len());
        context.index_uri("file:///foo.rb", source);
        assert_eq!(44, context.graph().definitions.len());
        assert_eq!(7, context.graph().constant_references.len());
        assert_eq!(2, context.graph().method_references.len());
        assert_eq!(1, context.graph().documents.len());
        assert_eq!(12, context.graph().names.len());
        assert_eq!(41, context.graph().strings.len());
    }

    #[test]
    fn ancestor_changes_invalidate_constant_references() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            r"
            module Foo
              CONST = 1
            end

            module Bar
              CONST = 2
            end
            ",
        );
        context.index_uri(
            "file:///foo2.rb",
            r"
            class Baz
              include Foo

              CONST
            end
            ",
        );
        context.resolve();

        // Initially, CONST points to `Foo::CONST`
        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo2.rb:4:3-4:8");
        assert_declaration_references_count_eq!(context, "Foo::CONST", 1);

        // By adding a new file that prepends `Bar`, we change the ancestors of `Baz` and now `CONST` should point to
        // `Bar::CONST`
        context.index_uri(
            "file:///foo3.rb",
            r"
            class Baz
              prepend Bar
            end
            ",
        );

        // Verify that indexing the new file unresolves the constant reference
        let reference_name = context
            .graph()
            .constant_references()
            .values()
            .find_map(|r| {
                let name = context.graph().names().get(r.name_id()).unwrap();

                if context.graph().strings().get(name.str()).unwrap().as_str() == "CONST" {
                    Some(name)
                } else {
                    None
                }
            })
            .unwrap();

        assert!(
            matches!(reference_name, NameRef::Unresolved(_)),
            "Did not properly invalidate constant reference"
        );
        assert_declaration_references_count_eq!(context, "Foo::CONST", 0);
    }

    #[test]
    fn new_constants_invalidate_applicable_references() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            r"
            module Foo
              module Bar
                module Baz
                end
              end
            end
            ",
        );
        context.index_uri(
            "file:///qux.rb",
            r"
            module Foo
              module Bar
                module Baz
                  class Qux
                    include Bar
                  end
                end
              end
            end
            ",
        );
        context.resolve();

        // Initially, `Bar` points to `Foo::Bar`
        assert_constant_reference_to!(context, "Foo::Bar", "file:///qux.rb:5:17-5:20");
        assert_declaration_references_count_eq!(context, "Foo::Bar", 1);
        assert_ancestors_eq!(
            context,
            "Foo::Bar::Baz::Qux",
            ["Foo::Bar::Baz::Qux", "Foo::Bar", "Object"]
        );

        // After a new `Bar` constant is created closer in the lexical scope of `Qux`, the constant reference needs to
        // be invalidated alongside `Qux`'s ancestor chain
        context.index_uri(
            "file:///foo.rb",
            r"
            module Foo
              module Bar
                module Baz
                  module Bar; end
                end
              end
            end
            ",
        );

        // Verify that indexing the new file unresolves the constant reference
        let reference_name = context
            .graph()
            .constant_references()
            .values()
            .find_map(|r| {
                let name = context.graph().names().get(r.name_id()).unwrap();

                if context.graph().strings().get(name.str()).unwrap().as_str() == "Bar" {
                    Some(name)
                } else {
                    None
                }
            })
            .unwrap();

        assert!(
            matches!(reference_name, NameRef::Unresolved(_)),
            "Did not properly invalidate constant reference"
        );
        assert_declaration_references_count_eq!(context, "Foo::Bar", 0);
        let empty_ancestors: [&str; 0] = [];
        assert_ancestors_eq!(context, "Foo::Bar::Baz::Qux", empty_ancestors);
    }

    #[test]
    fn deleting_a_file_invalidates_ancestors() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            r"
            module Foo
              CONST = 1
            end

            class Bar
              CONST
            end
            ",
        );
        context.index_uri(
            "file:///bar.rb",
            r"
            class Bar
              include Foo
            end
            ",
        );
        context.resolve();

        // Initially, `CONST` points to `Foo::CONST`
        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:6:3-6:8");
        assert_declaration_references_count_eq!(context, "Foo::CONST", 1);
        assert_ancestors_eq!(context, "Bar", ["Bar", "Foo", "Object"]);

        // After deleting the `bar.rb` file, the ancestors of `Bar` are invalidated and so should the `CONST` reference
        context.delete_uri("file:///bar.rb");

        // Verify that indexing the new file unresolves the constant reference
        let reference_name = context
            .graph()
            .constant_references()
            .values()
            .find_map(|r| {
                let name = context.graph().names().get(r.name_id()).unwrap();

                if context.graph().strings().get(name.str()).unwrap().as_str() == "CONST" {
                    Some(name)
                } else {
                    None
                }
            })
            .unwrap();

        assert!(
            matches!(reference_name, NameRef::Unresolved(_)),
            "Did not properly invalidate constant reference"
        );
        assert_declaration_references_count_eq!(context, "Foo::CONST", 0);
        let empty_ancestors: [&str; 0] = [];
        assert_ancestors_eq!(context, "Bar", empty_ancestors);
    }

    #[test]
    fn invalidating_constant_aliases() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            r"
            module Foo
              CONST = 1
            end

            class Bar
              ALIAS_CONST = CONST
            end
            ",
        );
        context.index_uri(
            "file:///bar.rb",
            r"
            class Bar
              include Foo
            end
            ",
        );
        context.resolve();

        // Initially, `ALIAS_CONST` targets `Foo::CONST`
        assert_alias_targets_contain!(context, "Bar::ALIAS_CONST", "Foo::CONST");

        // After deleting the `bar.rb` file, the ancestors of `Bar` are invalidated and so should the `CONST` reference
        context.delete_uri("file:///bar.rb");

        assert_no_constant_alias_target!(context, "Bar::ALIAS_CONST");
    }

    #[test]
    fn new_constant_in_existing_chain_invalidates_references() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            r"
            module Foo
              CONST = 1
            end

            module Bar
            end
            ",
        );
        context.index_uri(
            "file:///foo2.rb",
            r"
            class Baz
              include Foo
              prepend Bar

              CONST
            end
            ",
        );
        context.resolve();

        // Initially, CONST points to `Foo::CONST`
        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo2.rb:5:3-5:8");
        assert_declaration_references_count_eq!(context, "Foo::CONST", 1);

        // Now, we define a new constant earlier in the ancestor chain and `CONST` needs to be invalidated since it will
        // point to `Bar::CONST`
        context.index_uri(
            "file:///foo3.rb",
            r"
            module Bar
              CONST = 2
            end
            ",
        );

        // Verify that indexing the new file unresolves the constant reference
        let reference_name = context
            .graph()
            .constant_references()
            .values()
            .find_map(|r| {
                let name = context.graph().names().get(r.name_id()).unwrap();

                if context.graph().strings().get(name.str()).unwrap().as_str() == "CONST" {
                    Some(name)
                } else {
                    None
                }
            })
            .unwrap();

        assert!(
            matches!(reference_name, NameRef::Unresolved(_)),
            "Did not properly invalidate constant reference"
        );
        assert_declaration_references_count_eq!(context, "Foo::CONST", 0);
    }

    #[test]
    fn superclass_change_invalidates_references() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///parent.rb",
            r"
            class Parent
              CONST = 1
            end

            class OtherParent
              CONST = 2
            end
            ",
        );
        context.index_uri(
            "file:///child.rb",
            r"
            class Child < Parent
              CONST
            end
            ",
        );
        context.resolve();

        // Initially, CONST resolves to Parent::CONST
        assert_constant_reference_to!(context, "Parent::CONST", "file:///child.rb:2:3-2:8");
        assert_declaration_references_count_eq!(context, "Parent::CONST", 1);

        // Re-index child.rb to change superclass from Parent to OtherParent
        context.index_uri(
            "file:///child.rb",
            r"
            class Child < OtherParent
              CONST
            end
            ",
        );

        // CONST should be unresolved after superclass change
        let reference_name = context
            .graph()
            .constant_references()
            .values()
            .find_map(|r| {
                let name = context.graph().names().get(r.name_id()).unwrap();

                if context.graph().strings().get(name.str()).unwrap().as_str() == "CONST" {
                    Some(name)
                } else {
                    None
                }
            })
            .unwrap();

        assert!(
            matches!(reference_name, NameRef::Unresolved(_)),
            "Did not properly invalidate constant reference after superclass change"
        );
        assert_declaration_references_count_eq!(context, "Parent::CONST", 0);
    }

    #[test]
    fn mixin_removal_invalidates_references() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///bar.rb",
            r"
            module Bar
              CONST = 1
            end
            ",
        );
        context.index_uri(
            "file:///foo.rb",
            r"
            class Foo
              include Bar
              CONST
            end
            ",
        );
        context.resolve();

        // Initially, CONST resolves to Bar::CONST
        assert_constant_reference_to!(context, "Bar::CONST", "file:///foo.rb:3:3-3:8");
        assert_declaration_references_count_eq!(context, "Bar::CONST", 1);

        // Re-index foo.rb without the include
        context.index_uri(
            "file:///foo.rb",
            r"
            class Foo
              CONST
            end
            ",
        );

        // CONST should be unresolved after mixin removal
        let reference_name = context
            .graph()
            .constant_references()
            .values()
            .find_map(|r| {
                let name = context.graph().names().get(r.name_id()).unwrap();

                if context.graph().strings().get(name.str()).unwrap().as_str() == "CONST" {
                    Some(name)
                } else {
                    None
                }
            })
            .unwrap();

        assert!(
            matches!(reference_name, NameRef::Unresolved(_)),
            "Did not properly invalidate constant reference after mixin removal"
        );
        assert_declaration_references_count_eq!(context, "Bar::CONST", 0);
    }

    #[test]
    fn transitive_ancestor_member_addition() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///c.rb",
            r"
            module C
            end
            ",
        );
        context.index_uri(
            "file:///b.rb",
            r"
            module B
              include C
            end
            ",
        );
        context.index_uri(
            "file:///a.rb",
            r"
            class A
              include B
              CONST
            end
            ",
        );
        context.resolve();

        // Initially, CONST is unresolved because no one defines it
        let reference_name = context
            .graph()
            .constant_references()
            .values()
            .find_map(|r| {
                let name = context.graph().names().get(r.name_id()).unwrap();

                if context.graph().strings().get(name.str()).unwrap().as_str() == "CONST" {
                    Some(name)
                } else {
                    None
                }
            })
            .unwrap();

        assert!(
            matches!(reference_name, NameRef::Unresolved(_)),
            "CONST should be unresolved initially"
        );

        // Add CONST to module C via a new file
        context.index_uri(
            "file:///c2.rb",
            r"
            module C
              CONST = 1
            end
            ",
        );

        // After adding the constant, the reference is still unresolved (invalidation only unresolves,
        // it doesn't re-resolve). But after calling resolve(), it should find C::CONST through
        // the transitive ancestor chain A -> B -> C.
        context.resolve();

        assert_constant_reference_to!(context, "C::CONST", "file:///a.rb:3:3-3:8");
        assert_declaration_references_count_eq!(context, "C::CONST", 1);
    }

    #[test]
    fn phase_a_and_b_interaction() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///m.rb",
            r"
            module M
            end
            ",
        );
        context.index_uri(
            "file:///n.rb",
            r"
            module N
            end
            ",
        );
        context.index_uri(
            "file:///a.rb",
            r"
            class A
              include M
              include N
              CONST
            end
            ",
        );
        context.resolve();

        // Initially, CONST is unresolved
        let reference_name = context
            .graph()
            .constant_references()
            .values()
            .find_map(|r| {
                let name = context.graph().names().get(r.name_id()).unwrap();

                if context.graph().strings().get(name.str()).unwrap().as_str() == "CONST" {
                    Some(name)
                } else {
                    None
                }
            })
            .unwrap();

        assert!(
            matches!(reference_name, NameRef::Unresolved(_)),
            "CONST should be unresolved initially"
        );

        // Index a new file that both changes ancestors (prepend SomeNewModule to A)
        // AND adds a new constant (CONST in N)
        context.index_uri(
            "file:///x.rb",
            r"
            class A
              prepend SomeNewModule
            end

            module N
              CONST = 1
            end

            module SomeNewModule
            end
            ",
        );

        // After resolve, CONST should resolve to N::CONST through A's ancestor chain
        context.resolve();

        assert_constant_reference_to!(context, "N::CONST", "file:///a.rb:4:3-4:8");
        assert_declaration_references_count_eq!(context, "N::CONST", 1);
    }

    #[test]
    fn mixin_removal_re_resolves_correctly() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///bar.rb",
            r"
            module Bar
              CONST = 1
            end
            ",
        );
        context.index_uri(
            "file:///foo.rb",
            r"
            class Foo
              include Bar
              CONST
            end
            ",
        );
        context.resolve();

        // Initially, CONST resolves to Bar::CONST
        assert_constant_reference_to!(context, "Bar::CONST", "file:///foo.rb:3:3-3:8");
        assert_declaration_references_count_eq!(context, "Bar::CONST", 1);

        // Re-index foo.rb without the include
        context.index_uri(
            "file:///foo.rb",
            r"
            class Foo
              CONST
            end
            ",
        );

        // After resolve, CONST should remain unresolved because Foo no longer includes Bar
        context.resolve();

        let reference_name = context
            .graph()
            .constant_references()
            .values()
            .find_map(|r| {
                let name = context.graph().names().get(r.name_id()).unwrap();

                if context.graph().strings().get(name.str()).unwrap().as_str() == "CONST" {
                    Some(name)
                } else {
                    None
                }
            })
            .unwrap();

        assert!(
            matches!(reference_name, NameRef::Unresolved(_)),
            "CONST should remain unresolved after re-resolution without the mixin"
        );
        assert_declaration_references_count_eq!(context, "Bar::CONST", 0);
    }

    #[test]
    fn ancestor_changes_re_resolve_correctly() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            r"
            module Foo
              CONST = 1
            end

            module Bar
              CONST = 2
            end
            ",
        );
        context.index_uri(
            "file:///foo2.rb",
            r"
            class Baz
              include Foo

              CONST
            end
            ",
        );
        context.resolve();

        // Initially, CONST points to Foo::CONST
        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo2.rb:4:3-4:8");
        assert_declaration_references_count_eq!(context, "Foo::CONST", 1);

        // Add a prepend of Bar to Baz, changing the ancestor chain
        context.index_uri(
            "file:///foo3.rb",
            r"
            class Baz
              prepend Bar
            end
            ",
        );

        // After resolve, CONST should now resolve to Bar::CONST since Bar is prepended
        // (prepended modules come before the class itself in the ancestor chain)
        context.resolve();

        assert_constant_reference_to!(context, "Bar::CONST", "file:///foo2.rb:4:3-4:8");
        assert_declaration_references_count_eq!(context, "Bar::CONST", 1);
        assert_declaration_references_count_eq!(context, "Foo::CONST", 0);
    }

    #[test]
    fn invalidation_cascade_from_reference_to_declaration() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            r"
            module Foo
              module Bar
                module Baz
                end
              end
            end
            ",
        );
        context.index_uri(
            "file:///foo2.rb",
            r"
            module Foo
              include Bar

              class Baz::Qux
              end
            end
            ",
        );
        context.resolve();

        // Initially, the class created is `Foo::Bar::Baz::Qux`, which is accessible via inheritance thanks to the
        // include
        assert_declaration_exists!(context, "Foo::Bar::Baz::Qux");

        // Add a new `Baz` that can be accessible through the lexical scope, which has priority over ancestor. This
        // invalidates the `Baz` reference and as consequence must invalidate `Qux` as well
        context.index_uri(
            "file:///foo3.rb",
            r"
            module Foo
              module Baz
              end
            end
            ",
        );

        assert_declaration_does_not_exist!(context, "Foo::Bar::Baz::Qux");
    }
}
