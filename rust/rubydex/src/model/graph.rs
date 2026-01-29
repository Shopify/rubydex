use std::collections::hash_map::Entry;
use std::sync::LazyLock;

use crate::diagnostic::Diagnostic;
use crate::indexing::local_graph::LocalGraph;
use crate::model::declaration::{Ancestor, Declaration, Namespace};
use crate::model::definitions::Definition;
use crate::model::document::Document;
use crate::model::encoding::Encoding;
use crate::model::identity_maps::{IdentityHashMap, IdentityHashSet};
use crate::model::ids::{DeclarationId, DefinitionId, NameId, ReferenceId, StringId, UriId};
use crate::model::name::{Name, NameRef, ParentScope, ResolvedName};
use crate::model::references::{ConstantReference, MethodRef};
use crate::model::snapshot::{DocumentSnapshot, find_affected_references};
use crate::model::string_ref::StringRef;
use crate::resolution::Resolver;
use crate::stats;

pub static OBJECT_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("Object"));
pub static MODULE_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("Module"));
pub static CLASS_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("Class"));

#[derive(Debug, Default)]
pub struct PendingChanges {
    pub old_snapshots: IdentityHashMap<UriId, DocumentSnapshot>,
    pub pending_definitions: Vec<DefinitionId>,
    pub pending_ref_ids: Vec<(ReferenceId, NameId)>,
    pub emptied_declarations: Vec<DeclarationId>,
    pub orphaned_definitions: Vec<DefinitionId>,
}

impl PendingChanges {
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.old_snapshots.is_empty() && self.pending_definitions.is_empty() && self.pending_ref_ids.is_empty()
    }

    pub fn clear(&mut self) {
        self.old_snapshots.clear();
        self.pending_definitions.clear();
        self.pending_ref_ids.clear();
        self.emptied_declarations.clear();
    }
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

    // Index mapping declarations to references nested within them (for incremental resolution)
    nesting_to_references: IdentityHashMap<DeclarationId, Vec<ReferenceId>>,

    /// Pending changes from indexing that need to be processed during resolution.
    pending_changes: PendingChanges,

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
            nesting_to_references: IdentityHashMap::default(),
            pending_changes: PendingChanges::default(),
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

    pub fn add_declaration<F>(&mut self, declaration_id: DeclarationId, definition_id: DefinitionId, constructor: F)
    where
        F: FnOnce() -> Declaration,
    {
        let declaration = self.declarations.entry(declaration_id).or_insert_with(constructor);
        if !declaration.definitions().contains(&definition_id) {
            declaration.add_definition(definition_id);
        }
    }

    pub fn clear_declarations(&mut self) {
        self.declarations.clear();

        for name_ref in self.names.values_mut() {
            name_ref.reset_to_unresolved();
        }
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
            Definition::GlobalVariable(it) => {
                let nesting_definition = it
                    .lexical_nesting_id()
                    .and_then(|id| self.definitions().get(&id).unwrap().name_id());
                (nesting_definition, it.str_id())
            }
            Definition::GlobalVariableAlias(it) => {
                let nesting_definition = it
                    .lexical_nesting_id()
                    .and_then(|id| self.definitions().get(&id).unwrap().name_id());
                (nesting_definition, it.new_name_str_id())
            }
            Definition::InstanceVariable(it) => {
                let nesting_definition = it
                    .lexical_nesting_id()
                    .and_then(|id| self.definitions().get(&id).unwrap().name_id());
                (nesting_definition, it.str_id())
            }
            Definition::ClassVariable(it) => {
                let nesting_definition = it
                    .lexical_nesting_id()
                    .and_then(|id| self.definitions().get(&id).unwrap().name_id());
                (nesting_definition, it.str_id())
            }
            Definition::AttrAccessor(it) => {
                let nesting_definition = it
                    .lexical_nesting_id()
                    .and_then(|id| self.definitions().get(&id).unwrap().name_id());
                (nesting_definition, it.str_id())
            }
            Definition::AttrReader(it) => {
                let nesting_definition = it
                    .lexical_nesting_id()
                    .and_then(|id| self.definitions().get(&id).unwrap().name_id());
                (nesting_definition, it.str_id())
            }
            Definition::AttrWriter(it) => {
                let nesting_definition = it
                    .lexical_nesting_id()
                    .and_then(|id| self.definitions().get(&id).unwrap().name_id());
                (nesting_definition, it.str_id())
            }
            Definition::Method(it) => {
                let nesting_definition = it
                    .lexical_nesting_id()
                    .and_then(|id| self.definitions().get(&id).unwrap().name_id());
                (nesting_definition, it.str_id())
            }
            Definition::MethodAlias(it) => {
                let nesting_definition = it
                    .lexical_nesting_id()
                    .and_then(|id| self.definitions().get(&id).unwrap().name_id());
                (nesting_definition, it.new_name_str_id())
            }
        };

        let nesting_declaration_id = match nesting_name_id {
            Some(name_id) => self.name_id_to_declaration_id(*name_id),
            None => Some(&*OBJECT_ID),
        }?;

        self.declarations
            .get(nesting_declaration_id)
            .unwrap()
            .as_namespace()
            .unwrap()
            .member(member_str_id)
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

    /// Returns the nesting-to-references index
    #[must_use]
    pub fn nesting_to_references(&self) -> &IdentityHashMap<DeclarationId, Vec<ReferenceId>> {
        &self.nesting_to_references
    }

    #[must_use]
    pub fn has_pending_changes(&self) -> bool {
        !self.pending_changes.is_empty()
    }

    #[must_use]
    pub fn pending_changes(&self) -> &PendingChanges {
        &self.pending_changes
    }

    pub fn pending_changes_mut(&mut self) -> &mut PendingChanges {
        &mut self.pending_changes
    }

    pub fn clear_pending_changes(&mut self) {
        self.pending_changes.clear();
    }

    pub fn add_reference_to_nesting_index(&mut self, ref_id: ReferenceId, name_id: &NameId) {
        let Some(name_ref) = self.names.get(name_id) else {
            return;
        };

        let mut current_nesting = *name_ref.nesting();
        while let Some(nesting_name_id) = current_nesting {
            if let Some(NameRef::Resolved(resolved)) = self.names.get(&nesting_name_id) {
                let decl_id = *resolved.declaration_id();
                self.nesting_to_references.entry(decl_id).or_default().push(ref_id);
            }

            if let Some(nesting_name_ref) = self.names.get(&nesting_name_id) {
                current_nesting = *nesting_name_ref.nesting();
            } else {
                break;
            }
        }
    }

    /// Removes a reference from the nesting index.
    pub fn remove_reference_from_nesting_index(&mut self, ref_id: ReferenceId, name_id: &NameId) {
        let Some(name_ref) = self.names.get(name_id) else {
            return;
        };

        let mut current_nesting = *name_ref.nesting();
        while let Some(nesting_name_id) = current_nesting {
            if let Some(NameRef::Resolved(resolved)) = self.names.get(&nesting_name_id) {
                let decl_id = *resolved.declaration_id();
                if let Some(refs) = self.nesting_to_references.get_mut(&decl_id) {
                    refs.retain(|&id| id != ref_id);
                }
            }

            if let Some(nesting_name_ref) = self.names.get(&nesting_name_id) {
                current_nesting = *nesting_name_ref.nesting();
            } else {
                break;
            }
        }
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

    pub fn unresolve_name(&mut self, name_id: NameId) {
        if let Some(name_ref) = self.names.get_mut(&name_id)
            && let NameRef::Resolved(resolved) = name_ref
        {
            *name_ref = NameRef::Unresolved(Box::new(resolved.name().clone()));
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
        self.remove_definitions_for_uri(uri_id, true);
        self.documents.remove(&uri_id);
    }

    /// Merges everything in `other` into this Graph. This method is meant to merge all graph representations from
    /// different threads, but not meant to handle updates to the existing global representation
    pub fn extend(&mut self, local_graph: LocalGraph) {
        let (uri_id, document, definitions, strings, names, constant_references, method_references) =
            local_graph.into_parts();

        self.documents.insert(uri_id, document);
        self.definitions.extend(definitions);
        for (string_id, string_ref) in strings {
            match self.strings.entry(string_id) {
                Entry::Occupied(mut entry) => {
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
                    entry.get_mut().increment_ref_count(name_ref.ref_count());
                }
                Entry::Vacant(entry) => {
                    entry.insert(name_ref);
                }
            }
        }
        self.constant_references.extend(constant_references);
        self.method_references.extend(method_references);
    }

    pub fn update(&mut self, other: LocalGraph) {
        let uri_id = other.uri_id();

        if !self.pending_changes.old_snapshots.contains_key(&uri_id) {
            let snapshot = DocumentSnapshot::capture(self, uri_id);
            self.pending_changes.old_snapshots.insert(uri_id, snapshot);
        }

        self.pending_changes
            .pending_definitions
            .extend(other.definitions().keys().copied());
        self.pending_changes.pending_ref_ids.extend(
            other
                .constant_references()
                .iter()
                .map(|(&ref_id, r)| (ref_id, *r.name_id())),
        );

        self.remove_definitions_for_uri(uri_id, false);
        self.extend(other);
    }

    pub fn resolve_incremental(&mut self) -> usize {
        if self.pending_changes.is_empty() && self.pending_changes.orphaned_definitions.is_empty() {
            return 0;
        }

        let mut definitions_to_process: Vec<_> = self.pending_changes.pending_definitions.drain(..).collect();
        let previous_orphan_count = self.pending_changes.orphaned_definitions.len();
        definitions_to_process.append(&mut self.pending_changes.orphaned_definitions);

        let orphaned = {
            let mut resolver = Resolver::new(self);
            resolver.resolve_definitions(&definitions_to_process)
        };

        let orphans_were_resolved = orphaned.len() < previous_orphan_count;
        self.pending_changes.orphaned_definitions = orphaned;

        let emptied_declarations: Vec<_> = self.pending_changes.emptied_declarations.drain(..).collect();
        for decl_id in emptied_declarations {
            if let Some(decl) = self.declarations.get(&decl_id)
                && decl.has_no_definitions()
            {
                for name_ref in self.names.values_mut() {
                    if let NameRef::Resolved(resolved) = name_ref
                        && resolved.declaration_id() == &decl_id
                    {
                        name_ref.reset_to_unresolved();
                    }
                }
                let owner_id = *decl.owner_id();
                let unqualified_str_id = StringId::from(&decl.unqualified_name());
                if let Some(owner) = self.declarations.get_mut(&owner_id) {
                    match owner {
                        Declaration::Namespace(Namespace::Class(owner)) => {
                            owner.remove_member(&unqualified_str_id);
                        }
                        Declaration::Namespace(Namespace::SingletonClass(owner)) => {
                            owner.remove_member(&unqualified_str_id);
                        }
                        Declaration::Namespace(Namespace::Module(owner)) => {
                            owner.remove_member(&unqualified_str_id);
                        }
                        _ => {}
                    }
                }
                self.declarations.remove(&decl_id);
            }
        }

        let pending_ref_ids: Vec<_> = self.pending_changes.pending_ref_ids.drain(..).collect();
        for (ref_id, name_id) in &pending_ref_ids {
            self.add_reference_to_nesting_index(*ref_id, name_id);
        }

        let mut affected: IdentityHashSet<ReferenceId> = IdentityHashSet::default();
        let mut ancestors_to_invalidate: Vec<DeclarationId> = Vec::new();

        let old_snapshots: IdentityHashMap<_, _> = self.pending_changes.old_snapshots.drain().collect();
        for (uri_id, old_snapshot) in old_snapshots {
            let new_snapshot = DocumentSnapshot::capture(self, uri_id);
            let diff = old_snapshot.diff(&new_snapshot);

            ancestors_to_invalidate.extend(diff.changed_ancestors.iter().copied());
            affected.extend(find_affected_references(self, &diff, self.nesting_to_references()));

            if let Some(document) = self.documents.get(&uri_id) {
                for &ref_id in document.constant_references() {
                    if let Some(const_ref) = self.constant_references.get(&ref_id) {
                        let name_id = *const_ref.name_id();
                        if let Some(NameRef::Unresolved(_)) = self.names.get(&name_id) {
                            affected.insert(ref_id);
                        }
                    }
                }
            }
        }

        if orphans_were_resolved {
            for (&ref_id, const_ref) in &self.constant_references {
                let name_id = *const_ref.name_id();
                if let Some(NameRef::Unresolved(_)) = self.names.get(&name_id) {
                    affected.insert(ref_id);
                }
            }
        }

        if !ancestors_to_invalidate.is_empty() {
            let mut all_to_invalidate = ancestors_to_invalidate.clone();
            for decl_id in &ancestors_to_invalidate {
                if let Some(decl) = self.declarations.get(decl_id)
                    && let Some(namespace) = decl.as_namespace()
                    && let Some(singleton_id) = namespace.singleton_class()
                {
                    all_to_invalidate.push(*singleton_id);
                }
            }
            self.invalidate_ancestor_chains(all_to_invalidate);
        }

        let count = affected.len();

        let mut resolver = Resolver::new(self);
        resolver.resolve_references(&affected);

        count
    }

    pub fn incremental_update(&mut self, other: LocalGraph) -> usize {
        self.update(other);
        self.resolve_incremental()
    }

    #[allow(clippy::too_many_lines)]
    fn remove_definitions_for_uri(&mut self, uri_id: UriId, invalidate_ancestors: bool) {
        let Some(document) = self.documents.remove(&uri_id) else {
            return;
        };

        for ref_id in document.method_references() {
            if let Some(method_ref) = self.method_references.remove(ref_id) {
                self.untrack_string(*method_ref.str());
            }
        }

        for ref_id in document.constant_references() {
            if let Some(constant_ref) = self.constant_references.remove(ref_id) {
                let name_id = *constant_ref.name_id();
                self.remove_reference_from_nesting_index(*ref_id, &name_id);
                if let Some(NameRef::Resolved(resolved)) = self.names.get(&name_id)
                    && let Some(declaration) = self.declarations.get_mut(resolved.declaration_id())
                {
                    declaration.remove_reference(ref_id);
                }
                self.untrack_name(name_id);
            }
        }

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
                    if invalidate_ancestors {
                        let unqualified_str_id = StringId::from(&declaration.unqualified_name());
                        members_to_delete.push((*declaration.owner_id(), unqualified_str_id));
                        declarations_to_delete.push(declaration_id);

                        if let Some(namespace) = declaration.as_namespace()
                            && let Some(singleton_id) = namespace.singleton_class()
                        {
                            declarations_to_delete.push(*singleton_id);
                        }
                    } else {
                        self.pending_changes.emptied_declarations.push(declaration_id);
                    }
                }
            }

            if let Some(name_id) = self.definitions.get(def_id).unwrap().name_id() {
                self.untrack_name(*name_id);
            }
        }

        if invalidate_ancestors {
            self.invalidate_ancestor_chains(declarations_to_invalidate_ancestor_chains);

            // Cascade deletion: when a namespace is deleted, its members become orphans.
            // Collect all declarations that need to be deleted (including cascaded members).
            let mut all_to_delete: Vec<DeclarationId> = Vec::new();
            let mut queue = declarations_to_delete.clone();
            while let Some(decl_id) = queue.pop() {
                if all_to_delete.contains(&decl_id) {
                    continue;
                }
                all_to_delete.push(decl_id);
                if let Some(decl) = self.declarations.get(&decl_id)
                    && let Some(namespace) = decl.as_namespace()
                {
                    for member_id in namespace.members().values() {
                        queue.push(*member_id);
                    }
                }
            }

            for declaration_id in &all_to_delete {
                for name_ref in self.names.values_mut() {
                    if let NameRef::Resolved(resolved) = name_ref
                        && resolved.declaration_id() == declaration_id
                    {
                        name_ref.reset_to_unresolved();
                    }
                }
            }

            for declaration_id in all_to_delete {
                self.declarations.remove(&declaration_id);
            }
        } else {
            for declaration_id in declarations_to_delete {
                self.declarations.remove(&declaration_id);
            }
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

    fn invalidate_ancestor_chains(&mut self, initial_ids: Vec<DeclarationId>) {
        let mut queue = initial_ids;
        let mut visited = IdentityHashSet::<DeclarationId>::default();

        while let Some(declaration_id) = queue.pop() {
            if !visited.insert(declaration_id) {
                continue;
            }

            let namespace = self
                .declarations_mut()
                .get_mut(&declaration_id)
                .unwrap()
                .as_namespace_mut()
                .expect("expected namespace declaration");

            for ancestor in &namespace.ancestors() {
                if let Ancestor::Complete(ancestor_id) = ancestor {
                    self.declarations_mut()
                        .get_mut(ancestor_id)
                        .unwrap()
                        .as_namespace_mut()
                        .unwrap()
                        .remove_descendant(&declaration_id);
                }
            }

            let namespace = self
                .declarations_mut()
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
        use std::collections::HashMap;

        let mut declarations_with_docs = 0;
        let mut total_doc_size = 0;
        let mut declarations_types: HashMap<&str, usize> = HashMap::new();
        let mut definition_types: HashMap<&str, usize> = HashMap::new();
        let mut multi_definition_count = 0;

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
                if let Some(def) = self.definitions().get(def_id) {
                    *definition_types.entry(def.kind()).or_insert(0) += 1;
                }
            }
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

        println!();
        println!("Definition breakdown:");
        let mut types: Vec<_> = definition_types.iter().collect();
        types.sort_by_key(|(_, count)| std::cmp::Reverse(**count));
        for (kind, count) in types {
            println!("  {kind:20} {count:6}");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::comment::Comment;
    use crate::model::declaration::Ancestors;
    use crate::test_utils::GraphTest;
    use crate::{
        assert_constant_reference_to, assert_constant_reference_unresolved, assert_descendants, assert_members_eq,
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
        context.incremental_resolve();

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

        // Remove file a.rb content (removes the include Bar from Foo)
        context.index_uri("file:///a.rb", "");
        context.resolve();

        // After resolution, Baz should still have complete ancestors (rebuilt without the include)
        let baz_declaration = context.graph().declarations().get(&DeclarationId::from("Baz")).unwrap();
        assert!(matches!(
            baz_declaration.as_namespace().unwrap().ancestors(),
            Ancestors::Complete(_)
        ));

        // Foo no longer includes Bar, so Bar should not have Foo as a descendant
        let Declaration::Namespace(Namespace::Module(bar)) =
            context.graph().declarations().get(&DeclarationId::from("Bar")).unwrap()
        else {
            panic!("Expected Bar to be a module");
        };
        assert!(!bar.descendants().contains(&DeclarationId::from("Foo")));

        // Baz still extends Foo
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
        context.incremental_resolve();

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
        // Removing the constant should not remove the method
        context.index_uri("file:///foo.rb", "");
        context.incremental_resolve();

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
    fn nesting_index_is_populated_after_resolution() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            r"
            module Foo
              Bar
            end
            ",
        );
        context.resolve();

        let foo_id = DeclarationId::from("Foo");
        let nesting_index = context.graph().nesting_to_references();

        assert!(nesting_index.contains_key(&foo_id));
        assert!(!nesting_index.get(&foo_id).unwrap().is_empty());
    }

    #[test]
    fn nesting_index_is_updated_on_document_removal() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            r"
            module Foo
              Bar
            end
            ",
        );
        context.resolve();

        let foo_id = DeclarationId::from("Foo");
        assert!(!context.graph().nesting_to_references().get(&foo_id).unwrap().is_empty());

        context.delete_uri("file:///foo.rb");

        let refs = context.graph().nesting_to_references().get(&foo_id);
        assert!(refs.is_none() || refs.unwrap().is_empty());
    }

    #[test]
    fn incremental_update_resolves_references() {
        let mut context = GraphTest::new();

        // Initial state: Foo module with a reference to Bar
        context.index_uri("file:///foo.rb", "module Foo; Bar; end");
        context.resolve();

        // Bar is unresolved (no declaration exists)
        assert!(
            context
                .graph()
                .declarations()
                .get(&DeclarationId::from("Foo::Bar"))
                .is_none()
        );

        // Add Bar definition via incremental update
        let source = "module Foo; class Bar; end; Bar; end";
        let mut indexer = crate::indexing::ruby_indexer::RubyIndexer::new("file:///foo.rb".to_string(), source);
        indexer.index();
        let local_graph = indexer.local_graph();

        context.graph_mut().incremental_update(local_graph);

        // Now Bar should exist and the reference should resolve to it
        assert!(
            context
                .graph()
                .declarations()
                .get(&DeclarationId::from("Foo::Bar"))
                .is_some()
        );

        let bar_decl = context
            .graph()
            .declarations()
            .get(&DeclarationId::from("Foo::Bar"))
            .unwrap();
        assert!(!bar_decl.references().is_empty());
    }

    // ===================================================================================
    // Incremental Resolution Scenarios
    // ===================================================================================
    //
    // These tests verify that incremental resolution correctly handles different
    // change scenarios. Each test follows the pattern:
    // 1. Set up initial state and resolve
    // 2. Make a change (add/remove/modify)
    // 3. Resolve again
    // 4. Verify the resolution is correct
    //
    // Scenarios are categorized as:
    // - OffsetOnly: Only byte offsets change (comments, whitespace)
    // - MemberChanged: Constants are added/removed in nesting or ancestors
    // - AncestorChanged: Inheritance/mixins change affecting resolution

    mod incremental_resolution {
        use super::*;

        // -------------------------------------------------------------------------------
        // Offset Only Scenarios
        // -------------------------------------------------------------------------------

        #[test]
        fn offset_only_preserves_resolution() {
            let mut context = GraphTest::new();

            context.index_uri("file:///foo.rb", {
                r"
                module Foo
                  BAR = 1
                end
                "
            });
            context.index_uri("file:///ref.rb", "Foo::BAR");
            context.resolve();

            assert_constant_reference_to!(context, "Foo::BAR", "file:///ref.rb:0:5-0:8");

            context.index_uri("file:///foo.rb", {
                r"
                # This is a comment that shifts offsets
                module Foo
                  # Another comment
                  BAR = 1
                end
                "
            });
            context.incremental_resolve();

            assert_constant_reference_to!(context, "Foo::BAR", "file:///ref.rb:0:5-0:8");
        }

        #[test]
        fn offset_only_with_multiple_definitions() {
            let mut context = GraphTest::new();

            context.index_uri("file:///foo.rb", {
                r"
                module Foo
                  A = 1
                  B = 2
                  C = 3
                end
                "
            });
            context.index_uri("file:///ref.rb", {
                r"
                Foo::A
                Foo::B
                Foo::C
                "
            });
            context.resolve();

            assert_constant_reference_to!(context, "Foo::A", "file:///ref.rb:0:5-0:6");
            assert_constant_reference_to!(context, "Foo::B", "file:///ref.rb:1:5-1:6");
            assert_constant_reference_to!(context, "Foo::C", "file:///ref.rb:2:5-2:6");

            context.index_uri("file:///foo.rb", {
                r"
                # Header comment
                module Foo
                  # Comment for A
                  A = 1

                  # Comment for B
                  B = 2

                  # Comment for C
                  C = 3
                end
                "
            });
            context.incremental_resolve();

            assert_constant_reference_to!(context, "Foo::A", "file:///ref.rb:0:5-0:6");
            assert_constant_reference_to!(context, "Foo::B", "file:///ref.rb:1:5-1:6");
            assert_constant_reference_to!(context, "Foo::C", "file:///ref.rb:2:5-2:6");
        }

        // -------------------------------------------------------------------------------
        // Member Added Scenarios
        // -------------------------------------------------------------------------------

        #[test]
        fn member_added_to_current_nesting() {
            let mut context = GraphTest::new();

            context.index_uri("file:///outer.rb", {
                r"
                module Outer
                  CONST = 1
                end
                "
            });
            context.index_uri("file:///inner.rb", {
                r"
                module Outer
                  module Inner
                    CONST
                  end
                end
                "
            });
            context.resolve();

            assert_constant_reference_to!(context, "Outer::CONST", "file:///inner.rb:2:4-2:9");

            context.index_uri("file:///inner.rb", {
                r"
                module Outer
                  module Inner
                    CONST = 2
                    CONST
                  end
                end
                "
            });
            context.incremental_resolve();

            assert_constant_reference_to!(context, "Outer::Inner::CONST", "file:///inner.rb:3:4-3:9");
        }

        #[test]
        fn member_added_to_parent_nesting() {
            let mut context = GraphTest::new();

            context.index_uri("file:///foo.rb", {
                r"
                module Foo
                  module Bar
                    CONST
                  end
                end
                "
            });
            context.resolve();

            assert_constant_reference_unresolved!(context, "file:///foo.rb:2:4-2:9");

            context.index_uri("file:///const.rb", {
                r"
                module Foo
                  CONST = 1
                end
                "
            });
            context.incremental_resolve();

            assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:2:4-2:9");
        }

        #[test]
        fn member_added_to_parent_class() {
            let mut context = GraphTest::new();

            context.index_uri("file:///parent.rb", "class Parent; end");
            context.index_uri("file:///child.rb", {
                r"
                class Child < Parent
                  INHERITED
                end
                "
            });
            context.resolve();

            assert_constant_reference_unresolved!(context, "file:///child.rb:1:2-1:11");

            context.index_uri("file:///parent.rb", {
                r"
                class Parent
                  INHERITED = 123
                end
                "
            });
            context.incremental_resolve();

            assert_constant_reference_to!(context, "Parent::INHERITED", "file:///child.rb:1:2-1:11");
        }

        #[test]
        fn member_added_shadows_outer() {
            let mut context = GraphTest::new();

            context.index_uri("file:///outer.rb", {
                r"
                module Foo
                  BAR = 1
                end
                "
            });
            context.index_uri("file:///ref.rb", {
                r"
                module Foo
                  module Qux
                    class Baz
                      BAR
                    end
                  end
                end
                "
            });
            context.resolve();

            assert_constant_reference_to!(context, "Foo::BAR", "file:///ref.rb:3:6-3:9");

            context.index_uri("file:///middle.rb", {
                r"
                module Foo
                  module Qux
                    BAR = 2
                  end
                end
                "
            });
            context.incremental_resolve();

            assert_constant_reference_to!(context, "Foo::Qux::BAR", "file:///ref.rb:3:6-3:9");
        }

        // -------------------------------------------------------------------------------
        // Member Removed Scenarios
        // -------------------------------------------------------------------------------

        #[test]
        fn member_removed_from_current_nesting() {
            let mut context = GraphTest::new();

            context.index_uri("file:///outer.rb", {
                r"
                module Foo
                  CONST = 1
                end
                "
            });
            context.index_uri("file:///inner.rb", {
                r"
                module Foo
                  module Bar
                    CONST = 2
                    CONST
                  end
                end
                "
            });
            context.resolve();

            assert_constant_reference_to!(context, "Foo::Bar::CONST", "file:///inner.rb:3:4-3:9");

            context.index_uri("file:///inner.rb", {
                r"
                module Foo
                  module Bar
                    CONST
                  end
                end
                "
            });
            context.incremental_resolve();

            assert_constant_reference_to!(context, "Foo::CONST", "file:///inner.rb:2:4-2:9");
        }

        #[test]
        fn member_removed_from_parent_class() {
            let mut context = GraphTest::new();

            context.index_uri("file:///parent.rb", {
                r"
                class Parent
                  CONST = 1
                end
                "
            });
            context.index_uri("file:///child.rb", {
                r"
                class Child < Parent
                  CONST
                end
                "
            });
            context.resolve();

            assert_constant_reference_to!(context, "Parent::CONST", "file:///child.rb:1:2-1:7");

            context.index_uri("file:///parent.rb", "class Parent; end");
            context.incremental_resolve();

            assert_constant_reference_unresolved!(context, "file:///child.rb:1:2-1:7");
        }

        #[test]
        fn member_removed_reveals_outer() {
            let mut context = GraphTest::new();

            context.index_uri("file:///outer.rb", {
                r"
                module Foo
                  BAR = 1
                end
                "
            });
            context.index_uri("file:///middle.rb", {
                r"
                module Foo
                  module Qux
                    BAR = 2
                  end
                end
                "
            });
            context.index_uri("file:///ref.rb", {
                r"
                module Foo
                  module Qux
                    class Baz
                      BAR
                    end
                  end
                end
                "
            });
            context.resolve();

            assert_constant_reference_to!(context, "Foo::Qux::BAR", "file:///ref.rb:3:6-3:9");

            context.index_uri("file:///middle.rb", {
                r"
                module Foo
                  module Qux
                  end
                end
                "
            });
            context.incremental_resolve();

            assert_constant_reference_to!(context, "Foo::BAR", "file:///ref.rb:3:6-3:9");
        }

        #[test]
        fn member_removed_via_file_deletion() {
            let mut context = GraphTest::new();

            context.index_uri("file:///foo.rb", {
                r"
                module Foo
                  class Bar
                    CONST = 1
                  end
                  BAZ = 2
                end
                "
            });
            context.index_uri("file:///ref.rb", {
                r"
                Foo::Bar::CONST
                Foo::BAZ
                "
            });
            context.resolve();

            assert_constant_reference_to!(context, "Foo::Bar::CONST", "file:///ref.rb:0:10-0:15");
            assert_constant_reference_to!(context, "Foo::BAZ", "file:///ref.rb:1:5-1:8");

            context.delete_uri("file:///foo.rb");
            context.incremental_resolve();

            assert_constant_reference_unresolved!(context, "file:///ref.rb:0:10-0:15");
            assert_constant_reference_unresolved!(context, "file:///ref.rb:1:5-1:8");
        }

        // -------------------------------------------------------------------------------
        // Ancestor Changed via Include
        // -------------------------------------------------------------------------------

        #[test]
        fn ancestors_changed_via_include_added() {
            let mut context = GraphTest::new();

            context.index_uri("file:///mixin.rb", {
                r"
                module Mixin
                  CONST = 42
                end
                "
            });
            context.index_uri("file:///foo.rb", {
                r"
                class Foo
                  CONST
                end
                "
            });
            context.resolve();

            assert_constant_reference_unresolved!(context, "file:///foo.rb:1:2-1:7");

            context.index_uri("file:///foo.rb", {
                r"
                class Foo
                  include Mixin
                  CONST
                end
                "
            });
            context.incremental_resolve();

            assert_constant_reference_to!(context, "Mixin::CONST", "file:///foo.rb:2:2-2:7");
        }

        #[test]
        fn ancestors_changed_via_include_removed() {
            let mut context = GraphTest::new();

            context.index_uri("file:///mixin.rb", {
                r"
                module Mixin
                  CONST = 42
                end
                "
            });
            context.index_uri("file:///foo.rb", {
                r"
                class Foo
                  include Mixin
                  CONST
                end
                "
            });
            context.resolve();

            assert_constant_reference_to!(context, "Mixin::CONST", "file:///foo.rb:2:2-2:7");

            context.index_uri("file:///foo.rb", {
                r"
                class Foo
                  CONST
                end
                "
            });
            context.incremental_resolve();

            assert_constant_reference_unresolved!(context, "file:///foo.rb:1:2-1:7");
        }

        #[test]
        fn ancestors_changed_via_include_in_separate_file() {
            let mut context = GraphTest::new();

            context.index_uri("file:///mixin.rb", {
                r"
                module Mixin
                  CONST = 42
                end
                "
            });
            context.index_uri("file:///foo.rb", {
                r"
                class Foo
                  CONST
                end
                "
            });
            context.resolve();

            assert_constant_reference_unresolved!(context, "file:///foo.rb:1:2-1:7");

            context.index_uri("file:///foo_mixin.rb", {
                r"
                class Foo
                  include Mixin
                end
                "
            });
            context.incremental_resolve();

            assert_constant_reference_to!(context, "Mixin::CONST", "file:///foo.rb:1:2-1:7");
        }

        #[test]
        fn ancestors_changed_via_include_swapped() {
            let mut context = GraphTest::new();

            context.index_uri("file:///mixin_a.rb", {
                r"
                module MixinA
                  CONST = 1
                end
                "
            });
            context.index_uri("file:///mixin_b.rb", {
                r"
                module MixinB
                  CONST = 2
                end
                "
            });
            context.index_uri("file:///foo.rb", {
                r"
                class Foo
                  include MixinA
                  CONST
                end
                "
            });
            context.resolve();

            assert_constant_reference_to!(context, "MixinA::CONST", "file:///foo.rb:2:2-2:7");

            context.index_uri("file:///foo.rb", {
                r"
                class Foo
                  include MixinB
                  CONST
                end
                "
            });
            context.incremental_resolve();

            assert_constant_reference_to!(context, "MixinB::CONST", "file:///foo.rb:2:2-2:7");
        }

        // -------------------------------------------------------------------------------
        // Ancestor Changed via Extend
        // -------------------------------------------------------------------------------

        #[test]
        fn ancestors_changed_via_extend() {
            let mut context = GraphTest::new();

            context.index_uri("file:///mixin.rb", {
                r"
                module Mixin
                  CONST = 1
                end
                "
            });
            context.index_uri("file:///foo.rb", {
                r"
                class Foo
                  class << self
                    CONST
                  end
                end
                "
            });
            context.resolve();

            assert_constant_reference_unresolved!(context, "file:///foo.rb:2:4-2:9");

            context.index_uri("file:///foo.rb", {
                r"
                class Foo
                  extend Mixin
                  class << self
                    CONST
                  end
                end
                "
            });
            context.incremental_resolve();

            assert_constant_reference_to!(context, "Mixin::CONST", "file:///foo.rb:3:4-3:9");
        }

        // -------------------------------------------------------------------------------
        // Ancestor Changed via Superclass
        // -------------------------------------------------------------------------------

        #[test]
        fn ancestors_changed_via_superclass_change() {
            let mut context = GraphTest::new();

            context.index_uri("file:///parents.rb", {
                r"
                class ParentA
                  CONST = 1
                end
                class ParentB
                  CONST = 2
                end
                "
            });
            context.index_uri("file:///child.rb", {
                r"
                class Child < ParentA
                  CONST
                end
                "
            });
            context.resolve();

            assert_constant_reference_to!(context, "ParentA::CONST", "file:///child.rb:1:2-1:7");

            context.index_uri("file:///child.rb", {
                r"
                class Child < ParentB
                  CONST
                end
                "
            });
            context.incremental_resolve();

            assert_constant_reference_to!(context, "ParentB::CONST", "file:///child.rb:1:2-1:7");
        }

        #[test]
        fn ancestors_changed_via_mixin_in_parent() {
            let mut context = GraphTest::new();

            context.index_uri("file:///mixin.rb", {
                r"
                module Mixin
                  CONST = 1
                end
                "
            });
            context.index_uri("file:///parent.rb", "class Parent; end");
            context.index_uri("file:///child.rb", {
                r"
                class Child < Parent
                  CONST
                end
                "
            });
            context.resolve();

            assert_constant_reference_unresolved!(context, "file:///child.rb:1:2-1:7");

            context.index_uri("file:///parent.rb", {
                r"
                class Parent
                  include Mixin
                end
                "
            });
            context.incremental_resolve();

            assert_constant_reference_to!(context, "Mixin::CONST", "file:///child.rb:1:2-1:7");
        }

        #[test]
        fn ancestors_changed_middle_of_hierarchy_shadows() {
            let mut context = GraphTest::new();

            context.index_uri("file:///grandparent.rb", {
                r"
                class Grandparent
                  CONST = 1
                end
                "
            });
            context.index_uri("file:///parent.rb", "class Parent < Grandparent; end");
            context.index_uri("file:///child.rb", {
                r"
                class Child < Parent
                  CONST
                end
                "
            });
            context.resolve();

            assert_constant_reference_to!(context, "Grandparent::CONST", "file:///child.rb:1:2-1:7");

            context.index_uri("file:///parent.rb", {
                r"
                class Parent < Grandparent
                  CONST = 2
                end
                "
            });
            context.incremental_resolve();

            assert_constant_reference_to!(context, "Parent::CONST", "file:///child.rb:1:2-1:7");
        }

        // -------------------------------------------------------------------------------
        // Complex Scenarios
        // -------------------------------------------------------------------------------

        #[test]
        fn deeply_nested_shadowing_changes() {
            let mut context = GraphTest::new();

            context.index_uri("file:///foo.rb", {
                r"
                CONST = 1

                module Foo
                  CONST = 2

                  module Bar
                    CONST = 3

                    module Baz
                      CONST
                    end
                  end
                end
                "
            });
            context.resolve();

            assert_constant_reference_to!(context, "Foo::Bar::CONST", "file:///foo.rb:9:6-9:11");

            context.index_uri("file:///foo.rb", {
                r"
                CONST = 1

                module Foo
                  CONST = 2

                  module Bar
                    module Baz
                      CONST
                    end
                  end
                end
                "
            });
            context.incremental_resolve();

            assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:7:6-7:11");

            context.index_uri("file:///foo.rb", {
                r"
                CONST = 1

                module Foo
                  module Bar
                    module Baz
                      CONST
                    end
                  end
                end
                "
            });
            context.incremental_resolve();

            assert_constant_reference_to!(context, "CONST", "file:///foo.rb:5:6-5:11");
        }

        #[test]
        fn multiple_references_all_update() {
            let mut context = GraphTest::new();

            context.index_uri("file:///foo.rb", {
                r"
                module Foo
                  CONST = 1
                end
                "
            });
            context.index_uri("file:///ref1.rb", {
                r"
                module Foo
                  CONST
                end
                "
            });
            context.index_uri("file:///ref2.rb", {
                r"
                module Foo
                  module Bar
                    CONST
                  end
                end
                "
            });
            context.resolve();

            assert_constant_reference_to!(context, "Foo::CONST", "file:///ref1.rb:1:2-1:7");
            assert_constant_reference_to!(context, "Foo::CONST", "file:///ref2.rb:2:4-2:9");

            context.index_uri("file:///bar_const.rb", {
                r"
                module Foo
                  module Bar
                    CONST = 2
                  end
                end
                "
            });
            context.incremental_resolve();

            assert_constant_reference_to!(context, "Foo::CONST", "file:///ref1.rb:1:2-1:7");
            assert_constant_reference_to!(context, "Foo::Bar::CONST", "file:///ref2.rb:2:4-2:9");
        }

        #[test]
        fn owner_added_resolves_orphaned_definition() {
            let mut context = GraphTest::new();

            context.index_uri("file:///bar.rb", {
                r"
                class Foo::Bar
                  CONST = 1
                end
                "
            });
            context.index_uri("file:///ref.rb", "Foo::Bar::CONST");
            context.resolve();

            assert!(
                context
                    .graph()
                    .declarations()
                    .get(&DeclarationId::from("Foo::Bar"))
                    .is_none()
            );
            assert_constant_reference_unresolved!(context, "file:///ref.rb:0:10-0:15");

            context.index_uri("file:///foo.rb", "module Foo; end");
            context.incremental_resolve();

            assert!(
                context
                    .graph()
                    .declarations()
                    .get(&DeclarationId::from("Foo::Bar"))
                    .is_some()
            );
            assert_constant_reference_to!(context, "Foo::Bar::CONST", "file:///ref.rb:0:10-0:15");
        }

        #[test]
        fn owner_removed_orphans_children() {
            let mut context = GraphTest::new();

            context.index_uri("file:///foo.rb", "module Foo; end");
            context.index_uri("file:///bar.rb", {
                r"
                class Foo::Bar
                  CONST = 1
                end
                "
            });
            context.index_uri("file:///ref.rb", "Foo::Bar::CONST");
            context.resolve();

            assert_constant_reference_to!(context, "Foo::Bar::CONST", "file:///ref.rb:0:10-0:15");

            context.delete_uri("file:///foo.rb");
            context.incremental_resolve();

            assert_constant_reference_unresolved!(context, "file:///ref.rb:0:10-0:15");
        }
    }
}
