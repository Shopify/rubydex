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
use crate::model::string_ref::StringRef;
use crate::stats;

/// An entity whose validity depends on a particular `NameId`.
/// Used as the value type in the `name_dependents` reverse index.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NameDependent {
    Definition(DefinitionId),
    Reference(ReferenceId),
    Name(NameId),
}

/// Items processed by the unified invalidation worklist.
enum InvalidationItem {
    /// A declaration whose ancestor chain is stale, or that has become empty and needs removal.
    Declaration(DeclarationId),
    /// A name whose dependencies may have changed, needing cascade or reference re-evaluation.
    Name(NameId),
}

pub static OBJECT_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("Object"));
pub static MODULE_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("Module"));
pub static CLASS_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("Class"));

/// A work item produced by graph mutations (update/delete) that needs resolution.
#[derive(Debug)]
pub enum Unit {
    /// A definition that defines a constant and might require resolution
    Definition(DefinitionId),
    /// A constant reference that needs to be resolved
    ConstantRef(ReferenceId),
    /// A declaration whose ancestors need re-linearization
    Ancestors(DeclarationId),
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

    /// The position encoding used for LSP line/column locations. Not related to the actual encoding of the file
    position_encoding: Encoding,

    /// Reverse index: for each declaration, which `NameId`s resolved to it.
    /// Used during incremental invalidation to find names that need unresolving when a declaration is removed.
    declaration_to_names: IdentityHashMap<DeclarationId, IdentityHashSet<NameId>>,

    /// Reverse index: for each `NameId`, what depends on it (definitions, references, and child names).
    /// Used during invalidation to efficiently find affected entities without scanning the full graph.
    name_dependents: IdentityHashMap<NameId, Vec<NameDependent>>,

    /// Accumulated work items from update/delete operations.
    /// Drained by `take_pending_work()` before resolution.
    pending_work: Vec<Unit>,
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
            position_encoding: Encoding::default(),
            declaration_to_names: IdentityHashMap::default(),
            name_dependents: IdentityHashMap::default(),
            pending_work: Vec::default(),
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

    /// # Panics
    ///
    /// Will panic if the `definition_id` is not registered in the graph
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

        let should_promote = self.declarations.get(&declaration_id).is_some_and(|existing| {
            matches!(existing, Declaration::Constant(_))
                && matches!(
                    self.definitions.get(&definition_id),
                    Some(Definition::Class(_) | Definition::Module(_) | Definition::SingletonClass(_))
                )
                && self.all_definitions_promotable(existing)
        });

        match self.declarations.entry(declaration_id) {
            Entry::Occupied(mut occupied_entry) => {
                debug_assert!(
                    occupied_entry.get().name() == fully_qualified_name,
                    "DeclarationId collision in global graph"
                );

                if should_promote {
                    let mut new_declaration = constructor(fully_qualified_name);
                    let removed_declaration = occupied_entry.remove();
                    new_declaration.as_namespace_mut().unwrap().extend(removed_declaration);
                    new_declaration.add_definition(definition_id);
                    self.declarations.insert(declaration_id, new_declaration);
                } else {
                    occupied_entry.get_mut().add_definition(definition_id);
                }
            }
            Entry::Vacant(vacant_entry) => {
                let mut declaration = constructor(fully_qualified_name);
                declaration.add_definition(definition_id);
                vacant_entry.insert(declaration);
            }
        }

        declaration_id
    }

    /// Checks if all constant definitions for a declaration have the PROMOTABLE flag set.
    /// Used to determine whether a constant can be promoted to a namespace.
    #[must_use]
    pub fn all_definitions_promotable(&self, declaration: &Declaration) -> bool {
        declaration
            .definitions()
            .iter()
            .all(|def_id| match self.definitions.get(def_id) {
                Some(Definition::Constant(c)) => c.flags().is_promotable(),
                _ => true,
            })
    }

    /// Promotes a `Declaration::Constant` to a namespace using the provided constructor. Transfers all definitions,
    /// references, and diagnostics from the old declaration.
    ///
    /// # Panics
    ///
    /// Will panic if the declaration ID doesn't exist
    pub fn promote_constant_to_namespace<F>(&mut self, declaration_id: DeclarationId, constructor: F)
    where
        F: FnOnce(String, DeclarationId) -> Declaration,
    {
        let old_decl = self.declarations.remove(&declaration_id).unwrap();
        let name = old_decl.name().to_string();
        let owner_id = *old_decl.owner_id();

        let mut new_decl = constructor(name, owner_id);
        new_decl.as_namespace_mut().unwrap().extend(old_decl);

        self.declarations.insert(declaration_id, new_decl);
    }

    #[must_use]
    pub fn is_namespace(&self, declaration_id: &DeclarationId) -> bool {
        self.declarations
            .get(declaration_id)
            .is_some_and(|decl| decl.as_namespace().is_some())
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

    /// Searches for the initial attached object for an arbitrarily nested singleton class.
    /// Walks up the owner chain until finding a non-singleton namespace.
    ///
    /// # Example
    /// For `Foo::<Foo>::<<Foo>>`, returns `Foo`
    ///
    /// # Panics
    ///
    /// Panics if we attached a singleton class to something that isn't a namespace
    #[must_use]
    pub fn attached_object<'a>(&'a self, maybe_singleton: &'a Namespace) -> &'a Namespace {
        let mut attached_object = maybe_singleton;

        while matches!(attached_object, Namespace::SingletonClass(_)) {
            attached_object = self
                .declarations
                .get(attached_object.owner_id())
                .unwrap()
                .as_namespace()
                .unwrap();
        }

        attached_object
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

    /// Drains the accumulated work items, returning them for use by the resolver.
    pub fn take_pending_work(&mut self) -> Vec<Unit> {
        std::mem::take(&mut self.pending_work)
    }

    /// Converts a `Resolved` `NameRef` back to `Unresolved`, preserving the original `Name` data.
    /// Returns the `DeclarationId` it was previously resolved to, if any.
    fn unresolve_name(&mut self, name_id: NameId) -> Option<DeclarationId> {
        let name_ref = self.names.get(&name_id)?;

        match name_ref {
            NameRef::Resolved(resolved) => {
                let declaration_id = *resolved.declaration_id();
                let name = resolved.name().clone();
                self.names.insert(name_id, NameRef::Unresolved(Box::new(name)));

                if let Some(name_set) = self.declaration_to_names.get_mut(&declaration_id) {
                    name_set.remove(&name_id);
                    if name_set.is_empty() {
                        self.declaration_to_names.remove(&declaration_id);
                    }
                }

                Some(declaration_id)
            }
            NameRef::Unresolved(_) => None,
        }
    }

    /// Unresolves a constant reference: removes it from the target declaration's reference set
    /// and unresolves its underlying name.
    fn unresolve_reference(&mut self, reference_id: ReferenceId) -> Option<DeclarationId> {
        let constant_ref = self.constant_references.get(&reference_id)?;
        let name_id = *constant_ref.name_id();

        if let Some(old_decl_id) = self.unresolve_name(name_id) {
            if let Some(declaration) = self.declarations.get_mut(&old_decl_id) {
                declaration.remove_reference(&reference_id);
            }
            Some(old_decl_id)
        } else {
            None
        }
    }

    /// Decrements the ref count for a name and removes it if the count reaches zero.
    ///
    /// This does not recursively untrack `parent_scope` or `nesting` names.
    pub fn untrack_name(&mut self, name_id: NameId) {
        if let Some(name_ref) = self.names.get_mut(&name_id) {
            let string_id = *name_ref.str();
            if !name_ref.decrement_ref_count() {
                self.remove_name(name_id);
            }
            self.untrack_string(string_id);
        }
    }

    /// Removes a name from the graph and cleans up all reverse indices that reference it.
    fn remove_name(&mut self, name_id: NameId) {
        if let Some(name_ref) = self.names.get(&name_id) {
            if let NameRef::Resolved(resolved) = name_ref {
                let declaration_id = *resolved.declaration_id();
                if let Some(name_set) = self.declaration_to_names.get_mut(&declaration_id) {
                    name_set.remove(&name_id);
                    if name_set.is_empty() {
                        self.declaration_to_names.remove(&declaration_id);
                    }
                }
            }

            let nesting_id = name_ref.nesting().as_ref().copied();
            let parent_scope_id = name_ref.parent_scope().as_ref().copied();

            // Remove this name from its nesting/parent_scope owners' dependent lists
            for dep_owner_id in [nesting_id, parent_scope_id].into_iter().flatten() {
                self.remove_name_dependent(dep_owner_id, NameDependent::Name(name_id));
            }
        }

        self.name_dependents.remove(&name_id);
        self.names.remove(&name_id);
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

                    self.declaration_to_names
                        .entry(declaration_id)
                        .or_default()
                        .insert(name_id);
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

    /// Handles the deletion of a document identified by `uri`.
    /// Returns the `UriId` of the removed document, or `None` if it didn't exist.
    /// Pending work is accumulated internally; drained by the resolver.
    pub fn delete_document(&mut self, uri: &str) -> Option<UriId> {
        let uri_id = UriId::from(uri);
        let document = self.documents.remove(&uri_id)?;
        let mut work = Vec::new();
        self.invalidate(Some(&document), None, &mut work);
        self.remove_document_data(&document);
        self.pending_work.extend(work);
        Some(uri_id)
    }

    /// Merges everything in `other` into this Graph. This method is meant to merge all graph representations from
    /// different threads, but not meant to handle updates to the existing global representation
    ///
    /// # Panics
    ///
    /// Panics if a name that was just inserted into the graph cannot be found when looking up
    /// its `parent_scope`.
    fn extend(&mut self, local_graph: LocalGraph, work: &mut Vec<Unit>) {
        let (uri_id, document, definitions, strings, names, constant_references, method_references, name_dependents) =
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

            work.push(Unit::Definition(definition_id));
        }

        for (constant_ref_id, constant_ref) in constant_references {
            work.push(Unit::ConstantRef(constant_ref_id));

            if self.constant_references.insert(constant_ref_id, constant_ref).is_some() {
                debug_assert!(false, "Constant ReferenceId collision in global graph");
            }
        }

        for (method_ref_id, method_ref) in method_references {
            if self.method_references.insert(method_ref_id, method_ref).is_some() {
                debug_assert!(false, "Method ReferenceId collision in global graph");
            }
        }

        for (name_id, deps) in name_dependents {
            let global_deps = self.name_dependents.entry(name_id).or_default();
            for dep in deps {
                if !global_deps.contains(&dep) {
                    global_deps.push(dep);
                }
            }
        }
    }

    /// Updates the global representation with the information contained in `other`, handling deletions, insertions and
    /// updates to existing entries. Pending work is accumulated internally; drained by the resolver.
    ///
    /// The three steps must run in this order:
    /// 1. `invalidate` — reads resolved names and declaration state to determine what to invalidate
    /// 2. `remove_document_data` — removes old refs/defs/names/strings from maps
    /// 3. `extend` — merges the new `LocalGraph` into the now-clean graph
    pub fn update(&mut self, other: LocalGraph) {
        let uri_id = other.uri_id();
        let mut work = Vec::new();
        let old_document = self.documents.remove(&uri_id);

        self.invalidate(old_document.as_ref(), Some(&other), &mut work);
        if let Some(doc) = &old_document {
            self.remove_document_data(doc);
        }

        self.extend(other, &mut work);
        self.pending_work.extend(work);
    }

    /// Detaches old definitions from declarations, identifies declarations touched by the
    /// new `LocalGraph`, and feeds everything into `invalidate_graph` as a single worklist.
    ///
    /// Constant reference detachment is deferred to `remove_document_data`, which runs after
    /// invalidation. References unresolved during invalidation are already detached by
    /// `unresolve_reference`; the rest are still resolved and detached during cleanup.
    ///
    /// Does NOT remove raw data (refs/defs/names/strings) from maps — the caller must
    /// follow up with `remove_document_data` for that.
    fn invalidate(
        &mut self,
        old_document: Option<&Document>,
        new_local_graph: Option<&LocalGraph>,
        work: &mut Vec<Unit>,
    ) {
        let mut items: Vec<InvalidationItem> = Vec::new();

        // Detach old definitions from their declarations, queue affected directly
        if let Some(document) = old_document {
            for def_id in document.definitions() {
                if let Some(declaration_id) = self.definition_id_to_declaration_id(*def_id).copied()
                    && let Some(declaration) = self.declarations.get_mut(&declaration_id)
                    && declaration.remove_definition(def_id)
                {
                    declaration.clear_diagnostics();
                    items.push(InvalidationItem::Declaration(declaration_id));
                }
            }
        }

        // Declarations touched by the new local graph
        if let Some(lg) = new_local_graph {
            for def in lg.definitions().values() {
                if let Some(name_id) = def.name_id()
                    && let Some(NameRef::Resolved(resolved)) = self.names.get(name_id)
                {
                    items.push(InvalidationItem::Declaration(*resolved.declaration_id()));
                }
            }

            for cr in lg.constant_references().values() {
                if let Some(name_ref) = self.names.get(cr.name_id())
                    && let Some(nesting_id) = name_ref.nesting()
                    && let Some(NameRef::Resolved(resolved)) = self.names.get(nesting_id)
                {
                    items.push(InvalidationItem::Declaration(*resolved.declaration_id()));
                }
            }
        }

        if !items.is_empty() {
            self.invalidate_graph(items, work);
        }
    }

    /// Removes raw document data (refs, defs, names, strings) from maps.
    /// Does not touch declarations or perform invalidation — that is handled by `invalidate`.
    fn remove_document_data(&mut self, document: &Document) {
        for ref_id in document.method_references() {
            if let Some(method_ref) = self.method_references.remove(ref_id) {
                self.untrack_string(*method_ref.str());
            }
        }

        for ref_id in document.constant_references() {
            if let Some(constant_ref) = self.constant_references.remove(ref_id) {
                // Detach from target declaration. References unresolved during invalidation
                // were already detached by `unresolve_reference`; this catches the rest.
                if let Some(NameRef::Resolved(resolved)) = self.names.get(constant_ref.name_id())
                    && let Some(declaration) = self.declarations.get_mut(resolved.declaration_id())
                {
                    declaration.remove_reference(ref_id);
                }

                self.remove_name_dependent(*constant_ref.name_id(), NameDependent::Reference(*ref_id));
                self.untrack_name(*constant_ref.name_id());
            }
        }

        for def_id in document.definitions() {
            if let Some(name_id) = self.definitions.get(def_id).unwrap().name_id() {
                self.untrack_name(*name_id);
            }

            let definition = self.definitions.remove(def_id).unwrap();
            if let Some(name_id) = definition.name_id() {
                self.remove_name_dependent(*name_id, NameDependent::Definition(*def_id));
            }
            self.untrack_definition_strings(&definition);
        }
    }

    /// Removes a specific dependent from the `name_dependents` entry for `name_id`,
    /// cleaning up the entry if no dependents remain.
    fn remove_name_dependent(&mut self, name_id: NameId, dependent: NameDependent) {
        if let Some(deps) = self.name_dependents.get_mut(&name_id) {
            deps.retain(|d| *d != dependent);
            if deps.is_empty() {
                self.name_dependents.remove(&name_id);
            }
        }
    }

    /// Unified invalidation worklist. Processes declaration and name items in a single loop,
    /// where processing one item can push new items back onto the queue.
    fn invalidate_graph(&mut self, items: Vec<InvalidationItem>, work: &mut Vec<Unit>) {
        let mut queue = items;
        let mut visited_declarations = IdentityHashSet::<DeclarationId>::default();
        let mut visited_names = IdentityHashSet::<NameId>::default();

        while let Some(item) = queue.pop() {
            match item {
                InvalidationItem::Declaration(decl_id) => {
                    self.process_declaration(decl_id, &mut queue, &mut visited_declarations, work);
                }
                InvalidationItem::Name(name_id) => {
                    self.process_name(name_id, &mut queue, &mut visited_names, work);
                }
            }
        }
    }

    /// Processes a declaration in the invalidation worklist.
    ///
    /// A declaration should be removed if it has no definitions or if its owner has already
    /// been removed from the graph (orphaned). Otherwise, its ancestor chain is cleared and
    /// descendants are queued for the same treatment.
    fn process_declaration(
        &mut self,
        decl_id: DeclarationId,
        queue: &mut Vec<InvalidationItem>,
        visited_declarations: &mut IdentityHashSet<DeclarationId>,
        work: &mut Vec<Unit>,
    ) {
        let Some(decl) = self.declarations.get(&decl_id) else {
            return;
        };

        let should_remove = decl.has_no_definitions() || !self.declarations.contains_key(decl.owner_id());

        if should_remove {
            // Queue members + singleton for removal
            if let Some(ns) = decl.as_namespace() {
                if let Some(singleton_id) = ns.singleton_class() {
                    queue.push(InvalidationItem::Declaration(*singleton_id));
                }
                for member_decl_id in ns.members().values() {
                    queue.push(InvalidationItem::Declaration(*member_decl_id));
                }
                for descendant_id in ns.descendants() {
                    queue.push(InvalidationItem::Declaration(*descendant_id));
                }
            }

            // Unresolve names resolved to this declaration, queue their Name dependents
            if let Some(name_set) = self.declaration_to_names.remove(&decl_id) {
                for name_id in name_set {
                    self.unresolve_name(name_id);
                    if let Some(deps) = self.name_dependents.get(&name_id) {
                        for dep in deps {
                            if let NameDependent::Name(dep_name_id) = dep {
                                queue.push(InvalidationItem::Name(*dep_name_id));
                            }
                        }
                    }
                }
            }

            // Clean up owner membership and queue remaining definitions for re-resolution
            if let Some(decl) = self.declarations.get(&decl_id) {
                let unqualified_str_id = StringId::from(&decl.unqualified_name());
                let owner_id = *decl.owner_id();

                for def_id in decl.definitions() {
                    work.push(Unit::Definition(*def_id));
                }

                if let Some(owner) = self.declarations.get_mut(&owner_id)
                    && let Some(ns) = owner.as_namespace_mut()
                {
                    ns.remove_member(&unqualified_str_id);
                }
            }

            self.declarations.remove(&decl_id);
        } else {
            // Ancestor-stale mode
            if !visited_declarations.insert(decl_id) {
                return;
            }

            let Some(namespace) = self.declarations.get_mut(&decl_id).and_then(|d| d.as_namespace_mut()) else {
                return;
            };

            // Remove self from each ancestor's descendant set
            for ancestor in &namespace.clone_ancestors() {
                if let Ancestor::Complete(ancestor_id) = ancestor
                    && let Some(anc_decl) = self.declarations.get_mut(ancestor_id)
                    && let Some(ns) = anc_decl.as_namespace_mut()
                {
                    ns.remove_descendant(&decl_id);
                }
            }

            let namespace = self.declarations.get_mut(&decl_id).unwrap().as_namespace_mut().unwrap();

            namespace.for_each_descendant(|descendant_id| {
                queue.push(InvalidationItem::Declaration(*descendant_id));
            });

            namespace.clear_ancestors();
            namespace.clear_descendants();

            work.push(Unit::Ancestors(decl_id));

            // Queue Name dependents of resolved names (skipping seeds themselves)
            if let Some(name_set) = self.declaration_to_names.get(&decl_id) {
                for seed_name_id in name_set {
                    if let Some(deps) = self.name_dependents.get(seed_name_id) {
                        for dep in deps {
                            if let NameDependent::Name(dep_name_id) = dep {
                                queue.push(InvalidationItem::Name(*dep_name_id));
                            }
                        }
                    }
                }
            }
        }
    }

    /// Processes a name in the invalidation worklist.
    ///
    /// Always propagates to `name_dependents`. Then checks whether the name needs full
    /// structural cascade (nesting or parent scope dependency broken) or just reference
    /// re-evaluation (ancestor context changed).
    fn process_name(
        &mut self,
        name_id: NameId,
        queue: &mut Vec<InvalidationItem>,
        visited_names: &mut IdentityHashSet<NameId>,
        work: &mut Vec<Unit>,
    ) {
        if !visited_names.insert(name_id) {
            return;
        }

        let dependents: Vec<NameDependent> = self.name_dependents.get(&name_id).cloned().unwrap_or_default();

        // Always propagate to Name dependents
        for dep in &dependents {
            if let NameDependent::Name(dep_name_id) = dep {
                queue.push(InvalidationItem::Name(*dep_name_id));
            }
        }

        if self.has_unresolved_dependency(name_id) {
            // Structural cascade: the name's resolution is invalid
            if let Some(old_decl_id) = self.unresolve_name(name_id) {
                for dep in &dependents {
                    match dep {
                        NameDependent::Reference(ref_id) => {
                            if let Some(decl) = self.declarations.get_mut(&old_decl_id) {
                                decl.remove_reference(ref_id);
                            }
                            work.push(Unit::ConstantRef(*ref_id));
                        }
                        NameDependent::Definition(def_id) => {
                            work.push(Unit::Definition(*def_id));

                            if let Some(decl) = self.declarations.get_mut(&old_decl_id) {
                                decl.remove_definition(def_id);
                            }

                            if self
                                .declarations
                                .get(&old_decl_id)
                                .is_some_and(Declaration::has_no_definitions)
                            {
                                queue.push(InvalidationItem::Declaration(old_decl_id));
                            }
                        }
                        NameDependent::Name(_) => {} // already propagated above
                    }
                }
            }
        } else {
            // Ancestor change only: re-evaluate constant references under this name
            let is_resolved = matches!(self.names.get(&name_id), Some(NameRef::Resolved(_)));

            for dep in &dependents {
                if let NameDependent::Reference(ref_id) = dep {
                    if is_resolved {
                        self.unresolve_reference(*ref_id);
                    }
                    work.push(Unit::ConstantRef(*ref_id));
                }
            }
        }
    }

    /// Returns true if the name's nesting or parent scope dependency has been broken,
    /// meaning the name needs full structural cascade rather than just reference re-evaluation.
    fn has_unresolved_dependency(&self, name_id: NameId) -> bool {
        let Some(name_ref) = self.names.get(&name_id) else {
            return false;
        };

        // Nesting is unresolved or removed: the lexical scope this name lives in is invalid
        if let Some(nesting_id) = name_ref.nesting()
            && matches!(self.names.get(nesting_id), Some(NameRef::Unresolved(_)) | None)
        {
            return true;
        }

        // Parent scope is still resolved (pointing to a now-stale declaration) or was removed:
        // the qualifier (e.g. `Foo` in `Foo::Bar`) may resolve differently
        if let Some(parent_id) = name_ref.parent_scope().as_ref()
            && matches!(self.names.get(parent_id), Some(NameRef::Resolved(_)) | None)
        {
            return true;
        }

        false
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
    use crate::model::name::NameRef;
    use crate::test_utils::GraphTest;
    use crate::{
        assert_alias_targets_contain, assert_ancestors_eq, assert_constant_reference_to,
        assert_declaration_does_not_exist, assert_declaration_exists, assert_declaration_references_count_eq,
        assert_descendants, assert_members_eq, assert_no_constant_alias_target, assert_no_diagnostics,
        assert_no_members,
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

    // ==========================================
    // Incremental invalidation tests
    // ==========================================

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
    fn new_namespace_shadowing_include_target_invalidates_references() {
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

        assert_constant_reference_to!(context, "Foo::Bar", "file:///qux.rb:5:17-5:20");
        assert_declaration_references_count_eq!(context, "Foo::Bar", 1);
        assert_ancestors_eq!(
            context,
            "Foo::Bar::Baz::Qux",
            ["Foo::Bar::Baz::Qux", "Foo::Bar", "Object"]
        );

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
    fn deleting_include_file_invalidates_ancestors_and_references() {
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

        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:6:3-6:8");
        assert_declaration_references_count_eq!(context, "Foo::CONST", 1);
        assert_ancestors_eq!(context, "Bar", ["Bar", "Foo", "Object"]);

        context.delete_uri("file:///bar.rb");

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

        assert_alias_targets_contain!(context, "Bar::ALIAS_CONST", "Foo::CONST");

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

        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo2.rb:5:3-5:8");
        assert_declaration_references_count_eq!(context, "Foo::CONST", 1);

        context.index_uri(
            "file:///foo3.rb",
            r"
            module Bar
              CONST = 2
            end
            ",
        );

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

        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo2.rb:4:3-4:8");
        assert_declaration_references_count_eq!(context, "Foo::CONST", 1);

        context.index_uri(
            "file:///foo3.rb",
            r"
            class Baz
              prepend Bar
            end
            ",
        );

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

        assert_declaration_exists!(context, "Foo::Bar::Baz::Qux");

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

    // ==========================================
    // Edge case tests for robustness
    // ==========================================

    #[test]
    fn multiple_definitions_one_removed_declaration_survives() {
        let mut context = GraphTest::new();

        context.index_uri("file:///a.rb", "module Foo; end");
        context.index_uri("file:///b.rb", "module Foo; end");
        context.resolve();

        assert_declaration_exists!(context, "Foo");
        assert_eq!(context.graph().get("Foo").unwrap().len(), 2);

        context.delete_uri("file:///a.rb");
        assert_declaration_exists!(context, "Foo");
        assert_eq!(context.graph().get("Foo").unwrap().len(), 1);
    }

    #[test]
    fn re_indexing_same_content_preserves_state() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            r"
            module Foo
              CONST = 1
            end
            ",
        );
        context.index_uri(
            "file:///bar.rb",
            r"
            class Bar
              include Foo
              CONST
            end
            ",
        );
        context.resolve();

        assert_constant_reference_to!(context, "Foo::CONST", "file:///bar.rb:3:3-3:8");
        assert_ancestors_eq!(context, "Bar", ["Bar", "Foo", "Object"]);

        context.index_uri(
            "file:///bar.rb",
            r"
            class Bar
              include Foo
              CONST
            end
            ",
        );
        context.resolve();
        assert_constant_reference_to!(context, "Foo::CONST", "file:///bar.rb:3:3-3:8");
        assert_ancestors_eq!(context, "Bar", ["Bar", "Foo", "Object"]);
    }

    #[test]
    fn incremental_resolve_after_delete_and_re_add() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            r"
            module Foo
              CONST = 1
            end
            ",
        );
        context.index_uri(
            "file:///bar.rb",
            r"
            class Bar
              include Foo
              CONST
            end
            ",
        );
        context.resolve();

        assert_constant_reference_to!(context, "Foo::CONST", "file:///bar.rb:3:3-3:8");

        context.delete_uri("file:///foo.rb");
        context.index_uri(
            "file:///foo.rb",
            r"
            module Foo
              CONST = 42
            end
            ",
        );

        context.resolve();
        assert_constant_reference_to!(context, "Foo::CONST", "file:///bar.rb:3:3-3:8");
    }

    #[test]
    fn deep_ancestor_chain_invalidation() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///a.rb",
            r"
            module A
              DEEP_CONST = 1
            end
            module B
              include A
            end
            module C
              include B
            end
            class D
              include C
              DEEP_CONST
            end
            ",
        );
        context.resolve();

        // DEEP_CONST resolves through D -> C -> B -> A
        assert_constant_reference_to!(context, "A::DEEP_CONST", "file:///a.rb:12:3-12:13");

        // Add a new file that changes C's ancestors
        context.index_uri(
            "file:///b.rb",
            r"
            module C
              prepend B
            end
            ",
        );

        let reference_name = context
            .graph()
            .constant_references()
            .values()
            .find_map(|r| {
                let name = context.graph().names().get(r.name_id()).unwrap();
                if context.graph().strings().get(name.str()).unwrap().as_str() == "DEEP_CONST" {
                    Some(name)
                } else {
                    None
                }
            })
            .unwrap();

        assert!(
            matches!(reference_name, NameRef::Unresolved(_)),
            "Deep ancestor chain should invalidate constant references"
        );
    }

    // ==========================================
    // Regression tests: recursive declaration cleanup
    // ==========================================

    #[test]
    fn removing_namespace_declaration_cleans_up_member_methods() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            r"
            class Foo
              def hello; end
              def world; end
            end
            ",
        );
        context.resolve();

        assert_declaration_exists!(context, "Foo");
        assert!(context.graph().get("Foo#hello()").is_some());
        assert!(context.graph().get("Foo#world()").is_some());

        context.delete_uri("file:///foo.rb");

        assert!(context.graph().get("Foo").is_none());
        assert!(context.graph().get("Foo#hello()").is_none());
        assert!(context.graph().get("Foo#world()").is_none());
    }

    #[test]
    fn removing_declaration_cascades_to_nested_members() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            r"
            module Outer
              class Inner
                CONST = 1
                def method_name; end
                module Nested; end
              end
            end
            ",
        );
        context.resolve();

        assert_declaration_exists!(context, "Outer");
        assert_declaration_exists!(context, "Outer::Inner");
        assert_declaration_exists!(context, "Outer::Inner::Nested");
        assert!(context.graph().get("Outer::Inner").is_some());

        context.delete_uri("file:///foo.rb");

        assert!(context.graph().get("Outer").is_none());
        assert!(context.graph().get("Outer::Inner").is_none());
        assert!(context.graph().get("Outer::Inner::Nested").is_none());
        assert!(context.graph().get("Outer::Inner#method_name()").is_none());
    }

    #[test]
    fn cascade_removes_declaration_with_singleton_and_members() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            r"
            module Foo
              module Bar
                class Baz
                  def self.class_method; end
                  CONST = 1
                end
              end
            end
            ",
        );
        context.index_uri(
            "file:///bar.rb",
            r"
            module Foo
              include Bar

              class Baz::Qux
                def instance_method; end
              end
            end
            ",
        );
        context.resolve();

        // Qux exists via inheritance (Foo includes Bar, so Baz is accessible)
        assert_declaration_exists!(context, "Foo::Bar::Baz::Qux");

        // Add a new Baz in Foo's lexical scope, invalidating the Baz reference
        context.index_uri(
            "file:///baz.rb",
            r"
            module Foo
              module Baz
              end
            end
            ",
        );

        // Qux and all its members should be gone (cascade from Baz reference invalidation)
        assert_declaration_does_not_exist!(context, "Foo::Bar::Baz::Qux");
        assert!(context.graph().get("Foo::Bar::Baz::Qux#instance_method()").is_none());
    }

    #[test]
    fn new_file_adding_superclass_invalidates_ancestors() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "class Foo; end");
        context.index_uri("file:///bar.rb", "module Bar; end");
        context.resolve();

        assert_ancestors_eq!(context, "Foo", ["Foo", "Object"]);

        // A new file reopens Foo with a superclass — ancestors must be invalidated
        context.index_uri(
            "file:///foo2.rb",
            r"
            class Foo < Bar
            end
            ",
        );

        let empty_ancestors: [&str; 0] = [];
        assert_ancestors_eq!(context, "Foo", empty_ancestors);

        // After re-resolve, Foo should now inherit from Bar
        // (Bar is a module, so Foo's chain is Foo → Bar → Object)
        context.resolve();
        // Bar is a module so its ancestors are empty; Foo gets [Foo, Bar, Object]
        // through the class parent resolution
        let foo_decl = context.graph().declarations().get(&DeclarationId::from("Foo")).unwrap();
        assert!(
            foo_decl.as_namespace().unwrap().has_complete_ancestors(),
            "Foo ancestors should be complete after re-resolve"
        );
    }

    #[test]
    fn adding_include_resolves_previously_unresolved_references() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            r"
            class Foo
              CONST
            end

            module Bar
              CONST = 1
            end
            ",
        );
        context.resolve();

        // CONST is unresolved (Foo doesn't include Bar yet, CONST not found)
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
        assert!(matches!(reference_name, NameRef::Unresolved(_)));

        context.index_uri(
            "file:///foo_include.rb",
            r"
            class Foo
              include Bar
            end
            ",
        );

        // After re-resolve, CONST should now resolve through Foo -> Bar
        context.resolve();
        assert_constant_reference_to!(context, "Bar::CONST", "file:///foo.rb:2:3-2:8");
        assert_ancestors_eq!(context, "Foo", ["Foo", "Bar", "Object"]);
    }

    #[test]
    fn deleting_file_with_include_invalidates_constant_references() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            r"
            class Foo
              CONST
            end

            module Bar
              CONST = 1
            end
            ",
        );
        context.index_uri(
            "file:///foo_include.rb",
            r"
            class Foo
              include Bar
            end
            ",
        );
        context.resolve();

        // CONST resolves through Foo -> Bar
        assert_constant_reference_to!(context, "Bar::CONST", "file:///foo.rb:2:3-2:8");

        // Delete the file that includes Bar — ancestors change, CONST should be invalidated
        context.delete_uri("file:///foo_include.rb");

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
            "Deleting include file should invalidate constant reference"
        );
        assert_declaration_references_count_eq!(context, "Bar::CONST", 0);
    }

    #[test]
    fn re_indexing_preserves_constant_singleton_classes() {
        let mut context = GraphTest::new();

        // `EXTENDED = GENERAL + %w[...]` calls `+` on GENERAL, which makes the resolver
        // create a singleton class for the GENERAL constant (to host the method receiver).
        context.index_uri(
            "file:///ns.rb",
            r"
            module Ns
              GENERAL = %w[nodoc].freeze
              EXTENDED = GENERAL + %w[arg args]
            end
            ",
        );

        context.index_uri(
            "file:///child.rb",
            r"
            class Ns::Child
              def name; end
            end
            ",
        );

        context.resolve();

        // Singleton class for the constant exists after initial resolve.
        // Ancestors include self, matching Ruby's singleton_class.ancestors behavior.
        assert_declaration_exists!(context, "Ns::GENERAL::<GENERAL>");
        assert_ancestors_eq!(
            context,
            "Ns::GENERAL::<GENERAL>",
            ["Ns::GENERAL::<GENERAL>", "Module", "Object"]
        );

        context.index_uri(
            "file:///child.rb",
            r"
            class Ns::Child
              def name; end
              def other; end
            end
            ",
        );
        context.resolve();

        assert_declaration_exists!(context, "Ns::GENERAL::<GENERAL>");
        assert_ancestors_eq!(
            context,
            "Ns::GENERAL::<GENERAL>",
            ["Ns::GENERAL::<GENERAL>", "Module", "Object"]
        );
    }

    #[test]
    fn re_indexing_module_invalidates_compact_class_inside_it() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            r"
            class Foo; end
            ",
        );

        context.index_uri(
            "file:///m.rb",
            r"
            module M
              class Foo::Bar
                def bar; end
              end
            end
            ",
        );

        context.resolve();

        assert_declaration_exists!(context, "Foo::Bar");
        assert_ancestors_eq!(context, "Foo::Bar", ["Foo::Bar", "Object"]);
        assert_members_eq!(context, "Foo::Bar", ["bar()"]);

        context.index_uri(
            "file:///m.rb",
            r"
            module M
              module Foo; end

              class Foo::Bar # Now the Foo in the class name resolves to M::Foo instead of the top-level Foo, changing the declaration's ancestors and members
                def bar; end
              end
            end
            ",
        );
        context.resolve();

        // Every declaration should be under M::Foo now
        assert_declaration_exists!(context, "M::Foo::Bar");
        assert_ancestors_eq!(context, "M::Foo::Bar", ["M::Foo::Bar", "Object"]);
        assert_members_eq!(context, "M::Foo::Bar", ["bar()"]);
    }

    #[test]
    fn invalidating_namespace_cascades_to_compact_class_and_its_members() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            r"
            class Foo
            end
            ",
        );

        context.index_uri(
            "file:///bar.rb",
            r"
            class Foo::Bar
              def bar; end
            end
            ",
        );

        context.resolve();

        assert_declaration_exists!(context, "Foo");
        assert_declaration_exists!(context, "Foo::Bar");
        assert_ancestors_eq!(context, "Foo::Bar", ["Foo::Bar", "Object"]);
        assert_members_eq!(context, "Foo", ["Bar"]);
        assert_members_eq!(context, "Foo::Bar", ["bar()"]);

        context.index_uri(
            "file:///foo.rb",
            r"
            class Baz; end

            Foo = Baz

            class Foo::Bar
              def bar; end
            end
            ",
        );
        context.resolve();

        assert_declaration_exists!(context, "Baz::Bar");
        assert_ancestors_eq!(context, "Baz", ["Baz", "Object"]);
        assert_ancestors_eq!(context, "Baz::Bar", ["Baz::Bar", "Object"]);
        assert_members_eq!(context, "Baz", ["Bar"]);
        assert_members_eq!(context, "Baz::Bar", ["bar()"]);
    }

    #[test]
    fn deleting_sole_definition_file_cascades_removal_through_nested_declarations() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///a.rb",
            r"
            module A
            end
            ",
        );

        context.index_uri(
            "file:///b.rb",
            r"
            module A::B
            end
            ",
        );

        context.index_uri(
            "file:///c.rb",
            r"
            class A::B::C
            end
            ",
        );
        context.resolve();

        assert_declaration_exists!(context, "A");
        assert_declaration_exists!(context, "A::B");
        assert_declaration_exists!(context, "A::B::C");

        context.delete_uri("file:///a.rb");

        assert!(context.graph().get("A").is_none());
        assert!(context.graph().get("A::B").is_none());
        assert_declaration_does_not_exist!(context, "A::B::C");

        context.resolve();
        assert!(context.graph().get("A").is_none());
        assert!(context.graph().get("A::B").is_none());
        assert!(context.graph().get("A::B::C").is_none());
    }
}
