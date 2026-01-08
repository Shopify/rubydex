use std::sync::{Mutex, MutexGuard, RwLock};

use crate::model::{
    identity_maps::{IdentityHashMap, IdentityHashSet},
    ids::{DeclarationId, DefinitionId, NameId, ReferenceId, StringId},
};

/// A single ancestor in the linearized ancestor chain
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ancestor {
    /// A complete ancestor that we have fully linearized
    Complete(DeclarationId),
    /// A partial ancestor that is missing linearization
    Partial(NameId),
}

/// The ancestor chain and its current state
#[derive(Debug, Clone)]
pub enum Ancestors {
    /// A complete linearization of ancestors with all parts resolved
    Complete(Vec<Ancestor>),
    /// A cyclic linearization of ancestors (e.g.: a module that includes itself)
    Cyclic(Vec<Ancestor>),
    /// A partial linearization of ancestors with some parts unresolved. This chain state always triggers retries
    Partial(Vec<Ancestor>),
}

impl Ancestors {
    pub fn iter(&self) -> std::slice::Iter<'_, Ancestor> {
        match self {
            Ancestors::Complete(ancestors) | Ancestors::Partial(ancestors) | Ancestors::Cyclic(ancestors) => {
                ancestors.iter()
            }
        }
    }

    #[must_use]
    pub fn to_partial(self) -> Self {
        match self {
            Ancestors::Complete(ancestors) | Ancestors::Cyclic(ancestors) | Ancestors::Partial(ancestors) => {
                Ancestors::Partial(ancestors)
            }
        }
    }
}

impl<'a> IntoIterator for &'a Ancestors {
    type Item = &'a Ancestor;
    type IntoIter = std::slice::Iter<'a, Ancestor>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// Base struct for simple declarations like variables and methods
#[derive(Debug)]
pub struct SimpleDeclaration {
    /// The fully qualified name of this declaration
    name: String,
    /// The list of definition IDs that compose this declaration
    definition_ids: Vec<DefinitionId>,
    /// The list of references that are made to this declaration
    references: Vec<ReferenceId>,
    /// The ID of the owner of this declaration
    owner_id: DeclarationId,
}

impl SimpleDeclaration {
    #[must_use]
    pub fn new(name: String, owner_id: DeclarationId) -> Self {
        Self {
            name,
            definition_ids: Vec::new(),
            references: Vec::new(),
            owner_id,
        }
    }

    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[must_use]
    pub fn references(&self) -> &[ReferenceId] {
        &self.references
    }

    #[must_use]
    pub fn definitions(&self) -> &[DefinitionId] {
        &self.definition_ids
    }

    #[must_use]
    pub fn has_no_definitions(&self) -> bool {
        self.definition_ids.is_empty()
    }

    pub fn add_definition(&mut self, definition_id: DefinitionId) {
        debug_assert!(
            !self.definition_ids.contains(&definition_id),
            "Cannot add the same exact definition to a declaration twice. Duplicate definition IDs"
        );
        self.definition_ids.push(definition_id);
    }

    pub fn add_reference(&mut self, id: ReferenceId) {
        self.references.push(id);
    }

    pub fn remove_definition(&mut self, definition_id: &DefinitionId) -> bool {
        if let Some(pos) = self.definition_ids.iter().position(|id| id == definition_id) {
            self.definition_ids.swap_remove(pos);
            self.definition_ids.shrink_to_fit();
            true
        } else {
            false
        }
    }

    #[must_use]
    pub fn owner_id(&self) -> &DeclarationId {
        &self.owner_id
    }

    #[must_use]
    pub fn unqualified_name(&self) -> String {
        self.name.rsplit("::").next().unwrap_or(&self.name).to_string()
    }

    pub fn extend(&mut self, other: &SimpleDeclaration) {
        self.definition_ids.extend(other.definition_ids.iter().copied());
        self.references.extend(other.references.iter().copied());
    }
}

/// Struct for namespace-like declarations such as classes and modules
#[derive(Debug)]
pub struct NamespaceDeclaration {
    /// The base simple declaration fields
    simple: SimpleDeclaration,
    /// The entities that are owned by this declaration. For example, constants and methods that are defined inside of
    /// the namespace. Note that this is a hashmap of unqualified name IDs to declaration IDs. That assists the
    /// traversal of the graph when trying to resolve constant references or trying to discover which methods exist in a
    /// class
    members: IdentityHashMap<StringId, DeclarationId>,
    /// The linearized ancestor chain for this declaration. These are the other declarations that this
    /// declaration inherits from
    ancestors: RwLock<Ancestors>,
    /// The set of declarations that inherit from this declaration
    descendants: Mutex<IdentityHashSet<DeclarationId>>,
    /// The singleton class associated with this declaration
    singleton_class_id: Option<DeclarationId>,
}

impl NamespaceDeclaration {
    #[must_use]
    pub fn new(name: String, owner_id: DeclarationId) -> Self {
        Self {
            simple: SimpleDeclaration::new(name, owner_id),
            members: IdentityHashMap::default(),
            ancestors: RwLock::new(Ancestors::Partial(Vec::new())),
            descendants: Mutex::new(IdentityHashSet::default()),
            singleton_class_id: None,
        }
    }

    #[must_use]
    pub fn simple(&self) -> &SimpleDeclaration {
        &self.simple
    }

    pub fn simple_mut(&mut self) -> &mut SimpleDeclaration {
        &mut self.simple
    }

    pub fn set_singleton_class_id(&mut self, declaration_id: DeclarationId) {
        self.singleton_class_id = Some(declaration_id);
    }

    pub fn singleton_class_id(&self) -> Option<&DeclarationId> {
        self.singleton_class_id.as_ref()
    }

    #[must_use]
    pub fn members(&self) -> &IdentityHashMap<StringId, DeclarationId> {
        &self.members
    }

    pub fn add_member(&mut self, string_id: StringId, declaration_id: DeclarationId) {
        self.members.insert(string_id, declaration_id);
    }

    pub fn remove_member(&mut self, string_id: &StringId) -> Option<DeclarationId> {
        self.members.remove(string_id)
    }

    #[must_use]
    pub fn get_member(&self, string_id: &StringId) -> Option<&DeclarationId> {
        self.members.get(string_id)
    }

    pub fn set_ancestors(&self, ancestors: Ancestors) {
        *self.ancestors.write().unwrap() = ancestors;
    }

    #[must_use]
    pub fn ancestors(&self) -> RwLockReadGuard<'_, Ancestors> {
        self.ancestors.read().unwrap()
    }

    #[must_use]
    pub fn clone_ancestors(&self) -> Ancestors {
        self.ancestors.read().unwrap().clone()
    }

    #[must_use]
    pub fn has_complete_ancestors(&self) -> bool {
        matches!(
            *self.ancestors.read().unwrap(),
            Ancestors::Complete(_) | Ancestors::Cyclic(_)
        )
    }

    pub fn add_descendant(&self, descendant_id: DeclarationId) {
        self.descendants.lock().unwrap().insert(descendant_id);
    }

    #[must_use]
    pub fn descendants(&self) -> MutexGuard<'_, IdentityHashSet<DeclarationId>> {
        self.descendants.lock().unwrap()
    }

    pub fn extend(&mut self, other: &NamespaceDeclaration) {
        self.simple.extend(&other.simple);
        self.members.extend(other.members.iter().map(|(k, v)| (*k, *v)));
    }
}

/// A `Declaration` represents the global concept of an entity in Ruby. For example, the class `Foo` may be defined 3
/// times in different files and the `Foo` declaration is the combination of all of those definitions that contribute to
/// the same fully qualified name
#[derive(Debug)]
pub enum Declaration {
    Class(Box<NamespaceDeclaration>),
    SingletonClass(Box<NamespaceDeclaration>),
    Module(Box<NamespaceDeclaration>),
    Constant(Box<SimpleDeclaration>),
    Method(Box<SimpleDeclaration>),
    GlobalVariable(Box<SimpleDeclaration>),
    InstanceVariable(Box<SimpleDeclaration>),
    ClassVariable(Box<SimpleDeclaration>),
}

impl Declaration {
    /// Returns a reference to the underlying [`SimpleDeclaration`] for any declaration type
    #[must_use]
    pub fn as_simple(&self) -> &SimpleDeclaration {
        match self {
            Declaration::Class(it) | Declaration::SingletonClass(it) | Declaration::Module(it) => it.simple(),
            Declaration::Constant(it)
            | Declaration::Method(it)
            | Declaration::GlobalVariable(it)
            | Declaration::InstanceVariable(it)
            | Declaration::ClassVariable(it) => it,
        }
    }

    /// Returns a mutable reference to the underlying [`SimpleDeclaration`] for any declaration type
    pub fn as_simple_mut(&mut self) -> &mut SimpleDeclaration {
        match self {
            Declaration::Class(it) | Declaration::SingletonClass(it) | Declaration::Module(it) => it.simple_mut(),
            Declaration::Constant(it)
            | Declaration::Method(it)
            | Declaration::GlobalVariable(it)
            | Declaration::InstanceVariable(it)
            | Declaration::ClassVariable(it) => it,
        }
    }

    /// Returns a reference to the underlying [`NamespaceDeclaration`] for namespace types (`Class`, `Module`, `SingletonClass`)
    /// Returns `None` for simple declaration types
    #[must_use]
    pub fn as_namespace(&self) -> Option<&NamespaceDeclaration> {
        match self {
            Declaration::Class(it) | Declaration::SingletonClass(it) | Declaration::Module(it) => Some(it),
            Declaration::Constant(_)
            | Declaration::Method(_)
            | Declaration::GlobalVariable(_)
            | Declaration::InstanceVariable(_)
            | Declaration::ClassVariable(_) => None,
        }
    }

    /// Returns a mutable reference to the underlying [`NamespaceDeclaration`] for namespace types
    /// Returns `None` for simple declaration types
    pub fn as_namespace_mut(&mut self) -> Option<&mut NamespaceDeclaration> {
        match self {
            Declaration::Class(it) | Declaration::SingletonClass(it) | Declaration::Module(it) => Some(it),
            Declaration::Constant(_)
            | Declaration::Method(_)
            | Declaration::GlobalVariable(_)
            | Declaration::InstanceVariable(_)
            | Declaration::ClassVariable(_) => None,
        }
    }

    /// Extend this declaration with more definitions by merging `other` into self
    ///
    /// # Panics
    ///
    /// Panics if `other` is a different variant than `self`
    pub fn extend(&mut self, other: Declaration) {
        match (self, other) {
            (Declaration::Class(a), Declaration::Class(b))
            | (Declaration::SingletonClass(a), Declaration::SingletonClass(b))
            | (Declaration::Module(a), Declaration::Module(b)) => a.extend(&b),
            (Declaration::Constant(a), Declaration::Constant(b))
            | (Declaration::Method(a), Declaration::Method(b))
            | (Declaration::GlobalVariable(a), Declaration::GlobalVariable(b))
            | (Declaration::InstanceVariable(a), Declaration::InstanceVariable(b))
            | (Declaration::ClassVariable(a), Declaration::ClassVariable(b)) => a.extend(&b),
            _ => panic!("Tried to merge incompatible declaration types"),
        }
    }

    #[must_use]
    pub fn name(&self) -> &str {
        self.as_simple().name()
    }

    #[must_use]
    pub fn references(&self) -> &[ReferenceId] {
        self.as_simple().references()
    }

    #[must_use]
    pub fn definitions(&self) -> &[DefinitionId] {
        self.as_simple().definitions()
    }

    #[must_use]
    pub fn has_no_definitions(&self) -> bool {
        self.as_simple().has_no_definitions()
    }

    pub fn add_definition(&mut self, definition_id: DefinitionId) {
        self.as_simple_mut().add_definition(definition_id);
    }

    pub fn add_reference(&mut self, id: ReferenceId) {
        self.as_simple_mut().add_reference(id);
    }

    // Deletes a definition from this declaration
    pub fn remove_definition(&mut self, definition_id: &DefinitionId) -> bool {
        self.as_simple_mut().remove_definition(definition_id)
    }

    #[must_use]
    pub fn owner_id(&self) -> &DeclarationId {
        self.as_simple().owner_id()
    }

    #[must_use]
    pub fn unqualified_name(&self) -> String {
        self.as_simple().unqualified_name()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic(expected = "Cannot add the same exact definition to a declaration twice. Duplicate definition IDs")]
    fn inserting_duplicate_definitions() {
        let mut decl = Declaration::Class(Box::new(NamespaceDeclaration::new(
            "MyDecl".to_string(),
            DeclarationId::from("Object"),
        )));
        let def_id = DefinitionId::new(123);

        // The second call will panic because we're adding the same exact ID twice
        decl.add_definition(def_id);
        decl.add_definition(def_id);
    }

    #[test]
    fn adding_and_removing_members() {
        let mut decl = Declaration::Class(Box::new(NamespaceDeclaration::new(
            "Foo".to_string(),
            DeclarationId::from("Object"),
        )));
        let member_name_id = StringId::from("Bar");
        let member_decl_id = DeclarationId::from("Foo::Bar");

        let ns = decl.as_namespace_mut().unwrap();
        ns.add_member(member_name_id, member_decl_id);
        assert_eq!(ns.members().len(), 1);

        let removed = ns.remove_member(&member_name_id);
        assert_eq!(removed, Some(member_decl_id));
        assert_eq!(ns.members().len(), 0);
    }

    #[test]
    fn unqualified_name() {
        let decl = Declaration::Class(Box::new(NamespaceDeclaration::new(
            "Foo".to_string(),
            DeclarationId::from("Foo"),
        )));
        assert_eq!(decl.unqualified_name(), "Foo");

        let decl = Declaration::Class(Box::new(NamespaceDeclaration::new(
            "Foo::Bar".to_string(),
            DeclarationId::from("Foo"),
        )));
        assert_eq!(decl.unqualified_name(), "Bar");

        let decl = Declaration::Class(Box::new(NamespaceDeclaration::new(
            "Foo::Bar::baz".to_string(),
            DeclarationId::from("Foo::Bar"),
        )));
        assert_eq!(decl.unqualified_name(), "baz");
    }

    #[test]
    fn as_simple_returns_simple_declaration() {
        let decl = Declaration::Method(Box::new(SimpleDeclaration::new(
            "foo".to_string(),
            DeclarationId::from("Object"),
        )));
        let simple = decl.as_simple();
        assert_eq!(simple.name(), "foo");
    }

    #[test]
    fn as_namespace_returns_namespace_for_classes() {
        let decl = Declaration::Class(Box::new(NamespaceDeclaration::new(
            "Foo".to_string(),
            DeclarationId::from("Object"),
        )));
        assert!(decl.as_namespace().is_some());
    }

    #[test]
    fn as_namespace_returns_none_for_methods() {
        let decl = Declaration::Method(Box::new(SimpleDeclaration::new(
            "foo".to_string(),
            DeclarationId::from("Object"),
        )));
        assert!(decl.as_namespace().is_none());
    }
}
