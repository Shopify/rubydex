use std::hash::Hash;

use crate::assert_mem_size;
use crate::model::ids::{
    ClassVariableReferenceId, GlobalVariableReferenceId, InstanceVariableReferenceId, MethodReferenceId,
};
use crate::model::{
    identity_maps::{IdentityHashMap, IdentityHashSet},
    ids::{ConstantReferenceId, DeclarationId, DefinitionId, NameId, StringId},
};

/// A single ancestor in the linearized ancestor chain
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ancestor {
    /// A complete ancestor that we have fully linearized
    Complete(DeclarationId),
    /// A partial ancestor that is missing linearization
    Partial(NameId),
}
assert_mem_size!(Ancestor, 16);

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
assert_mem_size!(Ancestors, 32);

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

macro_rules! all_namespaces {
    ($value:expr, $var:ident => $expr:expr) => {
        match $value {
            Namespace::Class($var) => $expr,
            Namespace::Module($var) => $expr,
            Namespace::SingletonClass($var) => $expr,
            Namespace::Todo($var) => $expr,
        }
    };
}

/// Macro to generate a new struct for namespace-like declarations such as classes and modules
macro_rules! namespace_declaration {
    ($variant:ident, $name:ident) => {
        #[derive(Debug)]
        pub struct $name {
            core: DeclarationCore<ConstantReferenceId>,
            namespace_store: NamespaceStore,
        }

        impl $name {
            #[must_use]
            pub fn new(name: String, owner_id: DeclarationId) -> Self {
                Self {
                    core: DeclarationCore::new(name, owner_id),
                    namespace_store: NamespaceStore::new(),
                }
            }

            pub fn extend(&mut self, other: Declaration) {
                self.core.definition_ids.extend(other.definitions());

                match other {
                    Declaration::Namespace(namespace) => {
                        self.namespace_store.members.extend(namespace.members());
                        self.core.references.extend(namespace.references());
                    }
                    Declaration::Constant(it) | Declaration::ConstantAlias(it) => {
                        self.core.references.extend(it.references());
                    }
                    Declaration::Method(_)
                    | Declaration::GlobalVariable(_)
                    | Declaration::InstanceVariable(_)
                    | Declaration::ClassVariable(_) => {
                        panic!("Cannot extend a namespace declaration with a non-namespace declaration");
                    }
                }
            }
        }
    };
}

/// The core data of a declaration, shared across all of them
#[derive(Debug)]
pub struct DeclarationCore<T> {
    /// The fully qualified name of this declaration
    name: Box<str>,
    /// The list of definition IDs that compose this declaration
    definition_ids: Vec<DefinitionId>,
    /// The set of references that are made to this declaration
    references: IdentityHashSet<T>,
    /// The ID of the owner of this declaration
    owner_id: DeclarationId,
}

impl<T: Eq + Hash> DeclarationCore<T> {
    #[must_use]
    pub fn new(name: String, owner_id: DeclarationId) -> Self {
        Self {
            name: name.into_boxed_str(),
            definition_ids: Vec::new(),
            references: IdentityHashSet::default(),
            owner_id,
        }
    }

    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[must_use]
    pub fn unqualified_name(&self) -> String {
        let after_colons = self.name.rsplit("::").next().unwrap_or(&self.name);
        after_colons.rsplit('#').next().unwrap_or(after_colons).to_string()
    }

    pub fn extend(&mut self, other: Self) {
        self.definition_ids.extend(other.definition_ids);
        self.references.extend(other.references);
    }

    #[must_use]
    pub fn references(&self) -> &IdentityHashSet<T> {
        &self.references
    }

    pub fn add_reference(&mut self, reference_id: T) {
        self.references.insert(reference_id);
    }

    pub fn remove_reference(&mut self, reference_id: &T) {
        self.references.remove(reference_id);
    }

    #[must_use]
    pub fn has_no_definitions(&self) -> bool {
        self.definition_ids.is_empty()
    }

    #[must_use]
    pub fn definitions(&self) -> &[DefinitionId] {
        &self.definition_ids
    }

    pub fn add_definition(&mut self, definition_id: DefinitionId) {
        debug_assert!(
            !self.definition_ids.contains(&definition_id),
            "Cannot add the same exact definition to a declaration twice. Duplicate definition IDs"
        );

        self.definition_ids.push(definition_id);
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
}

/// Storage for namespace data, like ancestors, descendants, members and singleton class
#[derive(Debug)]
pub struct NamespaceStore {
    /// The entities that are owned by this declaration. For example, constants and methods that are defined inside of
    /// the namespace. Note that this is a hashmap of unqualified name IDs to declaration IDs. That assists the
    /// traversal of the graph when trying to resolve constant references or trying to discover which methods exist in a
    /// class
    members: IdentityHashMap<StringId, DeclarationId>,
    /// The linearized ancestor chain for this declaration. These are the other declarations that this
    /// declaration inherits from
    ancestors: Ancestors,
    /// The set of declarations that inherit from this declaration
    descendants: IdentityHashSet<DeclarationId>,
    /// The singleton class associated with this declaration
    singleton_class_id: Option<DeclarationId>,
}

impl NamespaceStore {
    #[must_use]
    pub fn new() -> Self {
        Self {
            members: IdentityHashMap::default(),
            ancestors: Ancestors::Partial(Vec::new()),
            descendants: IdentityHashSet::default(),
            singleton_class_id: None,
        }
    }

    pub fn extend(&mut self, other: Self) {
        self.members.extend(other.members);
        self.descendants.extend(other.descendants);
    }

    pub fn set_singleton_class_id(&mut self, declaration_id: DeclarationId) {
        self.singleton_class_id = Some(declaration_id);
    }

    pub fn clear_singleton_class_id(&mut self) {
        self.singleton_class_id = None;
    }

    #[must_use]
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
    pub fn member(&self, string_id: &StringId) -> Option<&DeclarationId> {
        self.members.get(string_id)
    }

    pub fn set_ancestors(&mut self, ancestors: Ancestors) {
        self.ancestors = ancestors;
    }

    #[must_use]
    pub fn ancestors(&self) -> &Ancestors {
        &self.ancestors
    }

    #[must_use]
    pub fn clone_ancestors(&self) -> Ancestors {
        self.ancestors.clone()
    }

    #[must_use]
    pub fn has_complete_ancestors(&self) -> bool {
        matches!(&self.ancestors, Ancestors::Complete(_) | Ancestors::Cyclic(_))
    }

    pub fn add_descendant(&mut self, descendant_id: DeclarationId) {
        self.descendants.insert(descendant_id);
    }

    fn remove_descendant(&mut self, descendant_id: DeclarationId) {
        self.descendants.remove(&descendant_id);
    }

    pub fn clear_descendants(&mut self) {
        self.descendants.clear();
    }

    #[must_use]
    pub fn descendants(&self) -> &IdentityHashSet<DeclarationId> {
        &self.descendants
    }
}

impl Default for NamespaceStore {
    fn default() -> Self {
        Self::new()
    }
}

/// A `Declaration` represents the global concept of an entity in Ruby. For example, the class `Foo` may be defined 3
/// times in different files and the `Foo` declaration is the combination of all of those definitions that contribute to
/// the same fully qualified name
#[derive(Debug)]
pub enum Declaration {
    Namespace(Namespace),
    Constant(Box<ConstantDeclaration>),
    ConstantAlias(Box<ConstantAliasDeclaration>),
    Method(Box<MethodDeclaration>),
    GlobalVariable(Box<GlobalVariableDeclaration>),
    InstanceVariable(Box<InstanceVariableDeclaration>),
    ClassVariable(Box<ClassVariableDeclaration>),
}
assert_mem_size!(Declaration, 16);

impl Declaration {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Namespace(namespace) => namespace.core().name(),
            Self::ConstantAlias(it) | Self::Constant(it) => it.name(),
            Self::Method(it) => it.name(),
            Self::GlobalVariable(it) => it.name(),
            Self::InstanceVariable(it) => it.name(),
            Self::ClassVariable(it) => it.name(),
        }
    }

    #[must_use]
    pub fn kind(&self) -> &'static str {
        match self {
            Declaration::Namespace(namespace) => namespace.kind(),
            Declaration::Constant(_) => "Constant",
            Declaration::ConstantAlias(_) => "ConstantAlias",
            Declaration::Method(_) => "Method",
            Declaration::GlobalVariable(_) => "GlobalVariable",
            Declaration::InstanceVariable(_) => "InstanceVariable",
            Declaration::ClassVariable(_) => "ClassVariable",
        }
    }

    #[must_use]
    pub fn as_namespace(&self) -> Option<&Namespace> {
        match self {
            Declaration::Namespace(namespace) => Some(namespace),
            _ => None,
        }
    }

    #[must_use]
    pub fn as_namespace_mut(&mut self) -> Option<&mut Namespace> {
        match self {
            Declaration::Namespace(namespace) => Some(namespace),
            _ => None,
        }
    }

    #[must_use]
    pub fn as_constant(&self) -> Option<&ConstantDeclaration> {
        match self {
            Declaration::Constant(constant) => Some(constant),
            _ => None,
        }
    }

    #[must_use]
    pub fn as_constant_alias(&self) -> Option<&ConstantAliasDeclaration> {
        match self {
            Declaration::ConstantAlias(alias) => Some(alias),
            _ => None,
        }
    }

    #[must_use]
    pub fn as_method(&self) -> Option<&MethodDeclaration> {
        match self {
            Declaration::Method(method) => Some(method),
            _ => None,
        }
    }

    #[must_use]
    pub fn as_global_variable(&self) -> Option<&GlobalVariableDeclaration> {
        match self {
            Declaration::GlobalVariable(global) => Some(global),
            _ => None,
        }
    }

    #[must_use]
    pub fn as_class_variable(&self) -> Option<&ClassVariableDeclaration> {
        match self {
            Declaration::ClassVariable(cvar) => Some(cvar),
            _ => None,
        }
    }

    #[must_use]
    pub fn as_instance_variable(&self) -> Option<&InstanceVariableDeclaration> {
        match self {
            Declaration::InstanceVariable(ivar) => Some(ivar),
            _ => None,
        }
    }

    #[must_use]
    pub fn definitions(&self) -> &[DefinitionId] {
        match self {
            Self::Namespace(namespace) => namespace.core().definitions(),
            Self::Constant(it) | Self::ConstantAlias(it) => it.definitions(),
            Self::Method(it) => it.definitions(),
            Self::GlobalVariable(it) => it.definitions(),
            Self::InstanceVariable(it) => it.definitions(),
            Self::ClassVariable(it) => it.definitions(),
        }
    }

    #[must_use]
    pub fn has_no_definitions(&self) -> bool {
        match self {
            Self::Namespace(namespace) => namespace.core().has_no_definitions(),
            Self::Constant(it) | Self::ConstantAlias(it) => it.has_no_definitions(),
            Self::Method(it) => it.has_no_definitions(),
            Self::GlobalVariable(it) => it.has_no_definitions(),
            Self::InstanceVariable(it) => it.has_no_definitions(),
            Self::ClassVariable(it) => it.has_no_definitions(),
        }
    }

    pub fn add_definition(&mut self, definition_id: DefinitionId) {
        match self {
            Self::Namespace(namespace) => namespace.core_mut().add_definition(definition_id),
            Self::Constant(it) | Self::ConstantAlias(it) => it.add_definition(definition_id),
            Self::Method(it) => it.add_definition(definition_id),
            Self::GlobalVariable(it) => it.add_definition(definition_id),
            Self::InstanceVariable(it) => it.add_definition(definition_id),
            Self::ClassVariable(it) => it.add_definition(definition_id),
        }
    }

    // Deletes a definition from this declaration
    pub fn remove_definition(&mut self, definition_id: &DefinitionId) -> bool {
        match self {
            Self::Namespace(namespace) => namespace.core_mut().remove_definition(definition_id),
            Self::Constant(it) | Self::ConstantAlias(it) => it.remove_definition(definition_id),
            Self::Method(it) => it.remove_definition(definition_id),
            Self::GlobalVariable(it) => it.remove_definition(definition_id),
            Self::InstanceVariable(it) => it.remove_definition(definition_id),
            Self::ClassVariable(it) => it.remove_definition(definition_id),
        }
    }

    #[must_use]
    pub fn owner_id(&self) -> &DeclarationId {
        match self {
            Self::Namespace(namespace) => &namespace.core().owner_id,
            Self::Constant(it) | Self::ConstantAlias(it) => &it.owner_id,
            Self::Method(it) => &it.owner_id,
            Self::GlobalVariable(it) => &it.owner_id,
            Self::InstanceVariable(it) => &it.owner_id,
            Self::ClassVariable(it) => &it.owner_id,
        }
    }

    // Splits the fully qualified name either in the last `::` or the `#` to return the simple name of this declaration
    #[must_use]
    pub fn unqualified_name(&self) -> String {
        match self {
            Self::Namespace(namespace) => namespace.core().unqualified_name(),
            Self::Constant(it) | Self::ConstantAlias(it) => it.unqualified_name(),
            Self::Method(it) => it.unqualified_name(),
            Self::GlobalVariable(it) => it.unqualified_name(),
            Self::InstanceVariable(it) => it.unqualified_name(),
            Self::ClassVariable(it) => it.unqualified_name(),
        }
    }

    #[must_use]
    pub fn reference_count(&self) -> usize {
        match self {
            Self::Namespace(namespace) => namespace.core().references.len(),
            Self::Constant(it) | Self::ConstantAlias(it) => it.references.len(),
            Self::Method(it) => it.references.len(),
            Self::GlobalVariable(it) => it.references.len(),
            Self::InstanceVariable(it) => it.references.len(),
            Self::ClassVariable(it) => it.references.len(),
        }
    }

    /// Returns the constant reference IDs for declarations that track constant references (`Namespace`, `Constant`,
    /// `ConstantAlias`). Returns `None` for other declaration types.
    #[must_use]
    pub fn constant_references(&self) -> Option<&IdentityHashSet<ConstantReferenceId>> {
        match self {
            Declaration::Namespace(it) => Some(it.references()),
            Declaration::Constant(it) | Declaration::ConstantAlias(it) => Some(it.references()),
            _ => None,
        }
    }

    /// Adds a constant reference to this declaration.
    ///
    /// # Panics
    ///
    /// Panics if called on a declaration that doesn't track constant references.
    pub fn add_constant_reference(&mut self, reference_id: ConstantReferenceId) {
        match self {
            Declaration::Namespace(it) => it.add_reference(reference_id),
            Declaration::Constant(it) | Declaration::ConstantAlias(it) => it.add_reference(reference_id),
            _ => unreachable!("Cannot add constant reference to {} declaration", self.kind()),
        }
    }

    /// Removes a constant reference from this declaration.
    ///
    /// # Panics
    ///
    /// Panics if called on a declaration that doesn't track constant references.
    pub fn remove_constant_reference(&mut self, reference_id: &ConstantReferenceId) {
        match self {
            Declaration::Namespace(it) => it.remove_reference(reference_id),
            Declaration::Constant(it) | Declaration::ConstantAlias(it) => it.remove_reference(reference_id),
            _ => unreachable!("Cannot remove constant reference from {} declaration", self.kind()),
        }
    }
}

#[derive(Debug)]
pub enum Namespace {
    Class(Box<ClassDeclaration>),
    SingletonClass(Box<SingletonClassDeclaration>),
    Module(Box<ModuleDeclaration>),
    Todo(Box<TodoDeclaration>),
}
assert_mem_size!(Namespace, 16);

impl Namespace {
    #[must_use]
    pub fn core(&self) -> &DeclarationCore<ConstantReferenceId> {
        all_namespaces!(self, it => &it.core)
    }

    #[must_use]
    pub fn core_mut(&mut self) -> &mut DeclarationCore<ConstantReferenceId> {
        all_namespaces!(self, it => &mut it.core)
    }

    #[must_use]
    pub fn kind(&self) -> &'static str {
        match self {
            Namespace::Class(_) => "Class",
            Namespace::SingletonClass(_) => "SingletonClass",
            Namespace::Module(_) => "Module",
            Namespace::Todo(_) => "<TODO>",
        }
    }

    #[must_use]
    pub fn references(&self) -> &IdentityHashSet<ConstantReferenceId> {
        all_namespaces!(self, it => &it.core.references)
    }

    pub fn remove_reference(&mut self, reference_id: &ConstantReferenceId) {
        all_namespaces!(self, it => {
            it.core.remove_reference(reference_id);
        });
    }

    pub fn add_reference(&mut self, reference_id: ConstantReferenceId) {
        all_namespaces!(self, it => {
            it.core.add_reference(reference_id);
        });
    }

    #[must_use]
    pub fn definitions(&self) -> &[DefinitionId] {
        all_namespaces!(self, it => &it.core.definitions())
    }

    #[must_use]
    pub fn members(&self) -> &IdentityHashMap<StringId, DeclarationId> {
        all_namespaces!(self, it => &it.namespace_store.members)
    }

    pub fn extend(&mut self, other: Declaration) {
        all_namespaces!(self, it => it.extend(other));
    }

    #[must_use]
    pub fn ancestors(&self) -> &Ancestors {
        all_namespaces!(self, it => it.namespace_store.ancestors())
    }

    #[must_use]
    pub fn clone_ancestors(&self) -> Ancestors {
        all_namespaces!(self, it => it.namespace_store.clone_ancestors())
    }

    pub fn set_ancestors(&mut self, ancestors: Ancestors) {
        all_namespaces!(self, it => it.namespace_store.set_ancestors(ancestors));
    }

    #[must_use]
    pub fn has_complete_ancestors(&self) -> bool {
        all_namespaces!(self, it => it.namespace_store.has_complete_ancestors())
    }

    #[must_use]
    pub fn descendants(&self) -> &IdentityHashSet<DeclarationId> {
        all_namespaces!(self, it => it.namespace_store.descendants())
    }

    pub fn add_descendant(&mut self, descendant_id: DeclarationId) {
        all_namespaces!(self, it => it.namespace_store.add_descendant(descendant_id));
    }

    pub fn remove_descendant(&mut self, descendant_id: DeclarationId) {
        all_namespaces!(self, it => it.namespace_store.remove_descendant(descendant_id));
    }

    pub fn for_each_ancestor<F>(&self, mut f: F)
    where
        F: FnMut(&Ancestor),
    {
        all_namespaces!(self, it => it.namespace_store.ancestors().iter().for_each(&mut f));
    }

    pub fn for_each_descendant<F>(&self, mut f: F)
    where
        F: FnMut(&DeclarationId),
    {
        all_namespaces!(self, it => it.namespace_store.descendants().iter().for_each(&mut f));
    }

    pub fn clear_ancestors(&mut self) {
        all_namespaces!(self, it => it.namespace_store.set_ancestors(Ancestors::Partial(vec![])));
    }

    pub fn clear_descendants(&mut self) {
        all_namespaces!(self, it => it.namespace_store.clear_descendants());
    }

    #[must_use]
    pub fn member(&self, str_id: &StringId) -> Option<&DeclarationId> {
        all_namespaces!(self, it => it.namespace_store.member(str_id))
    }

    pub fn add_member(&mut self, string_id: StringId, declaration_id: DeclarationId) {
        all_namespaces!(self, it => it.namespace_store.add_member(string_id, declaration_id));
    }

    pub fn remove_member(&mut self, str_id: &StringId) -> Option<DeclarationId> {
        all_namespaces!(self, it => it.namespace_store.remove_member(str_id))
    }

    #[must_use]
    pub fn singleton_class(&self) -> Option<&DeclarationId> {
        all_namespaces!(self, it => it.namespace_store.singleton_class_id())
    }

    pub fn set_singleton_class_id(&mut self, declaration_id: DeclarationId) {
        all_namespaces!(self, it => it.namespace_store.set_singleton_class_id(declaration_id));
    }

    pub fn clear_singleton_class_id(&mut self) {
        all_namespaces!(self, it => it.namespace_store.clear_singleton_class_id());
    }

    #[must_use]
    pub fn owner_id(&self) -> &DeclarationId {
        all_namespaces!(self, it => &it.core.owner_id)
    }

    #[must_use]
    pub fn name(&self) -> &str {
        all_namespaces!(self, it => &it.core.name())
    }
}

namespace_declaration!(Class, ClassDeclaration);
assert_mem_size!(ClassDeclaration, 184);
namespace_declaration!(Module, ModuleDeclaration);
assert_mem_size!(ModuleDeclaration, 184);
namespace_declaration!(SingletonClass, SingletonClassDeclaration);
assert_mem_size!(SingletonClassDeclaration, 184);
namespace_declaration!(Todo, TodoDeclaration);
assert_mem_size!(TodoDeclaration, 184);

pub type ConstantDeclaration = DeclarationCore<ConstantReferenceId>;
assert_mem_size!(ConstantDeclaration, 80);

pub type MethodDeclaration = DeclarationCore<MethodReferenceId>;
assert_mem_size!(MethodDeclaration, 80);

pub type GlobalVariableDeclaration = DeclarationCore<GlobalVariableReferenceId>;
assert_mem_size!(GlobalVariableDeclaration, 80);

pub type InstanceVariableDeclaration = DeclarationCore<InstanceVariableReferenceId>;
assert_mem_size!(InstanceVariableDeclaration, 80);

pub type ClassVariableDeclaration = DeclarationCore<ClassVariableReferenceId>;
assert_mem_size!(ClassVariableDeclaration, 80);

pub type ConstantAliasDeclaration = DeclarationCore<ConstantReferenceId>;
assert_mem_size!(ConstantAliasDeclaration, 80);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic(expected = "Cannot add the same exact definition to a declaration twice. Duplicate definition IDs")]
    fn inserting_duplicate_definitions() {
        let mut decl = Declaration::Namespace(Namespace::Class(Box::new(ClassDeclaration::new(
            "MyDecl".to_string(),
            DeclarationId::from("Object"),
        ))));
        let def_id = DefinitionId::new(123);

        // The second call will panic because we're adding the same exact ID twice
        decl.add_definition(def_id);
        decl.add_definition(def_id);
    }

    #[test]
    fn adding_and_removing_members() {
        let decl = Declaration::Namespace(Namespace::Class(Box::new(ClassDeclaration::new(
            "Foo".to_string(),
            DeclarationId::from("Object"),
        ))));
        let member_name_id = StringId::from("Bar");
        let member_decl_id = DeclarationId::from("Foo::Bar");

        let Declaration::Namespace(mut namespace) = decl else {
            panic!("Expected a namespace declaration");
        };
        namespace.add_member(member_name_id, member_decl_id);
        assert_eq!(namespace.members().len(), 1);

        let removed = namespace.remove_member(&member_name_id);
        assert_eq!(removed, Some(member_decl_id));
        assert_eq!(namespace.members().len(), 0);
    }

    #[test]
    fn unqualified_name() {
        let decl = Declaration::Namespace(Namespace::Class(Box::new(ClassDeclaration::new(
            "Foo".to_string(),
            DeclarationId::from("Foo"),
        ))));
        assert_eq!(decl.unqualified_name(), "Foo");

        let decl = Declaration::Namespace(Namespace::Class(Box::new(ClassDeclaration::new(
            "Foo::Bar".to_string(),
            DeclarationId::from("Foo"),
        ))));
        assert_eq!(decl.unqualified_name(), "Bar");

        let decl = Declaration::Namespace(Namespace::Class(Box::new(ClassDeclaration::new(
            "Foo::Bar#baz".to_string(),
            DeclarationId::from("Foo::Bar"),
        ))));
        assert_eq!(decl.unqualified_name(), "baz");
    }
}
