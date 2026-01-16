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

macro_rules! all_declarations {
    ($value:expr, $var:ident => $expr:expr) => {
        match $value {
            Declaration::Namespace(Namespace::Class($var)) => $expr,
            Declaration::Namespace(Namespace::Module($var)) => $expr,
            Declaration::Namespace(Namespace::SingletonClass($var)) => $expr,
            Declaration::Constant($var) => $expr,
            Declaration::ConstantAlias($var) => $expr,
            Declaration::Method($var) => $expr,
            Declaration::GlobalVariable($var) => $expr,
            Declaration::InstanceVariable($var) => $expr,
            Declaration::ClassVariable($var) => $expr,
        }
    };
}

macro_rules! all_namespaces {
    ($value:expr, $var:ident => $expr:expr) => {
        match $value {
            Namespace::Class($var) => $expr,
            Namespace::Module($var) => $expr,
            Namespace::SingletonClass($var) => $expr,
        }
    };
}

/// Macro to generate a new struct for namespace-like declarations such as classes and modules
macro_rules! namespace_declaration {
    ($variant:ident, $name:ident) => {
        #[derive(Debug)]
        pub struct $name {
            /// The fully qualified name of this declaration
            name: String,
            /// The list of definition IDs that compose this declaration
            definition_ids: Vec<DefinitionId>,
            /// The set of references that are made to this declaration
            references: IdentityHashSet<ReferenceId>,
            /// The ID of the owner of this declaration. For singleton classes, this is the ID of the attached object
            owner_id: DeclarationId,
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

        impl $name {
            #[must_use]
            pub fn new(name: String, owner_id: DeclarationId) -> Self {
                Self {
                    name,
                    definition_ids: Vec::new(),
                    members: IdentityHashMap::default(),
                    references: IdentityHashSet::default(),
                    owner_id,
                    ancestors: Ancestors::Partial(Vec::new()),
                    descendants: IdentityHashSet::default(),
                    singleton_class_id: None,
                }
            }

            pub fn extend(&mut self, other: Namespace) {
                self.definition_ids.extend(other.definitions());
                self.references.extend(other.references());
                self.members.extend(other.members());
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
            pub fn member(&self, string_id: &StringId) -> Option<&DeclarationId> {
                self.members.get(string_id)
            }

            pub fn set_ancestors(&mut self, ancestors: Ancestors) {
                self.ancestors = ancestors;
            }

            pub fn ancestors(&self) -> &Ancestors {
                &self.ancestors
            }

            pub fn ancestors_mut(&mut self) -> &mut Ancestors {
                &mut self.ancestors
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

            fn remove_descendant(&mut self, descendant_id: &DeclarationId) {
                self.descendants.remove(descendant_id);
            }

            pub fn clear_descendants(&mut self) {
                self.descendants.clear();
            }

            pub fn descendants(&self) -> &IdentityHashSet<DeclarationId> {
                &self.descendants
            }
        }
    };
}

/// Macro to generate a new struct for simple declarations like variables and methods
macro_rules! simple_declaration {
    ($name:ident) => {
        #[derive(Debug)]
        pub struct $name {
            /// The fully qualified name of this declaration
            name: String,
            /// The list of definition IDs that compose this declaration
            definition_ids: Vec<DefinitionId>,
            /// The set of references that are made to this declaration
            references: IdentityHashSet<ReferenceId>,
            /// The ID of the owner of this declaration
            owner_id: DeclarationId,
        }

        impl $name {
            #[must_use]
            pub fn new(name: String, owner_id: DeclarationId) -> Self {
                Self {
                    name,
                    definition_ids: Vec::new(),
                    references: IdentityHashSet::default(),
                    owner_id,
                }
            }

            pub fn extend(&mut self, other: Declaration) {
                self.definition_ids.extend(other.definitions());
                self.references.extend(other.references());
            }
        }
    };
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

impl Declaration {
    #[must_use]
    pub fn name(&self) -> &str {
        all_declarations!(self, it => &it.name)
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
    pub fn references(&self) -> &IdentityHashSet<ReferenceId> {
        all_declarations!(self, it => &it.references)
    }

    #[must_use]
    pub fn definitions(&self) -> &[DefinitionId] {
        all_declarations!(self, it => &it.definition_ids)
    }

    #[must_use]
    pub fn has_no_definitions(&self) -> bool {
        all_declarations!(self, it => it.definition_ids.is_empty())
    }

    pub fn add_definition(&mut self, definition_id: DefinitionId) {
        all_declarations!(self, it => {
            debug_assert!(
                !it.definition_ids.contains(&definition_id),
                "Cannot add the same exact definition to a declaration twice. Duplicate definition IDs"
            );

            it.definition_ids.push(definition_id);
        });
    }

    pub fn add_reference(&mut self, id: ReferenceId) {
        all_declarations!(self, it => {
            it.references.insert(id);
        });
    }

    pub fn remove_reference(&mut self, reference_id: &ReferenceId) {
        all_declarations!(self, it => {
            it.references.remove(reference_id);
        });
    }

    // Deletes a definition from this declaration
    pub fn remove_definition(&mut self, definition_id: &DefinitionId) -> bool {
        all_declarations!(self, it => {
            if let Some(pos) = it.definition_ids.iter().position(|id| id == definition_id) {
                it.definition_ids.swap_remove(pos);
                it.definition_ids.shrink_to_fit();
                true
            } else {
                false
            }
        })
    }

    #[must_use]
    pub fn owner_id(&self) -> &DeclarationId {
        all_declarations!(self, it => &it.owner_id)
    }

    // Splits the fully qualified name either in the last `::` or the `#` to return the simple name of this declaration
    #[must_use]
    pub fn unqualified_name(&self) -> String {
        all_declarations!(self, it => {
            let after_colons = it.name.rsplit("::").next().unwrap_or(&it.name);
            after_colons.rsplit('#').next().unwrap_or(after_colons).to_string()
        })
    }
}

#[derive(Debug)]
pub enum Namespace {
    Class(Box<ClassDeclaration>),
    SingletonClass(Box<SingletonClassDeclaration>),
    Module(Box<ModuleDeclaration>),
}

impl Namespace {
    #[must_use]
    pub fn kind(&self) -> &'static str {
        match self {
            Namespace::Class(_) => "Class",
            Namespace::SingletonClass(_) => "SingletonClass",
            Namespace::Module(_) => "Module",
        }
    }

    #[must_use]
    pub fn references(&self) -> &IdentityHashSet<ReferenceId> {
        all_namespaces!(self, it => &it.references)
    }

    #[must_use]
    pub fn definitions(&self) -> &[DefinitionId] {
        all_namespaces!(self, it => &it.definition_ids)
    }

    #[must_use]
    pub fn members(&self) -> &IdentityHashMap<StringId, DeclarationId> {
        all_namespaces!(self, it => &it.members)
    }

    /// # Panics
    ///
    /// Panics if the declaration is not a namespace or a constant
    #[must_use]
    pub fn ancestors(&self) -> Ancestors {
        all_namespaces!(self, it => it.clone_ancestors())
    }

    pub fn set_ancestors(&mut self, ancestors: Ancestors) {
        all_namespaces!(self, it => it.set_ancestors(ancestors));
    }

    #[must_use]
    pub fn has_complete_ancestors(&self) -> bool {
        all_namespaces!(self, it => it.has_complete_ancestors())
    }

    pub fn add_descendant(&mut self, descendant_id: DeclarationId) {
        all_namespaces!(self, it => it.add_descendant(descendant_id));
    }

    pub fn remove_descendant(&mut self, descendant_id: &DeclarationId) {
        all_namespaces!(self, it => it.remove_descendant(descendant_id));
    }

    pub fn for_each_ancestor<F>(&self, mut f: F)
    where
        F: FnMut(&Ancestor),
    {
        all_namespaces!(self, it => it.ancestors().iter().for_each(&mut f));
    }

    pub fn for_each_descendant<F>(&self, mut f: F)
    where
        F: FnMut(&DeclarationId),
    {
        all_namespaces!(self, it => it.descendants().iter().for_each(&mut f));
    }

    pub fn clear_ancestors(&mut self) {
        all_namespaces!(self, it => it.set_ancestors(Ancestors::Partial(vec![])));
    }

    pub fn clear_descendants(&mut self) {
        all_namespaces!(self, it => it.clear_descendants());
    }

    #[must_use]
    pub fn member(&self, str_id: &StringId) -> Option<&DeclarationId> {
        all_namespaces!(self, it => it.member(str_id))
    }

    #[must_use]
    pub fn singleton_class(&self) -> Option<&DeclarationId> {
        all_namespaces!(self, it => it.singleton_class_id())
    }

    pub fn set_singleton_class_id(&mut self, declaration_id: DeclarationId) {
        all_namespaces!(self, it => it.set_singleton_class_id(declaration_id));
    }
}

namespace_declaration!(Class, ClassDeclaration);
namespace_declaration!(Module, ModuleDeclaration);
namespace_declaration!(SingletonClass, SingletonClassDeclaration);
simple_declaration!(ConstantDeclaration);
simple_declaration!(MethodDeclaration);
simple_declaration!(GlobalVariableDeclaration);
simple_declaration!(InstanceVariableDeclaration);
simple_declaration!(ClassVariableDeclaration);
simple_declaration!(ConstantAliasDeclaration);

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

        let Declaration::Namespace(Namespace::Class(mut class)) = decl else {
            panic!("Expected a class declaration");
        };
        class.add_member(member_name_id, member_decl_id);
        assert_eq!(class.members.len(), 1);

        let removed = class.remove_member(&member_name_id);
        assert_eq!(removed, Some(member_decl_id));
        assert_eq!(class.members.len(), 0);
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
