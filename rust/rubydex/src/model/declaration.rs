use std::sync::{Mutex, MutexGuard, RwLock, RwLockReadGuard};

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
            Declaration::Class($var) => $expr,
            Declaration::SingletonClass($var) => $expr,
            Declaration::Module($var) => $expr,
            Declaration::Constant($var) => $expr,
            Declaration::Method($var) => $expr,
            Declaration::GlobalVariable($var) => $expr,
            Declaration::InstanceVariable($var) => $expr,
            Declaration::ClassVariable($var) => $expr,
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
            ancestors: RwLock<Ancestors>,
            /// The set of declarations that inherit from this declaration
            descendants: Mutex<IdentityHashSet<DeclarationId>>,
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
                    ancestors: RwLock::new(Ancestors::Partial(Vec::new())),
                    descendants: Mutex::new(IdentityHashSet::default()),
                    singleton_class_id: None,
                }
            }

            pub fn extend(&mut self, other: Declaration) {
                match other {
                    Declaration::$variant(it) => {
                        self.members.extend(it.members);
                    }
                    _ => panic!("Tried to merge incompatible declaration types"),
                }
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
                let mut ancestors_lock = self.ancestors.write().unwrap();
                *ancestors_lock = ancestors;
            }

            fn ancestors(&self) -> RwLockReadGuard<'_, Ancestors> {
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

            fn remove_descendant(&self, descendant_id: &DeclarationId) {
                self.descendants.lock().unwrap().remove(descendant_id);
            }

            pub fn descendants(&self) -> MutexGuard<'_, IdentityHashSet<DeclarationId>> {
                self.descendants.lock().unwrap()
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

            pub fn extend(&mut self, _other: Declaration) {}
        }
    };
}

/// A `Declaration` represents the global concept of an entity in Ruby. For example, the class `Foo` may be defined 3
/// times in different files and the `Foo` declaration is the combination of all of those definitions that contribute to
/// the same fully qualified name
#[derive(Debug)]
pub enum Declaration {
    Class(Box<ClassDeclaration>),
    SingletonClass(Box<SingletonClassDeclaration>),
    Module(Box<ModuleDeclaration>),
    Constant(Box<ConstantDeclaration>),
    Method(Box<MethodDeclaration>),
    GlobalVariable(Box<GlobalVariableDeclaration>),
    InstanceVariable(Box<InstanceVariableDeclaration>),
    ClassVariable(Box<ClassVariableDeclaration>),
}

impl Declaration {
    // Extend this declaration with more definitions by moving `other.definition_ids` inside
    pub fn extend(&mut self, other: Declaration) {
        all_declarations!(self, it => {
            it.definition_ids.extend(other.definitions());
            it.references.extend(other.references());
            it.extend(other);
        });
    }

    #[must_use]
    pub fn name(&self) -> &str {
        all_declarations!(self, it => &it.name)
    }

    #[must_use]
    pub fn kind(&self) -> &'static str {
        match self {
            Declaration::Class(_) => "Class",
            Declaration::SingletonClass(_) => "SingletonClass",
            Declaration::Module(_) => "Module",
            Declaration::Constant(_) => "Constant",
            Declaration::Method(_) => "Method",
            Declaration::GlobalVariable(_) => "GlobalVariable",
            Declaration::InstanceVariable(_) => "InstanceVariable",
            Declaration::ClassVariable(_) => "ClassVariable",
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

    // This will change once we fix fully qualified names to not use `::` as separators for everything. Also, we may
    // want to actually store this in the struct. Currently, it is only used to cleanup a member that got deleted from
    // the graph, so we're avoiding the extra memory cost by computing it on demand.
    #[must_use]
    pub fn unqualified_name(&self) -> String {
        all_declarations!(self, it => it.name.rsplit("::").next().unwrap_or(&it.name).to_string())
    }

    pub fn remove_descendant(declaration: &Declaration, descendant_id: &DeclarationId) {
        match declaration {
            Declaration::Class(c) => c.remove_descendant(descendant_id),
            Declaration::Module(m) => m.remove_descendant(descendant_id),
            Declaration::SingletonClass(s) => s.remove_descendant(descendant_id),
            _ => {}
        }
    }

    pub fn for_each_ancestor<F>(declaration: &Declaration, mut f: F)
    where
        F: FnMut(&Ancestor),
    {
        match declaration {
            Declaration::Class(c) => c.ancestors().iter().for_each(&mut f),
            Declaration::Module(m) => m.ancestors().iter().for_each(&mut f),
            Declaration::SingletonClass(s) => s.ancestors().iter().for_each(&mut f),
            _ => {}
        }
    }

    pub fn for_each_descendant<F>(declaration: &Declaration, mut f: F)
    where
        F: FnMut(&DeclarationId),
    {
        match declaration {
            Declaration::Class(c) => c.descendants().iter().for_each(&mut f),
            Declaration::Module(m) => m.descendants().iter().for_each(&mut f),
            Declaration::SingletonClass(s) => s.descendants().iter().for_each(&mut f),
            _ => {}
        }
    }

    pub fn clear_ancestors(declaration: &Declaration) {
        match declaration {
            Declaration::Class(c) => c.set_ancestors(Ancestors::Partial(vec![])),
            Declaration::Module(m) => m.set_ancestors(Ancestors::Partial(vec![])),
            Declaration::SingletonClass(s) => s.set_ancestors(Ancestors::Partial(vec![])),
            _ => {}
        }
    }

    pub fn clear_descendants(declaration: &Declaration) {
        match declaration {
            Declaration::Class(c) => c.descendants().clear(),
            Declaration::Module(m) => m.descendants().clear(),
            Declaration::SingletonClass(s) => s.descendants().clear(),
            _ => {}
        }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic(expected = "Cannot add the same exact definition to a declaration twice. Duplicate definition IDs")]
    fn inserting_duplicate_definitions() {
        let mut decl = Declaration::Class(Box::new(ClassDeclaration::new(
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
        let decl = Declaration::Class(Box::new(ClassDeclaration::new(
            "Foo".to_string(),
            DeclarationId::from("Object"),
        )));
        let member_name_id = StringId::from("Bar");
        let member_decl_id = DeclarationId::from("Foo::Bar");

        let Declaration::Class(mut class) = decl else {
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
        let decl = Declaration::Class(Box::new(ClassDeclaration::new(
            "Foo".to_string(),
            DeclarationId::from("Foo"),
        )));
        assert_eq!(decl.unqualified_name(), "Foo");

        let decl = Declaration::Class(Box::new(ClassDeclaration::new(
            "Foo::Bar".to_string(),
            DeclarationId::from("Foo"),
        )));
        assert_eq!(decl.unqualified_name(), "Bar");

        let decl = Declaration::Class(Box::new(ClassDeclaration::new(
            "Foo::Bar::baz".to_string(),
            DeclarationId::from("Foo::Bar"),
        )));
        assert_eq!(decl.unqualified_name(), "baz");
    }
}
