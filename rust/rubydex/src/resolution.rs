use std::{
    collections::{HashSet, VecDeque},
    hash::BuildHasher,
};

use crate::model::{
    declaration::{
        Ancestor, Ancestors, ClassDeclaration, ClassVariableDeclaration, ConstantAliasDeclaration, ConstantDeclaration,
        Declaration, GlobalVariableDeclaration, InstanceVariableDeclaration, MethodDeclaration, ModuleDeclaration,
        Namespace, SingletonClassDeclaration,
    },
    definitions::{Definition, Mixin, Receiver},
    graph::{CLASS_ID, Graph, MODULE_ID, OBJECT_ID},
    identity_maps::{IdentityHashMap, IdentityHashSet},
    ids::{DeclarationId, DefinitionId, NameId, ReferenceId, StringId},
    name::{Name, NameRef, ParentScope},
};

pub enum Unit {
    /// A definition that defines a constant and might require resolution
    Definition(DefinitionId),
    /// A constant reference that needs to be resolved
    ConstantRef(ReferenceId),
    /// A list of ancestors that have been partially linearized and need to be retried
    Ancestors(DeclarationId),
}

enum Outcome {
    /// The constant was successfully resolved to the given declaration ID. The second optional tuple element is a
    /// declaration that still needs to have its ancestors linearized
    Resolved(DeclarationId, Option<DeclarationId>),
    /// We had everything we needed to resolved this constant, but we couldn't find it. This means it's not defined (or
    /// defined in a way that static analysis won't discover it). Failing to resolve a constant may also uncovered
    /// ancestors that require linearization, which is the second element
    Unresolved(Option<DeclarationId>),
    /// We couldn't resolve this constant right now because certain dependencies were missing. For example, a constant
    /// reference involved in computing ancestors (like an include) was found, but wasn't resolved yet. We need to place
    /// this back in the queue to retry once we have progressed further. The optional declaration ID is an ancestor that
    /// needs to be linearized before we can retry.
    Retry(Option<DeclarationId>),
}

impl Outcome {
    fn is_resolved_or_retry(&self) -> bool {
        matches!(self, Outcome::Resolved(_, _) | Outcome::Retry(_))
    }
}

struct LinearizationContext {
    descendants: IdentityHashSet<DeclarationId>,
    seen_ids: IdentityHashSet<DeclarationId>,
    cyclic: bool,
    partial: bool,
}

impl LinearizationContext {
    fn new() -> Self {
        Self {
            descendants: IdentityHashSet::default(),
            seen_ids: IdentityHashSet::default(),
            cyclic: false,
            partial: false,
        }
    }
}

pub struct Resolver<'a> {
    graph: &'a mut Graph,
    /// Contains all units of work for resolution, sorted in order for resolution (less complex constant names first)
    unit_queue: VecDeque<Unit>,
    /// Whether we made any progress in the last pass of the resolution loop
    made_progress: bool,
}

impl<'a> Resolver<'a> {
    pub fn new(graph: &'a mut Graph) -> Self {
        Self {
            graph,
            unit_queue: VecDeque::new(),
            made_progress: false,
        }
    }

    /// Runs the resolution phase on the graph. The resolution phase is when 4 main pieces of information are computed:
    ///
    /// 1. Declarations for all definitions
    /// 2. Members and ownership for all declarations
    /// 3. Resolution of all constant references
    /// 4. Inheritance relationships between declarations
    ///
    /// # Panics
    ///
    /// Can panic if there's inconsistent data in the graph
    pub fn resolve_all(&mut self) {
        // TODO: temporary code while we don't have synchronization. We clear all declarations instead of doing the minimal
        // amount of work
        self.graph.clear_declarations();
        // Ensure that Object exists ahead of time so that we can associate top level declarations with the right membership

        {
            self.graph.declarations_mut().insert(
                *OBJECT_ID,
                Declaration::Namespace(Namespace::Class(Box::new(ClassDeclaration::new(
                    "Object".to_string(),
                    *OBJECT_ID,
                )))),
            );
            self.graph.declarations_mut().insert(
                *MODULE_ID,
                Declaration::Namespace(Namespace::Class(Box::new(ClassDeclaration::new(
                    "Module".to_string(),
                    *OBJECT_ID,
                )))),
            );
            self.graph.declarations_mut().insert(
                *CLASS_ID,
                Declaration::Namespace(Namespace::Class(Box::new(ClassDeclaration::new(
                    "Class".to_string(),
                    *OBJECT_ID,
                )))),
            );
        }

        let other_ids = self.prepare_units();

        loop {
            // Flag to ensure the end of the resolution loop. We go through all items in the queue based on its current
            // length. If we made any progress in this pass of the queue, we can continue because we're unlocking more work
            // to be done
            self.made_progress = false;

            // Loop through the current length of the queue, which won't change during this pass. Retries pushed to the back
            // are only processed in the next pass, so that we can assess whether we made any progress
            for _ in 0..self.unit_queue.len() {
                let Some(unit_id) = self.unit_queue.pop_front() else {
                    break;
                };

                match unit_id {
                    Unit::Definition(id) => {
                        self.handle_definition_unit(unit_id, id);
                    }
                    Unit::ConstantRef(id) => {
                        self.handle_reference_unit(unit_id, id);
                    }
                    Unit::Ancestors(id) => {
                        self.handle_ancestor_unit(id);
                    }
                }
            }

            if !self.made_progress || self.unit_queue.is_empty() {
                break;
            }
        }

        self.handle_remaining_definitions(other_ids);
    }

    /// Resolves a single constant against the graph. This method is not meant to be used by the resolution phase, but by
    /// the Ruby API
    pub fn resolve_constant(&mut self, name_id: NameId) -> Option<DeclarationId> {
        match self.resolve_constant_internal(name_id) {
            Outcome::Resolved(id, _) => Some(id),
            Outcome::Unresolved(_) | Outcome::Retry(_) => None,
        }
    }

    /// Handles a unit of work for resolving a constant definition
    fn handle_definition_unit(&mut self, unit_id: Unit, id: DefinitionId) {
        let mut needs_linearization = false;

        let outcome = match self.graph.definitions().get(&id).unwrap() {
            Definition::Class(class) => {
                self.handle_constant_declaration(*class.name_id(), id, false, |name, owner_id| {
                    needs_linearization = true;
                    Declaration::Namespace(Namespace::Class(Box::new(ClassDeclaration::new(name, owner_id))))
                })
            }
            Definition::Module(module) => {
                self.handle_constant_declaration(*module.name_id(), id, false, |name, owner_id| {
                    needs_linearization = true;
                    Declaration::Namespace(Namespace::Module(Box::new(ModuleDeclaration::new(name, owner_id))))
                })
            }
            Definition::Constant(constant) => {
                self.handle_constant_declaration(*constant.name_id(), id, false, |name, owner_id| {
                    Declaration::Constant(Box::new(ConstantDeclaration::new(name, owner_id)))
                })
            }
            Definition::ConstantAlias(alias) => {
                self.handle_constant_declaration(*alias.name_id(), id, false, |name, owner_id| {
                    Declaration::ConstantAlias(Box::new(ConstantAliasDeclaration::new(name, owner_id)))
                })
            }
            Definition::SingletonClass(singleton) => {
                self.handle_constant_declaration(*singleton.name_id(), id, true, |name, owner_id| {
                    needs_linearization = true;
                    Declaration::Namespace(Namespace::SingletonClass(Box::new(SingletonClassDeclaration::new(
                        name, owner_id,
                    ))))
                })
            }
            _ => panic!("Expected constant definitions"),
        };

        match outcome {
            Outcome::Retry(None) => {
                // There might be dependencies we haven't figured out yet, so we need to retry
                self.unit_queue.push_back(unit_id);
            }
            Outcome::Unresolved(None) => {
                // We couldn't resolve this name. Emit a diagnostic
            }
            Outcome::Retry(Some(id_needing_linearization)) | Outcome::Unresolved(Some(id_needing_linearization)) => {
                self.unit_queue.push_back(unit_id);
                self.unit_queue.push_back(Unit::Ancestors(id_needing_linearization));
            }
            Outcome::Resolved(id, None) => {
                if needs_linearization {
                    self.unit_queue.push_back(Unit::Ancestors(id));
                }
                self.made_progress = true;
            }
            Outcome::Resolved(_, Some(id_needing_linearization)) => {
                self.unit_queue.push_back(Unit::Ancestors(id_needing_linearization));
                self.made_progress = true;
            }
        }
    }

    /// Handles a unit of work for resolving a constant reference
    fn handle_reference_unit(&mut self, unit_id: Unit, id: ReferenceId) {
        let constant_ref = self.graph.constant_references().get(&id).unwrap();

        match self.resolve_constant_internal(*constant_ref.name_id()) {
            Outcome::Retry(None) => {
                // There might be dependencies we haven't figured out yet, so we need to retry
                self.unit_queue.push_back(unit_id);
            }
            Outcome::Unresolved(None) => {
                // We couldn't resolve this name. Emit a diagnostic
            }
            Outcome::Retry(Some(id_needing_linearization)) | Outcome::Unresolved(Some(id_needing_linearization)) => {
                self.unit_queue.push_back(unit_id);
                self.unit_queue.push_back(Unit::Ancestors(id_needing_linearization));
            }
            Outcome::Resolved(declaration_id, None) => {
                self.graph.record_resolved_reference(id, declaration_id);
                self.made_progress = true;
            }
            Outcome::Resolved(resolved_id, Some(id_needing_linearization)) => {
                self.graph.record_resolved_reference(id, resolved_id);
                self.made_progress = true;
                self.unit_queue.push_back(Unit::Ancestors(id_needing_linearization));
            }
        }
    }

    /// Handles a unit of work for linearizing ancestors of a declaration
    fn handle_ancestor_unit(&mut self, id: DeclarationId) {
        match self.ancestors_of(id) {
            Ancestors::Complete(_) | Ancestors::Cyclic(_) => {
                // We succeeded in some capacity this time
                self.made_progress = true;
            }
            Ancestors::Partial(_) => {
                // We still couldn't linearize ancestors, but there's a chance that this will succeed next time. We
                // re-enqueue for another try, but we don't consider it as making progress
                self.unit_queue.push_back(Unit::Ancestors(id));
            }
        }
    }

    /// Handle other definitions that don't require resolution, but need to have their declarations and membership created
    #[allow(clippy::too_many_lines)]
    fn handle_remaining_definitions(&mut self, other_ids: Vec<DefinitionId>) {
        for id in other_ids {
            match self.graph.definitions().get(&id).unwrap() {
                Definition::Method(method_definition) => {
                    let str_id = *method_definition.str_id();
                    let owner_id = match method_definition.receiver() {
                        Some(Receiver::SelfReceiver(def_id)) => {
                            let Some(&owner_decl_id) = self.graph.definition_id_to_declaration_id(*def_id) else {
                                // The enclosing class/module couldn't be resolved (e.g., `class Foo::Bar` where
                                // `Foo` is undefined). The method is orphaned as a consequence.
                                continue;
                            };
                            self.get_or_create_singleton_class(owner_decl_id)
                                .expect("Methods with self receiver should always be inside a namespace")
                        }
                        Some(Receiver::ConstantReceiver(name_id)) => {
                            let mut receiver_decl_id = match self.graph.names().get(name_id).unwrap() {
                                NameRef::Resolved(resolved) => *resolved.declaration_id(),
                                NameRef::Unresolved(_) => {
                                    continue;
                                }
                            };

                            let receiver_decl = self.graph.declarations().get(&receiver_decl_id).unwrap();

                            // If the method receiver is a constant alias, it needs to point to a namespace or else we don't have a place to declare the method
                            if matches!(receiver_decl, Declaration::ConstantAlias(_)) {
                                let resolved_ids = self.resolve_alias_chains(receiver_decl_id);
                                let Some(namespace) = resolved_ids
                                    .iter()
                                    .find(|id| self.graph.declarations().get(id).unwrap().as_namespace().is_some())
                                else {
                                    continue;
                                };

                                receiver_decl_id = *namespace;
                            }

                            let Some(singleton_id) = self.get_or_create_singleton_class(receiver_decl_id) else {
                                continue;
                            };

                            singleton_id
                        }
                        None => self.resolve_lexical_owner(*method_definition.lexical_nesting_id()),
                    };

                    self.create_declaration(str_id, id, owner_id, |name| {
                        Declaration::Method(Box::new(MethodDeclaration::new(name, owner_id)))
                    });
                }
                Definition::AttrAccessor(attr) => {
                    let owner_id = self.resolve_lexical_owner(*attr.lexical_nesting_id());

                    self.create_declaration(*attr.str_id(), id, owner_id, |name| {
                        Declaration::Method(Box::new(MethodDeclaration::new(name, owner_id)))
                    });
                }
                Definition::AttrReader(attr) => {
                    let owner_id = self.resolve_lexical_owner(*attr.lexical_nesting_id());

                    self.create_declaration(*attr.str_id(), id, owner_id, |name| {
                        Declaration::Method(Box::new(MethodDeclaration::new(name, owner_id)))
                    });
                }
                Definition::AttrWriter(attr) => {
                    let owner_id = self.resolve_lexical_owner(*attr.lexical_nesting_id());

                    self.create_declaration(*attr.str_id(), id, owner_id, |name| {
                        Declaration::Method(Box::new(MethodDeclaration::new(name, owner_id)))
                    });
                }
                Definition::GlobalVariable(var) => {
                    let owner_id = *OBJECT_ID;
                    let str_id = *var.str_id();
                    let name = self.graph.strings().get(&str_id).unwrap().as_str().to_string();

                    let declaration_id = self.graph.add_declaration(id, name, |fully_qualified_name| {
                        Declaration::GlobalVariable(Box::new(GlobalVariableDeclaration::new(
                            fully_qualified_name,
                            owner_id,
                        )))
                    });
                    self.graph.add_member(&owner_id, declaration_id, str_id);
                }
                Definition::InstanceVariable(var) => {
                    let str_id = *var.str_id();

                    // Top-level instance variables belong to the `<main>` object, not `Object`.
                    // We can't represent `<main>` yet, so skip creating declarations for these.
                    // TODO: Make sure we introduce `<main>` representation later and update this
                    let Some(nesting_id) = *var.lexical_nesting_id() else {
                        continue;
                    };

                    let Some(nesting_def) = self.graph.definitions().get(&nesting_id) else {
                        continue;
                    };

                    match nesting_def {
                        // When the instance variable is inside a method body, we determine the owner based on the method's receiver
                        Definition::Method(method) => {
                            if let Some(receiver) = method.receiver() {
                                let receiver_decl_id = match receiver {
                                    Receiver::SelfReceiver(def_id) => *self
                                        .graph
                                        .definition_id_to_declaration_id(*def_id)
                                        .expect("SelfReceiver definition should have a declaration"),
                                    Receiver::ConstantReceiver(name_id) => {
                                        let Some(NameRef::Resolved(resolved)) = self.graph.names().get(name_id) else {
                                            continue;
                                        };

                                        let mut receiver_decl_id = *resolved.declaration_id();
                                        let receiver_decl = self.graph.declarations().get(&receiver_decl_id).unwrap();

                                        // If the method receiver is a constant alias, it needs to point to a namespace or else we don't have a place to declare the method
                                        if matches!(receiver_decl, Declaration::ConstantAlias(_)) {
                                            let resolved_ids = self.resolve_alias_chains(receiver_decl_id);
                                            let Some(namespace) = resolved_ids.iter().find(|id| {
                                                self.graph.declarations().get(id).unwrap().as_namespace().is_some()
                                            }) else {
                                                continue;
                                            };

                                            receiver_decl_id = *namespace;
                                        }

                                        receiver_decl_id
                                    }
                                };

                                // Instance variable in singleton method - owned by the receiver's singleton class
                                let Some(owner_id) = self.get_or_create_singleton_class(receiver_decl_id) else {
                                    continue;
                                };
                                {
                                    debug_assert!(
                                        matches!(
                                            self.graph.declarations().get(&owner_id),
                                            Some(Declaration::Namespace(Namespace::SingletonClass(_)))
                                        ),
                                        "Instance variable in singleton method should be owned by a SingletonClass"
                                    );
                                }
                                self.create_declaration(str_id, id, owner_id, |name| {
                                    Declaration::InstanceVariable(Box::new(InstanceVariableDeclaration::new(
                                        name, owner_id,
                                    )))
                                });
                                continue;
                            }

                            // If the method has no explicit receiver, we resolve the owner based on the lexical nesting
                            let method_owner_id = self.resolve_lexical_owner(*method.lexical_nesting_id());

                            // If the method is in a singleton class, the instance variable belongs to the class object
                            // Like `class << Foo; def bar; @bar = 1; end; end`, where `@bar` is owned by `Foo::<Foo>`
                            if let Some(decl) = self.graph.declarations().get(&method_owner_id)
                                && matches!(decl, Declaration::Namespace(Namespace::SingletonClass(_)))
                            {
                                // Method in singleton class - owner is the singleton class itself
                                self.create_declaration(str_id, id, method_owner_id, |name| {
                                    Declaration::InstanceVariable(Box::new(InstanceVariableDeclaration::new(
                                        name,
                                        method_owner_id,
                                    )))
                                });
                            } else {
                                // Regular instance method
                                // Create an instance variable declaration for the method's owner
                                self.create_declaration(str_id, id, method_owner_id, |name| {
                                    Declaration::InstanceVariable(Box::new(InstanceVariableDeclaration::new(
                                        name,
                                        method_owner_id,
                                    )))
                                });
                            }
                        }
                        // If the instance variable is directly in a class/module body, it belongs to the class object
                        // and is owned by the singleton class of that class/module
                        Definition::Class(_) | Definition::Module(_) => {
                            let nesting_decl_id = self
                                .graph
                                .definition_id_to_declaration_id(nesting_id)
                                .copied()
                                .unwrap_or(*OBJECT_ID);
                            let owner_id = self
                                .get_or_create_singleton_class(nesting_decl_id)
                                .expect("class/module nesting should always be a namespace");
                            {
                                debug_assert!(
                                    matches!(
                                        self.graph.declarations().get(&owner_id),
                                        Some(Declaration::Namespace(Namespace::SingletonClass(_)))
                                    ),
                                    "Instance variable in class/module body should be owned by a SingletonClass"
                                );
                            }
                            self.create_declaration(str_id, id, owner_id, |name| {
                                Declaration::InstanceVariable(Box::new(InstanceVariableDeclaration::new(
                                    name, owner_id,
                                )))
                            });
                        }
                        // If in a singleton class body directly, the owner is the singleton class's singleton class
                        // Like `class << Foo; @bar = 1; end`, where `@bar` is owned by `Foo::<Foo>::<<Foo>>`
                        Definition::SingletonClass(_) => {
                            let singleton_class_decl_id = self
                                .graph
                                .definition_id_to_declaration_id(nesting_id)
                                .copied()
                                .unwrap_or(*OBJECT_ID);
                            let owner_id = self
                                .get_or_create_singleton_class(singleton_class_decl_id)
                                .expect("singleton class nesting should always be a namespace");
                            {
                                debug_assert!(
                                    matches!(
                                        self.graph.declarations().get(&owner_id),
                                        Some(Declaration::Namespace(Namespace::SingletonClass(_)))
                                    ),
                                    "Instance variable in singleton class body should be owned by a SingletonClass"
                                );
                            }
                            self.create_declaration(str_id, id, owner_id, |name| {
                                Declaration::InstanceVariable(Box::new(InstanceVariableDeclaration::new(
                                    name, owner_id,
                                )))
                            });
                        }
                        _ => {
                            panic!("Unexpected lexical nesting for instance variable: {nesting_def:?}");
                        }
                    }
                }
                Definition::ClassVariable(var) => {
                    // TODO: add diagnostic on the else branch. Defining class variables at the top level crashes
                    if let Some(owner_id) = self.resolve_class_variable_owner(*var.lexical_nesting_id()) {
                        self.create_declaration(*var.str_id(), id, owner_id, |name| {
                            Declaration::ClassVariable(Box::new(ClassVariableDeclaration::new(name, owner_id)))
                        });
                    }
                }
                Definition::MethodAlias(alias) => {
                    let owner_id = self.resolve_lexical_owner(*alias.lexical_nesting_id());

                    self.create_declaration(*alias.new_name_str_id(), id, owner_id, |name| {
                        Declaration::Method(Box::new(MethodDeclaration::new(name, owner_id)))
                    });
                }
                Definition::GlobalVariableAlias(alias) => {
                    self.create_declaration(*alias.new_name_str_id(), id, *OBJECT_ID, |name| {
                        Declaration::GlobalVariable(Box::new(GlobalVariableDeclaration::new(name, *OBJECT_ID)))
                    });
                }
                Definition::Class(_)
                | Definition::SingletonClass(_)
                | Definition::Module(_)
                | Definition::Constant(_)
                | Definition::ConstantAlias(_) => {
                    panic!("Unexpected definition type in non-constant resolution. This shouldn't happen")
                }
            }
        }
    }

    fn create_declaration<F>(
        &mut self,
        str_id: StringId,
        definition_id: DefinitionId,
        owner_id: DeclarationId,
        declaration_builder: F,
    ) where
        F: FnOnce(String) -> Declaration,
    {
        let fully_qualified_name = {
            let owner = self.graph.declarations().get(&owner_id).unwrap();
            let name_str = self.graph.strings().get(&str_id).unwrap();
            format!("{}#{}", owner.name(), name_str.as_str())
        };

        let declaration_id = self
            .graph
            .add_declaration(definition_id, fully_qualified_name, declaration_builder);
        self.graph.add_member(&owner_id, declaration_id, str_id);
    }

    /// Resolves owner for class variables, bypassing singleton classes.
    fn resolve_class_variable_owner(&self, lexical_nesting_id: Option<DefinitionId>) -> Option<DeclarationId> {
        let mut current_nesting = lexical_nesting_id;
        while let Some(nesting_id) = current_nesting {
            if let Some(nesting_def) = self.graph.definitions().get(&nesting_id)
                && matches!(nesting_def, Definition::SingletonClass(_))
            {
                current_nesting = *nesting_def.lexical_nesting_id();
            } else {
                break;
            }
        }
        current_nesting.and_then(|id| self.graph.definition_id_to_declaration_id(id).copied())
    }

    /// Resolves owner from lexical nesting.
    fn resolve_lexical_owner(&self, lexical_nesting_id: Option<DefinitionId>) -> DeclarationId {
        let Some(id) = lexical_nesting_id else {
            return *OBJECT_ID;
        };

        // If no declaration exists yet for this definition, walk up the lexical chain.
        // This handles the case where attr_* definitions inside methods are processed
        // before the method definition itself.
        let Some(declaration_id) = self.graph.definition_id_to_declaration_id(id) else {
            let definition = self.graph.definitions().get(&id).unwrap();
            return self.resolve_lexical_owner(*definition.lexical_nesting_id());
        };

        let declarations = self.graph.declarations();

        // If the associated declaration is a namespace that can own things, we found the right owner. Otherwise, we might
        // have found something nested inside something else (like a method), in which case we have to recurse until we find
        // the appropriate owner
        if matches!(
            declarations.get(declaration_id).unwrap(),
            Declaration::Namespace(Namespace::Class(_) | Namespace::Module(_) | Namespace::SingletonClass(_))
        ) {
            *declaration_id
        } else {
            let definition = self.graph.definitions().get(&id).unwrap();
            self.resolve_lexical_owner(*definition.lexical_nesting_id())
        }
    }

    /// Gets or creates a singleton class declaration for a given class/module declaration.  For class `Foo`, this
    /// returns the declaration for `Foo::<Foo>`.
    ///
    /// If the declaration is a `Constant` with all-promotable definitions, it is automatically promoted to a `Class`
    /// namespace before creating the singleton. Returns `None` if the declaration is not a namespace and cannot be
    /// promoted (e.g., `FOO = 42`).
    fn get_or_create_singleton_class(&mut self, attached_id: DeclarationId) -> Option<DeclarationId> {
        let attached_decl = self.graph.declarations().get(&attached_id).unwrap();

        if matches!(attached_decl, Declaration::Constant(_)) {
            if self.graph.all_definitions_promotable(attached_decl) {
                self.graph.promote_constant_to_namespace(attached_id, |name, owner_id| {
                    Declaration::Namespace(Namespace::Module(Box::new(ModuleDeclaration::new(name, owner_id))))
                });

                self.unit_queue.push_back(Unit::Ancestors(attached_id));
            } else {
                return None;
            }
        }

        let attached_decl = self.graph.declarations_mut().get_mut(&attached_id).unwrap();
        let fully_qualified_name = format!("{}::<{}>", attached_decl.name(), attached_decl.unqualified_name());

        let namespace_decl = attached_decl
            .as_namespace_mut()
            .expect("constants are handled above; all other callers pass namespace declarations");

        if let Some(singleton_id) = namespace_decl.singleton_class() {
            return Some(*singleton_id);
        }

        let decl_id = DeclarationId::from(&fully_qualified_name);
        namespace_decl.set_singleton_class_id(decl_id);

        self.graph.declarations_mut().insert(
            decl_id,
            Declaration::Namespace(Namespace::SingletonClass(Box::new(SingletonClassDeclaration::new(
                fully_qualified_name,
                attached_id,
            )))),
        );

        Some(decl_id)
    }

    /// Linearizes the ancestors of a declaration, returning the list of ancestor declaration IDs
    ///
    /// # Panics
    ///
    /// Can panic if there's inconsistent data in the graph
    #[must_use]
    fn ancestors_of(&mut self, declaration_id: DeclarationId) -> Ancestors {
        let mut context = LinearizationContext::new();
        self.linearize_ancestors(declaration_id, &mut context)
    }

    /// Linearizes the ancestors of a declaration, returning the list of ancestor declaration IDs
    ///
    /// # Panics
    ///
    /// Can panic if there's inconsistent data in the graph
    #[must_use]
    fn linearize_ancestors(&mut self, declaration_id: DeclarationId, context: &mut LinearizationContext) -> Ancestors {
        {
            let declaration = self.graph.declarations_mut().get_mut(&declaration_id).unwrap();

            // Add this declaration to the descendants so that we capture transitive descendant relationships
            context.descendants.insert(declaration_id);

            // Return the cached ancestors if we already computed them. If they are partial ancestors, ignore the cache to try
            // again
            if declaration.as_namespace().unwrap().has_complete_ancestors() {
                let cached = declaration.as_namespace().unwrap().clone_ancestors();
                self.propagate_descendants(&mut context.descendants, &cached);
                context.descendants.remove(&declaration_id);
                return cached;
            }

            if !context.seen_ids.insert(declaration_id) {
                // If we find a cycle when linearizing ancestors, it's an error that the programmer must fix. However, we try to
                // still approximate features by assuming that it must inherit from `Object` at some point (which is what most
                // classes/modules inherit from). This is not 100% correct, but it allows us to provide a bit better IDE support
                // for these cases
                let estimated_ancestors = if matches!(declaration, Declaration::Namespace(Namespace::Class(_))) {
                    Ancestors::Cyclic(vec![Ancestor::Complete(*OBJECT_ID)])
                } else {
                    Ancestors::Cyclic(vec![])
                };
                declaration
                    .as_namespace_mut()
                    .unwrap()
                    .set_ancestors(estimated_ancestors.clone());
                context.descendants.remove(&declaration_id);
                return estimated_ancestors;
            }

            // Automatically track descendants as we recurse. This has to happen before checking the cache since we may have
            // already linearized the parent's ancestors, but it's the first time we're discovering the descendant
            for descendant in &context.descendants {
                self.graph
                    .declarations_mut()
                    .get_mut(&declaration_id)
                    .unwrap()
                    .as_namespace_mut()
                    .unwrap()
                    .add_descendant(*descendant);
            }
        }

        let parent_ancestors = self.linearize_parent_ancestors(declaration_id, context);
        let declaration = self.graph.declarations().get(&declaration_id).unwrap();
        let mut mixins = Vec::new();

        // If we're linearizing a singleton class, add the extends of the attached class to the list of mixins to process
        if let Declaration::Namespace(Namespace::SingletonClass(_)) = declaration {
            let attached_decl = self.graph.declarations().get(declaration.owner_id()).unwrap();

            mixins.extend(
                attached_decl
                    .definitions()
                    .iter()
                    .filter_map(|definition_id| self.mixins_of(*definition_id))
                    .flatten()
                    .filter(|mixin| matches!(mixin, Mixin::Extend(_))),
            );
        }

        // Consider only prepends and includes for the current declaration
        mixins.extend(
            declaration
                .definitions()
                .iter()
                .filter_map(|definition_id| self.mixins_of(*definition_id))
                .flatten()
                .filter(|mixin| matches!(mixin, Mixin::Prepend(_) | Mixin::Include(_))),
        );

        let (linearized_prepends, linearized_includes) =
            self.linearize_mixins(context, mixins, parent_ancestors.as_ref());

        // Build the final list
        let mut ancestors = Vec::new();
        ancestors.extend(linearized_prepends);
        ancestors.push(Ancestor::Complete(declaration_id));
        ancestors.extend(linearized_includes);
        if let Some(parents) = parent_ancestors {
            ancestors.extend(parents);
        }

        let result = if context.cyclic {
            Ancestors::Cyclic(ancestors)
        } else if context.partial {
            Ancestors::Partial(ancestors)
        } else {
            Ancestors::Complete(ancestors)
        };
        self.graph
            .declarations_mut()
            .get_mut(&declaration_id)
            .unwrap()
            .as_namespace_mut()
            .unwrap()
            .set_ancestors(result.clone());

        context.descendants.remove(&declaration_id);
        result
    }

    fn linearize_parent_ancestors(
        &mut self,
        declaration_id: DeclarationId,
        context: &mut LinearizationContext,
    ) -> Option<Vec<Ancestor>> {
        if declaration_id == *OBJECT_ID {
            return None;
        }

        let declaration = self.graph.declarations().get(&declaration_id).unwrap();

        match declaration {
            Declaration::Namespace(Namespace::Class(_)) => {
                let definition_ids = declaration.definitions().to_vec();

                Some(match self.linearize_parent_class(&definition_ids, context) {
                    Ancestors::Complete(ids) => ids,
                    Ancestors::Cyclic(ids) => {
                        context.cyclic = true;
                        ids
                    }
                    Ancestors::Partial(ids) => {
                        context.partial = true;
                        ids
                    }
                })
            }
            Declaration::Namespace(Namespace::SingletonClass(_)) => {
                let owner_id = *declaration.owner_id();

                let (singleton_parent_id, partial_singleton) = self.singleton_parent_id(owner_id);
                if partial_singleton {
                    context.partial = true;
                }

                Some(match self.linearize_ancestors(singleton_parent_id, context) {
                    Ancestors::Complete(ids) => ids,
                    Ancestors::Cyclic(ids) => {
                        context.cyclic = true;
                        ids
                    }
                    Ancestors::Partial(ids) => {
                        context.partial = true;
                        ids
                    }
                })
            }
            _ => None,
        }
    }

    /// Linearize all mixins into a prepend and include list. This function requires the parent ancestors because included
    /// modules are deduplicated against them
    fn linearize_mixins(
        &mut self,
        context: &mut LinearizationContext,
        mixins: Vec<Mixin>,
        parent_ancestors: Option<&Vec<Ancestor>>,
    ) -> (VecDeque<Ancestor>, VecDeque<Ancestor>) {
        let mut linearized_prepends = VecDeque::new();
        let mut linearized_includes = VecDeque::new();

        // IMPORTANT! In the slice of mixins we receive, extends are the ones that occurred in the attached object, which we
        // collect ahead of time. This is the reason why we apparently treat an extend like an include, because an extend in
        // the attached object is equivalent to an include in the singleton class
        for mixin in mixins {
            let constant_reference = self
                .graph
                .constant_references()
                .get(mixin.constant_reference_id())
                .unwrap();

            match mixin {
                Mixin::Prepend(_) => {
                    match self.graph.names().get(constant_reference.name_id()).unwrap() {
                        NameRef::Resolved(resolved) => {
                            let declaration_id = resolved.declaration_id();

                            // Skip if the mixin is not a namespace (e.g.: dynamically defined class or module that we
                            // misidentified as a constant)
                            if !self.graph.is_namespace(declaration_id) {
                                continue;
                            }

                            let ids = match self.linearize_ancestors(*declaration_id, context) {
                                Ancestors::Complete(ids) => ids,
                                Ancestors::Cyclic(ids) => {
                                    context.cyclic = true;
                                    ids
                                }
                                Ancestors::Partial(ids) => {
                                    context.partial = true;
                                    ids
                                }
                            };

                            // Only reorder if there are new modules to add. If all modules being
                            // prepended are already in the chain (e.g., `prepend A` when A is already
                            // prepended via B), Ruby treats it as a no-op and keeps the existing order.
                            if ids.iter().any(|id| !linearized_prepends.contains(id)) {
                                // Remove existing entries that will be re-added from the new chain
                                linearized_prepends.retain(|id| !ids.contains(id));

                                for id in ids.into_iter().rev() {
                                    linearized_prepends.push_front(id);
                                }
                            }
                        }
                        NameRef::Unresolved(_) => {
                            // We haven't been able to resolve this name yet, so we push it as a partial linearization to finish
                            // later
                            context.partial = true;
                            linearized_prepends.push_front(Ancestor::Partial(*constant_reference.name_id()));
                        }
                    }
                }
                Mixin::Include(_) | Mixin::Extend(_) => {
                    match self.graph.names().get(constant_reference.name_id()).unwrap() {
                        NameRef::Resolved(resolved) => {
                            let declaration_id = resolved.declaration_id();

                            // Skip if the mixin is not a namespace (e.g.: dynamically defined class or module that we
                            // misidentified as a constant)
                            if !self.graph.is_namespace(declaration_id) {
                                continue;
                            }

                            let mut ids = match self.linearize_ancestors(*declaration_id, context) {
                                Ancestors::Complete(ids) => ids,
                                Ancestors::Cyclic(ids) => {
                                    context.cyclic = true;
                                    ids
                                }
                                Ancestors::Partial(ids) => {
                                    context.partial = true;
                                    ids
                                }
                            };

                            // Prepended module are deduped based only on other prepended modules
                            ids.retain(|id| {
                                !linearized_prepends.contains(id)
                                    && !linearized_includes.contains(id)
                                    && parent_ancestors
                                        .as_ref()
                                        .is_none_or(|parent_ids| !parent_ids.contains(id))
                            });

                            for id in ids.into_iter().rev() {
                                linearized_includes.push_front(id);
                            }
                        }
                        NameRef::Unresolved(_) => {
                            // We haven't been able to resolve this name yet, so we push it as a partial linearization to finish
                            // later
                            context.partial = true;
                            linearized_includes.push_front(Ancestor::Partial(*constant_reference.name_id()));
                        }
                    }
                }
            }
        }

        (linearized_prepends, linearized_includes)
    }

    /// Propagate descendants to all cached ancestors
    fn propagate_descendants<S: BuildHasher>(
        &mut self,
        descendants: &mut HashSet<DeclarationId, S>,
        cached: &Ancestors,
    ) {
        if !descendants.is_empty() {
            for ancestor in cached {
                if let Ancestor::Complete(ancestor_id) = ancestor {
                    for descendant in descendants.iter() {
                        self.graph
                            .declarations_mut()
                            .get_mut(ancestor_id)
                            .unwrap()
                            .as_namespace_mut()
                            .unwrap()
                            .add_descendant(*descendant);
                    }
                }
            }
        }
    }

    // Handles the resolution of the namespace name, the creation of the declaration and membership
    fn handle_constant_declaration<F>(
        &mut self,
        name_id: NameId,
        definition_id: DefinitionId,
        singleton: bool,
        declaration_builder: F,
    ) -> Outcome
    where
        F: FnOnce(String, DeclarationId) -> Declaration,
    {
        let name_ref = self.graph.names().get(&name_id).unwrap();
        let str_id = *name_ref.str();

        // The name of the declaration is determined by the name of its owner, which may or may not require resolution
        // depending on whether the name has a parent scope
        match self.name_owner_id(name_id) {
            Outcome::Resolved(owner_id, id_needing_linearization) => {
                let mut fully_qualified_name = self.graph.strings().get(&str_id).unwrap().to_string();

                let owner = self.graph.declarations().get(&owner_id).unwrap();
                let owner_is_namespace = owner.as_namespace().is_some();

                // We don't prefix declarations with `Object::`
                if owner_id != *OBJECT_ID {
                    fully_qualified_name.insert_str(0, "::");
                    fully_qualified_name.insert_str(0, owner.name());
                }

                let declaration_id =
                    self.graph
                        .add_declaration(definition_id, fully_qualified_name, |fully_qualified_name| {
                            declaration_builder(fully_qualified_name, owner_id)
                        });

                if owner_is_namespace {
                    if singleton {
                        self.graph
                            .declarations_mut()
                            .get_mut(&owner_id)
                            .unwrap()
                            .as_namespace_mut()
                            .unwrap()
                            .set_singleton_class_id(declaration_id);
                    } else {
                        self.graph.add_member(&owner_id, declaration_id, str_id);
                    }
                }

                self.graph.record_resolved_name(name_id, declaration_id);
                Outcome::Resolved(declaration_id, id_needing_linearization)
            }
            other => other,
        }
    }

    // Returns the owner declaration ID for a given name. If the name is simple and has no parent scope, then the owner is
    // either the nesting or Object. If the name has a parent scope, we attempt to resolve the reference and that should be
    // the name's owner. For aliases, resolves through to get the actual namespace.
    fn name_owner_id(&mut self, name_id: NameId) -> Outcome {
        let name_ref = self.graph.names().get(&name_id).unwrap();

        if let Some(parent_scope) = name_ref.parent_scope().as_ref() {
            // If we have `A::B`, the owner of `B` is whatever `A` resolves to.
            // If `A` is an alias, resolve through to get the actual namespace.
            match self.resolve_constant_internal(*parent_scope) {
                Outcome::Resolved(id, linearization) => self.resolve_to_primary_namespace(id, linearization),
                other => other,
            }
        } else if let Some(nesting_id) = name_ref.nesting()
            && !name_ref.parent_scope().is_top_level()
        {
            // Lexical nesting from block structure, e.g.:
            //   class ALIAS::Target
            //     CONST = 1  # CONST's nesting is the class, which may resolve to an alias target
            //   end
            // If `ALIAS` points to `Outer`, `CONST` should be owned by `Outer::Target`, not `ALIAS::Target`.
            match self.graph.names().get(nesting_id).unwrap() {
                NameRef::Resolved(resolved) => self.resolve_to_primary_namespace(*resolved.declaration_id(), None),
                NameRef::Unresolved(_) => {
                    // The only case where we wouldn't have the nesting resolved at this point is if it's available through
                    // inheritance or if it doesn't exist, so we need to retry later
                    Outcome::Retry(None)
                }
            }
        } else {
            // Any constants at the top level are owned by Object
            Outcome::Resolved(*OBJECT_ID, None)
        }
    }

    /// Resolves a declaration ID through any alias chain to get the primary (first) namespace.
    /// Returns `Retry` if the primary alias target hasn't been resolved yet.
    fn resolve_to_primary_namespace(
        &self,
        declaration_id: DeclarationId,
        linearization: Option<DeclarationId>,
    ) -> Outcome {
        let resolved_ids = self.resolve_alias_chains(declaration_id);

        // Get the primary (first) resolved target
        let Some(&primary_id) = resolved_ids.first() else {
            return Outcome::Retry(None);
        };

        // Check if the primary result is still an unresolved alias
        if matches!(
            self.graph.declarations().get(&primary_id),
            Some(Declaration::ConstantAlias(_))
        ) {
            return Outcome::Retry(None);
        }

        Outcome::Resolved(primary_id, linearization)
    }

    /// Attempts to resolve a constant reference against the graph. Returns the fully qualified declaration ID that the
    /// reference is related to or `None`. This method mutates the graph to remember which constants have already been
    /// resolved
    fn resolve_constant_internal(&mut self, name_id: NameId) -> Outcome {
        let name_ref = self.graph.names().get(&name_id).unwrap().clone();

        match name_ref {
            NameRef::Unresolved(name) => {
                match name.parent_scope() {
                    ParentScope::TopLevel => {
                        let result = self.search_top_level(*name.str());

                        if let Outcome::Resolved(declaration_id, _) = result {
                            self.graph.record_resolved_name(name_id, declaration_id);
                        }

                        result
                    }
                    ParentScope::Attached(parent_scope_id) => {
                        let NameRef::Resolved(parent_scope) = self.graph.names().get(parent_scope_id).unwrap() else {
                            return Outcome::Retry(None);
                        };

                        let mut target_decl_id = *parent_scope.declaration_id();
                        let target_decl = self.graph.declarations().get(&target_decl_id).unwrap();

                        // If the attached object is a constant alias, resolve it to the target namespace
                        // (e.g., ALIAS.bar where ALIAS = Foo should create the singleton class on Foo, not ALIAS)
                        if matches!(target_decl, Declaration::ConstantAlias(_)) {
                            let resolved_ids = self.resolve_alias_chains(target_decl_id);

                            if resolved_ids.iter().any(|id| {
                                matches!(self.graph.declarations().get(id), Some(Declaration::ConstantAlias(_)))
                            }) {
                                return Outcome::Retry(None);
                            }

                            let Some(&namespace_id) = resolved_ids.iter().find(|id| {
                                matches!(self.graph.declarations().get(id), Some(Declaration::Namespace(_)))
                            }) else {
                                return Outcome::Unresolved(None);
                            };

                            target_decl_id = namespace_id;
                        }

                        // If we found a singleton reference with a resolved attached object parent scope, we
                        // automatically create the singleton class
                        let Some(singleton_id) = self.get_or_create_singleton_class(target_decl_id) else {
                            return Outcome::Unresolved(None);
                        };
                        self.graph.record_resolved_name(name_id, singleton_id);
                        Outcome::Resolved(singleton_id, Some(singleton_id))
                    }
                    ParentScope::None => {
                        // Otherwise, it's a simple constant read and we can resolve it directly
                        let result = self.run_resolution(&name);

                        if let Outcome::Resolved(declaration_id, _) = result {
                            self.graph.record_resolved_name(name_id, declaration_id);
                        }

                        result
                    }
                    ParentScope::Some(parent_scope_id) => {
                        let NameRef::Resolved(parent_scope) = self.graph.names().get(parent_scope_id).unwrap() else {
                            return Outcome::Retry(None);
                        };

                        // Resolve the namespace in case it's an alias (e.g., ALIAS::CONST where ALIAS = Foo)
                        // An alias can have multiple targets, so we try all of them in order.
                        let resolved_ids = self.resolve_alias_chains(*parent_scope.declaration_id());

                        // Search each resolved target for the constant. Return early if found.
                        let mut missing_linearization_id = None;
                        let mut found_namespace = false;

                        for &id in &resolved_ids {
                            match self.graph.declarations().get(&id) {
                                Some(Declaration::ConstantAlias(_)) => {
                                    // Alias not fully resolved yet
                                    return Outcome::Retry(None);
                                }
                                Some(Declaration::Namespace(_)) => {
                                    found_namespace = true;

                                    match self.search_ancestors(id, *name.str()) {
                                        Outcome::Resolved(declaration_id, missing_linearization_id) => {
                                            self.graph.record_resolved_name(name_id, declaration_id);
                                            return Outcome::Resolved(declaration_id, missing_linearization_id);
                                        }
                                        Outcome::Retry(Some(needs_linearization_id))
                                        | Outcome::Unresolved(Some(needs_linearization_id)) => {
                                            missing_linearization_id.get_or_insert(needs_linearization_id);
                                        }
                                        Outcome::Unresolved(None) => {}
                                        Outcome::Retry(_) => unreachable!("search_ancestors never returns Retry"),
                                    }
                                }
                                _ => {
                                    // Not a namespace (e.g., a constant) - skip
                                }
                            }
                        }

                        // If no namespaces were found, this constant path can never resolve.
                        if !found_namespace {
                            return Outcome::Unresolved(None);
                        }

                        // Member not found in any namespace yet - retry in case it's added later
                        missing_linearization_id.map_or(Outcome::Retry(None), |id| Outcome::Unresolved(Some(id)))
                    }
                }
            }
            NameRef::Resolved(resolved) => Outcome::Resolved(*resolved.declaration_id(), None),
        }
    }

    /// Resolves an alias chain to get all possible final target declarations.
    /// Returns the original ID if it's not an alias or if the target hasn't been resolved yet.
    ///
    /// When an alias has multiple definitions with different targets (e.g., conditional assignment),
    /// this returns all possible final targets.
    fn resolve_alias_chains(&self, declaration_id: DeclarationId) -> Vec<DeclarationId> {
        let mut results = Vec::new();
        let mut queue = VecDeque::from([declaration_id]);
        let mut seen = HashSet::new();

        // Use BFS (pop_front) to preserve the order of alias targets.
        // The first target of an alias should remain the first/primary result.
        while let Some(current) = queue.pop_front() {
            if !seen.insert(current) {
                // Already processed or cycle detected
                continue;
            }

            match self.graph.declarations().get(&current) {
                Some(Declaration::ConstantAlias(_)) => {
                    let targets = self.graph.alias_targets(&current).unwrap_or_default();
                    if targets.is_empty() {
                        // Target not resolved yet, keep the alias for retry
                        results.push(current);
                    } else {
                        queue.extend(targets);
                    }
                }
                Some(_) => {
                    // Not an alias, this is a final target
                    results.push(current);
                }
                None => {
                    panic!("Declaration {current:?} not found in graph");
                }
            }
        }

        results
    }

    fn run_resolution(&mut self, name: &Name) -> Outcome {
        let str_id = *name.str();
        let mut missing_linearization_id = None;

        if let Some(nesting) = name.nesting() {
            let scope_outcome = self.search_lexical_scopes(name, str_id);

            // If we already resolved or need to retry, return early
            if scope_outcome.is_resolved_or_retry() {
                return scope_outcome;
            }

            // Search inheritance chain
            let ancestor_outcome = match self.graph.names().get(nesting).unwrap() {
                NameRef::Resolved(nesting_name_ref) => {
                    self.search_ancestors(*nesting_name_ref.declaration_id(), str_id)
                }
                NameRef::Unresolved(_) => Outcome::Retry(None),
            };
            match ancestor_outcome {
                Outcome::Resolved(_, _) | Outcome::Retry(None) => return ancestor_outcome,
                Outcome::Retry(Some(needs_linearization_id)) | Outcome::Unresolved(Some(needs_linearization_id)) => {
                    missing_linearization_id = Some(needs_linearization_id);
                }
                Outcome::Unresolved(None) => {}
            }
        }

        // If it's a top level reference starting with `::` or if we didn't find the constant anywhere else, the
        // fallback is the top level
        let outcome = self.search_top_level(str_id);

        if let Some(linearization_id) = missing_linearization_id {
            match outcome {
                Outcome::Resolved(id, _) => Outcome::Resolved(id, Some(linearization_id)),
                Outcome::Unresolved(_) => Outcome::Unresolved(Some(linearization_id)),
                Outcome::Retry(_) => {
                    panic!("Retry shouldn't happen when searching the top level")
                }
            }
        } else {
            outcome
        }
    }

    /// Search for a member in a declaration's ancestor chain.
    fn search_ancestors(&mut self, declaration_id: DeclarationId, str_id: StringId) -> Outcome {
        match self.ancestors_of(declaration_id) {
            Ancestors::Complete(ids) | Ancestors::Cyclic(ids) => ids
                .iter()
                .find_map(|ancestor_id| {
                    if let Ancestor::Complete(ancestor_id) = ancestor_id {
                        self.graph
                            .declarations()
                            .get(ancestor_id)
                            .unwrap()
                            .as_namespace()
                            .unwrap()
                            .member(&str_id)
                            .map(|id| Outcome::Resolved(*id, None))
                    } else {
                        None
                    }
                })
                .unwrap_or(Outcome::Unresolved(None)),
            Ancestors::Partial(ids) => {
                for ancestor_id in ids {
                    match ancestor_id {
                        Ancestor::Partial(name_id) => {
                            // Stop at unresolved ancestors to avoid resolving to a later one.
                            // Skip if the name matches what we're searching for.
                            if *self.graph.names().get(&name_id).unwrap().str() != str_id {
                                return Outcome::Retry(Some(declaration_id));
                            }
                        }
                        Ancestor::Complete(ancestor_id) => {
                            if let Some(id) = self
                                .graph
                                .declarations()
                                .get(&ancestor_id)
                                .unwrap()
                                .as_namespace()
                                .unwrap()
                                .member(&str_id)
                            {
                                return Outcome::Resolved(*id, Some(declaration_id));
                            }
                        }
                    }
                }
                Outcome::Unresolved(Some(declaration_id))
            }
        }
    }

    /// Look for the constant in the lexical scopes that are a part of its nesting
    fn search_lexical_scopes(&self, name: &Name, str_id: StringId) -> Outcome {
        let mut current_name = name;

        while let Some(nesting_id) = current_name.nesting() {
            if let NameRef::Resolved(nesting_name_ref) = self.graph.names().get(nesting_id).unwrap() {
                let declaration_id = *nesting_name_ref.declaration_id();

                match self.graph.declarations().get(&declaration_id) {
                    Some(Declaration::Namespace(namespace)) => {
                        if let Some(member) = namespace.member(&str_id) {
                            return Outcome::Resolved(*member, None);
                        }
                    }
                    Some(Declaration::ConstantAlias(_)) => {
                        for id in &self.resolve_alias_chains(declaration_id) {
                            if let Some(declaration) = self.graph.declarations().get(id)
                                && let Some(namespace) = declaration.as_namespace()
                                && let Some(member) = namespace.member(&str_id)
                            {
                                return Outcome::Resolved(*member, None);
                            }
                        }
                    }
                    _ => {}
                }

                current_name = nesting_name_ref.name();
            } else {
                return Outcome::Retry(None);
            }
        }

        Outcome::Unresolved(None)
    }

    /// Look for the constant at the top level (member of Object)
    fn search_top_level(&self, str_id: StringId) -> Outcome {
        match self
            .graph
            .declarations()
            .get(&OBJECT_ID)
            .unwrap()
            .as_namespace()
            .unwrap()
            .member(&str_id)
        {
            Some(member_id) => Outcome::Resolved(*member_id, None),
            None => Outcome::Unresolved(None),
        }
    }

    /// Returns a complexity score for a given name, which is used to sort names for resolution. The complexity is based
    /// on how many parent scopes are involved in a name's nesting. This is because simple names are always
    /// straightforward to resolve no matter how deep the nesting is. For example:
    ///
    /// ```ruby
    /// module Foo
    ///   module Bar
    ///     class Baz; end
    ///   end
    /// end
    /// ```
    ///
    /// These are all simple names because they don't require resolution logic to determine the final name of each step.
    /// We only have to ensure that they are ordered by nesting level. Names with parent scopes require that their parts
    /// be resolved to determine what they refer to and so they must be sorted last.
    ///
    /// ```ruby
    /// module Foo
    ///   module Bar::Baz
    ///     class Qux; end
    ///  end
    /// end
    /// ```
    ///
    /// In this case, we need `Bar` to have already been processed so that we can resolve the `Bar` reference inside of
    /// the `Foo` nesting, which then unblocks the resolution of `Baz` and finally `Qux`. Notice how `Qux` is a simple
    /// name, but it's nested under a complex name so we have to sort it last. This is why we consider the number of
    /// parent scopes in the entire nesting, not just for the name itself
    ///
    /// # Panics
    ///
    /// Will panic if there is inconsistent data in the graph
    fn name_depth(name: &NameRef, names: &IdentityHashMap<NameId, NameRef>) -> u32 {
        if name.parent_scope().is_top_level() {
            return 1;
        }

        let parent_depth = name.parent_scope().map_or(0, |id| {
            let name_ref = names.get(id).unwrap();
            Self::name_depth(name_ref, names)
        });

        let nesting_depth = name.nesting().map_or(0, |id| {
            let name_ref = names.get(&id).unwrap();
            Self::name_depth(name_ref, names)
        });

        parent_depth + nesting_depth + 1
    }

    fn prepare_units(&mut self) -> Vec<DefinitionId> {
        let estimated_length = self.graph.definitions().len() / 2;
        let mut definitions = Vec::with_capacity(estimated_length);
        let mut others = Vec::with_capacity(estimated_length);
        let names = self.graph.names();

        for (id, definition) in self.graph.definitions() {
            let uri = self.graph.documents().get(definition.uri_id()).unwrap().uri();

            match definition {
                Definition::Class(def) => {
                    definitions.push((
                        Unit::Definition(*id),
                        (names.get(def.name_id()).unwrap(), uri, definition.offset()),
                    ));
                }
                Definition::Module(def) => {
                    definitions.push((
                        Unit::Definition(*id),
                        (names.get(def.name_id()).unwrap(), uri, definition.offset()),
                    ));
                }
                Definition::Constant(def) => {
                    definitions.push((
                        Unit::Definition(*id),
                        (names.get(def.name_id()).unwrap(), uri, definition.offset()),
                    ));
                }
                Definition::ConstantAlias(def) => {
                    definitions.push((
                        Unit::Definition(*id),
                        (names.get(def.name_id()).unwrap(), uri, definition.offset()),
                    ));
                }
                Definition::SingletonClass(def) => {
                    definitions.push((
                        Unit::Definition(*id),
                        (names.get(def.name_id()).unwrap(), uri, definition.offset()),
                    ));
                }
                _ => {
                    others.push(*id);
                }
            }
        }

        // Sort namespaces based on their name complexity so that simpler names are always first
        // When the depth is the same, sort by URI and offset to maintain determinism
        definitions.sort_by(|(_, (name_a, uri_a, offset_a)), (_, (name_b, uri_b, offset_b))| {
            (Self::name_depth(name_a, names), uri_a, offset_a).cmp(&(Self::name_depth(name_b, names), uri_b, offset_b))
        });

        let mut const_refs = self
            .graph
            .constant_references()
            .iter()
            .map(|(id, constant_ref)| {
                let uri = self.graph.documents().get(&constant_ref.uri_id()).unwrap().uri();

                (
                    Unit::ConstantRef(*id),
                    (names.get(constant_ref.name_id()).unwrap(), uri, constant_ref.offset()),
                )
            })
            .collect::<Vec<_>>();

        // Sort constant references based on their name complexity so that simpler names are always first
        const_refs.sort_by(|(_, (name_a, uri_a, offset_a)), (_, (name_b, uri_b, offset_b))| {
            (Self::name_depth(name_a, names), uri_a, offset_a).cmp(&(Self::name_depth(name_b, names), uri_b, offset_b))
        });

        self.unit_queue
            .extend(definitions.into_iter().map(|(id, _)| id).collect::<VecDeque<_>>());
        self.unit_queue
            .extend(const_refs.into_iter().map(|(id, _)| id).collect::<VecDeque<_>>());

        others.shrink_to_fit();
        others
    }

    /// Returns the singleton parent ID for an attached object ID. A singleton class' parent depends on what the attached
    /// object is:
    ///
    /// - Module: parent is the `Module` class
    /// - Class: parent is the singleton class of the original parent class
    /// - Singleton class: recurse as many times as necessary to wrap the original attached object's parent class
    fn singleton_parent_id(&mut self, attached_id: DeclarationId) -> (DeclarationId, bool) {
        // Base case: if we reached `Object`, then the parent is `Class`
        if attached_id == *OBJECT_ID {
            return (*CLASS_ID, false);
        }

        let decl = self.graph.declarations().get(&attached_id).unwrap();

        match decl {
            Declaration::Namespace(Namespace::Module(_)) => (*MODULE_ID, false),
            Declaration::Namespace(Namespace::SingletonClass(_)) => {
                // For singleton classes, we keep recursively wrapping parents until we can reach the original attached
                // object
                let owner_id = *decl.owner_id();

                let (inner_parent, partial) = self.singleton_parent_id(owner_id);
                (
                    self.get_or_create_singleton_class(inner_parent)
                        .expect("singleton parent should always be a namespace"),
                    partial,
                )
            }
            Declaration::Namespace(Namespace::Class(_)) => {
                // For classes (the regular case), we need to return the singleton class of its parent
                let definition_ids = decl.definitions().to_vec();

                let (picked_parent, partial) = self.get_parent_class(&definition_ids);
                (
                    self.get_or_create_singleton_class(picked_parent)
                        .expect("parent class should always be a namespace"),
                    partial,
                )
            }
            _ => {
                // Other declaration types (constants, methods, etc.) shouldn't reach here,
                // but default to Object's singleton parent
                (*CLASS_ID, false)
            }
        }
    }

    fn get_parent_class(&self, definition_ids: &[DefinitionId]) -> (DeclarationId, bool) {
        let mut explicit_parents = Vec::new();
        let mut partial = false;

        for definition_id in definition_ids {
            let definition = self.graph.definitions().get(definition_id).unwrap();

            if let Definition::Class(class) = definition
                && let Some(superclass) = class.superclass_ref()
            {
                let name = self
                    .graph
                    .names()
                    .get(self.graph.constant_references().get(superclass).unwrap().name_id())
                    .unwrap();

                match name {
                    NameRef::Resolved(resolved) => {
                        let declaration_id = resolved.declaration_id();

                        if self.graph.is_namespace(declaration_id) {
                            explicit_parents.push(*declaration_id);
                        }
                    }
                    NameRef::Unresolved(_) => {
                        partial = true;
                    }
                }
            }
        }

        // If there's more than one parent class that isn't `Object` and they are different, then there's a superclass
        // mismatch error. TODO: We should add a diagnostic here
        (explicit_parents.first().copied().unwrap_or(*OBJECT_ID), partial)
    }

    fn linearize_parent_class(
        &mut self,
        definition_ids: &[DefinitionId],
        context: &mut LinearizationContext,
    ) -> Ancestors {
        let (picked_parent, partial) = self.get_parent_class(definition_ids);
        let result = self.linearize_ancestors(picked_parent, context);
        if partial { result.to_partial() } else { result }
    }

    fn mixins_of(&self, definition_id: DefinitionId) -> Option<Vec<Mixin>> {
        let definition = self.graph.definitions().get(&definition_id).unwrap();

        match definition {
            Definition::Class(class) => Some(class.mixins().to_vec()),
            Definition::SingletonClass(class) => Some(class.mixins().to_vec()),
            Definition::Module(module) => Some(module.mixins().to_vec()),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostic::Rule;
    use crate::test_utils::GraphTest;
    use crate::{
        assert_alias_targets_contain, assert_ancestors_eq, assert_constant_alias_target_eq,
        assert_constant_reference_to, assert_declaration_definitions_count_eq, assert_declaration_does_not_exist,
        assert_declaration_exists, assert_declaration_kind_eq, assert_declaration_references_count_eq,
        assert_descendants, assert_diagnostics_eq, assert_instance_variables_eq, assert_members_eq,
        assert_no_constant_alias_target, assert_no_diagnostics, assert_no_members, assert_owner_eq,
        assert_singleton_class_eq,
    };

    #[test]
    fn resolving_top_level_references() {
        let mut context = GraphTest::new();
        context.index_uri("file:///bar.rb", {
            r"
            class Bar; end

            ::Bar
            Bar
            "
        });
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              ::Bar
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_reference_to!(context, "Bar", "file:///bar.rb:3:3-3:6");
        assert_constant_reference_to!(context, "Bar", "file:///bar.rb:4:1-4:4");
        assert_constant_reference_to!(context, "Bar", "file:///foo.rb:2:5-2:8");
    }

    #[test]
    fn resolving_nested_reference() {
        let mut context = GraphTest::new();
        context.index_uri("file:///bar.rb", {
            r"
            module Foo
              CONST = 123

              class Bar
                CONST
                Foo::CONST
              end
            end
            "
        });
        context.resolve();

        assert_constant_reference_to!(context, "Foo::CONST", "file:///bar.rb:5:5-5:10");
        assert_constant_reference_to!(context, "Foo::CONST", "file:///bar.rb:6:10-6:15");
    }

    #[test]
    fn resolving_nested_reference_that_refer_to_top_level_constant() {
        let mut context = GraphTest::new();
        context.index_uri("file:///bar.rb", {
            r"
            class Baz; end

            module Foo
              class Bar
                Baz
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_constant_reference_to!(context, "Baz", "file:///bar.rb:5:5-5:8");
    }

    #[test]
    fn resolving_constant_path_references_at_top_level() {
        let mut context = GraphTest::new();
        context.index_uri("file:///bar.rb", {
            r"
            module Foo
              class Bar; end
            end

            Foo::Bar
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_reference_to!(context, "Foo::Bar", "file:///bar.rb:5:6-5:9");
    }

    #[test]
    fn resolving_reference_for_non_existing_declaration() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            Foo
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        let reference = context.graph().constant_references().values().next().unwrap();

        match context.graph().names().get(reference.name_id()) {
            Some(NameRef::Unresolved(_)) => {}
            _ => panic!("expected unresolved constant reference"),
        }
    }

    #[test]
    fn resolution_creates_global_declaration() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              class Bar
              end
            end

            class Foo::Baz
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo", ["Bar", "Baz"]);
        assert_owner_eq!(context, "Foo", "Object");

        assert_no_members!(context, "Foo::Bar");
        assert_owner_eq!(context, "Foo::Bar", "Foo");

        assert_no_members!(context, "Foo::Baz");
        assert_owner_eq!(context, "Foo::Baz", "Foo");
    }

    #[test]
    fn resolution_for_non_constant_declarations() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def initialize
                @name = 123
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo", ["@name", "initialize()"]);
        assert_owner_eq!(context, "Foo", "Object");
    }

    #[test]
    fn resolution_for_ambiguous_namespace_definitions() {
        // Like many examples of Ruby code that is ambiguous to static analysis, this example is ambiguous due to
        // require order. If `foo.rb` is loaded first, then `Bar` doesn't exist, Ruby crashes and we should emit an
        // error or warning for a non existing constant.
        //
        // If `bar.rb` is loaded first, then `Bar` resolves to top level `Bar` and `Bar::Baz` is defined, completely
        // escaping the `Foo` nesting.
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              class Bar::Baz
              end
            end
            "
        });
        context.index_uri("file:///bar.rb", {
            r"
            module Bar
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_no_members!(context, "Foo");
        assert_owner_eq!(context, "Foo", "Object");

        assert_members_eq!(context, "Bar", ["Baz"]);
        assert_owner_eq!(context, "Bar", "Object");
    }

    #[test]
    fn resolution_for_top_level_references() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              class ::Bar
                class Baz
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_no_members!(context, "Foo");
        assert_owner_eq!(context, "Foo", "Object");

        assert_members_eq!(context, "Bar", ["Baz"]);
        assert_owner_eq!(context, "Bar", "Object");

        assert_no_members!(context, "Bar::Baz");
        assert_owner_eq!(context, "Bar::Baz", "Bar");
    }

    #[test]
    fn resolution_does_not_loop_infinitely_on_non_existing_constants() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo::Bar
              class Baz
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_declaration_does_not_exist!(context, "Foo");
        assert_declaration_does_not_exist!(context, "Foo::Bar");
        assert_declaration_does_not_exist!(context, "Foo::Bar::Baz");
    }

    #[test]
    fn expected_name_depth_order() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              module Bar
                module Baz
                end

                module ::Top
                  class AfterTop
                  end
                end
              end

              module Qux::Zip
                module Zap
                  class Zop::Boop
                  end
                end
              end
            end
            "
        });

        let mut names = context.graph().names().values().collect::<Vec<_>>();
        assert_eq!(10, names.len());

        names.sort_by_key(|a| Resolver::name_depth(a, context.graph().names()));

        assert_eq!(
            [
                "Top", "Foo", "Qux", "AfterTop", "Bar", "Baz", "Zip", "Zap", "Zop", "Boop"
            ],
            names
                .iter()
                .map(|n| context.graph().strings().get(n.str()).unwrap().as_str())
                .collect::<Vec<_>>()
                .as_slice()
        );
    }

    #[test]
    fn resolution_for_singleton_class() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              class << self
                def bar; end
                BAZ = 123
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_no_members!(context, "Foo");
        assert_owner_eq!(context, "Foo", "Object");
        assert_singleton_class_eq!(context, "Foo", "Foo::<Foo>");

        assert_members_eq!(context, "Foo::<Foo>", ["BAZ", "bar()"]);
        assert_owner_eq!(context, "Foo::<Foo>", "Foo");
    }

    #[test]
    fn resolution_for_nested_singleton_class() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              class << self
                class << self
                  def baz; end
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_no_members!(context, "Foo");
        assert_singleton_class_eq!(context, "Foo", "Foo::<Foo>");

        assert_no_members!(context, "Foo::<Foo>");
        assert_singleton_class_eq!(context, "Foo::<Foo>", "Foo::<Foo>::<<Foo>>");

        assert_members_eq!(context, "Foo::<Foo>::<<Foo>>", ["baz()"]);
        assert_owner_eq!(context, "Foo::<Foo>::<<Foo>>", "Foo::<Foo>");
    }

    #[test]
    fn resolution_for_singleton_class_of_external_constant() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo; end
            class Bar
              class << Foo
                def baz; end

                class Baz; end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_no_members!(context, "Foo");
        assert_owner_eq!(context, "Foo", "Object");
        assert_singleton_class_eq!(context, "Foo", "Foo::<Foo>");

        assert_no_members!(context, "Bar");
        assert_owner_eq!(context, "Bar", "Object");

        assert_members_eq!(context, "Foo::<Foo>", ["Baz", "baz()"]);
        assert_owner_eq!(context, "Foo::<Foo>", "Foo");
    }

    #[test]
    fn resolution_for_class_variable_in_nested_singleton_class() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              class << self
                @@bar = 123

                class << self
                  @@baz = 456
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo", ["@@bar", "@@baz"]);
        assert_owner_eq!(context, "Foo", "Object");
    }

    #[test]
    fn resolution_for_class_variable_in_method() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def bar
                @@baz = 456
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo", ["@@baz", "bar()"]);
    }

    #[test]
    fn resolution_for_class_variable_only_follows_lexical_nesting() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo; end
            class Bar
              def Foo.demo
                @@cvar1 = 1
              end

              class << Foo
                def demo2
                  @@cvar2 = 1
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_no_members!(context, "Foo");
        assert_members_eq!(context, "Bar", ["@@cvar1", "@@cvar2"]);
    }

    #[test]
    fn resolution_for_class_variable_at_top_level() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            "
            @@var = 123
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // TODO: this should push an error diagnostic
        assert_declaration_does_not_exist!(context, "Object::@@var");
    }

    #[test]
    fn singleton_class_is_set() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            "
            class Foo
              class << self
              end
            end
            "
        });

        context.resolve();

        assert_no_diagnostics!(&context);

        assert_declaration_exists!(context, "Foo::<Foo>");
        assert_singleton_class_eq!(context, "Foo", "Foo::<Foo>");
    }

    #[test]
    fn resolution_for_method_with_receiver() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def self.bar; end

              class << self
                def self.nested_bar; end
              end
            end

            class Bar
              def Foo.baz; end

              def self.qux; end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo::<Foo>", ["bar()", "baz()"]);
        assert_owner_eq!(context, "Foo::<Foo>", "Foo");

        assert_members_eq!(context, "Foo::<Foo>::<<Foo>>", ["nested_bar()"]);
        assert_owner_eq!(context, "Foo::<Foo>::<<Foo>>", "Foo::<Foo>");

        assert_members_eq!(context, "Bar::<Bar>", ["qux()"]);
        assert_owner_eq!(context, "Bar::<Bar>", "Bar");
    }

    #[test]
    fn linearizing_super_classes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo; end
            class Bar < Foo; end
            class Baz < Bar; end
            class Qux < Baz; end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Qux", ["Qux", "Baz", "Bar", "Foo", "Object"]);
    }

    #[test]
    fn descendants_are_tracked_for_parent_classes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              CONST = 123
            end

            class Bar < Foo; end

            class Baz < Bar
              CONST
            end

            class Qux < Bar
              CONST
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_descendants!(context, "Foo", ["Bar"]);
        assert_descendants!(context, "Bar", ["Baz", "Qux"]);
    }

    #[test]
    fn linearizing_circular_super_classes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo < Bar; end
            class Bar < Baz; end
            class Baz < Foo; end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["Foo", "Bar", "Baz", "Object"]);
    }

    #[test]
    fn resolving_a_constant_inherited_from_the_super_class() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              CONST = 123
            end

            class Bar < Foo
              CONST
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:6:3-6:8");
    }

    #[test]
    fn does_not_loop_forever_on_non_existing_parents() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Bar < Foo
              CONST
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        let declaration = context.graph().declarations().get(&DeclarationId::from("Bar")).unwrap();
        assert!(matches!(
            declaration.as_namespace().unwrap().clone_ancestors(),
            Ancestors::Partial(_)
        ));
    }

    #[test]
    fn resolving_inherited_constant_dependent_on_complex_parent() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              module Bar
                class Baz
                  CONST = 123
                end
              end
            end
            class Qux < Foo::Bar::Baz
              CONST
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_constant_reference_to!(context, "Foo::Bar::Baz::CONST", "file:///foo.rb:9:3-9:8");
    }

    #[test]
    fn resolution_for_instance_and_class_instance_variables() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              @foo = 0

              def initialize
                @bar = 1
              end

              def self.baz
                @baz = 2
              end

              class << self
                def qux
                  @qux = 3
                end

                def self.nested
                  @nested = 4
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_instance_variables_eq!(context, "Foo", ["@bar"]);
        // @qux in `class << self; def qux` - self is Foo when called, so @qux belongs to Foo's singleton class
        assert_instance_variables_eq!(context, "Foo::<Foo>", ["@baz", "@foo", "@qux"]);
        assert_instance_variables_eq!(context, "Foo::<Foo>::<<Foo>>", ["@nested"]);
    }

    #[test]
    fn resolution_for_instance_variables_with_dynamic_method_owner() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
            end

            class Bar
              def Foo.bar
                @foo = 0
              end

              class << Foo
                def Bar.baz
                  @baz = 1
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_instance_variables_eq!(context, "Foo::<Foo>", ["@foo"]);
        assert_instance_variables_eq!(context, "Bar::<Bar>", ["@baz"]);
    }

    #[test]
    fn resolution_for_class_instance_variable_in_compact_namespace() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Bar; end

            class Foo
              class Bar::Baz
                @baz = 1
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // The class is `Bar::Baz`, so its singleton class is `Bar::Baz::<Baz>`
        assert_instance_variables_eq!(context, "Bar::Baz::<Baz>", ["@baz"]);
    }

    #[test]
    fn resolution_for_instance_variable_in_singleton_class_body() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              class << self
                @bar = 1

                class << self
                  @baz = 2
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_instance_variables_eq!(context, "Foo::<Foo>::<<Foo>>", ["@bar"]);
        assert_instance_variables_eq!(context, "Foo::<Foo>::<<Foo>>::<<<Foo>>>", ["@baz"]);
    }

    #[test]
    fn resolution_for_top_level_instance_variable() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            @foo = 0
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // Top-level instance variables belong to `<main>`, not `Object`.
        // We can't represent `<main>` yet, so no declaration is created.
        assert_declaration_does_not_exist!(context, "Object::@foo");
    }

    #[test]
    fn resolution_for_instance_variable_with_unresolved_receiver() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def foo.bar
                @baz = 0
              end
            end
            "
        });
        context.resolve();

        assert_diagnostics_eq!(
            &context,
            ["dynamic-singleton-definition: Dynamic receiver for singleton method definition (2:3-4:6)",]
        );

        // Instance variable in method with unresolved receiver should not create a declaration
        assert_declaration_does_not_exist!(context, "Object::@baz");
        assert_declaration_does_not_exist!(context, "Foo::@baz");
    }

    #[test]
    fn resolving_method_alias() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def foo; end

              alias bar foo
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo", ["bar()", "foo()"]);
    }

    #[test]
    fn resolving_global_variable_alias() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            $foo = 123
            alias $bar $foo
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Object", ["$bar", "$foo"]);
    }

    #[test]
    fn linearizing_parent_classes_with_parent_scope() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              class Bar
              end
            end
            class Baz < Foo::Bar
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Baz", ["Baz", "Foo::Bar", "Object"]);
    }

    #[test]
    fn resolving_constant_references_involved_in_prepends() {
        let mut context = GraphTest::new();

        // To linearize the ancestors of `Bar`, we need to resolve `Foo` first. However, during that resolution, we need
        // to check `Bar`'s ancestor chain before checking the top level (which is where we'll find `Foo`). In these
        // scenarios, we need to realize the dependency and skip ancestors
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Bar
              prepend Foo
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Bar", ["Foo", "Bar"]);
    }

    #[test]
    fn resolving_prepend_using_inherited_constant() {
        let mut context = GraphTest::new();
        // Prepending `Foo` makes `Bar` available, which we can then prepend as well. This requires resolving constants
        // with partially linearized ancestors
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              module Bar; end
            end
            class Baz
              prepend Foo
              prepend Bar
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Baz", ["Foo::Bar", "Foo", "Baz", "Object"]);
    }

    #[test]
    fn linearizing_prepended_modules() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Bar
              prepend Foo
            end
            class Baz
              prepend Bar
            end
            class Qux < Baz; end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["Foo"]);
        assert_ancestors_eq!(context, "Bar", ["Foo", "Bar"]);
        assert_ancestors_eq!(context, "Qux", ["Qux", "Foo", "Bar", "Baz", "Object"]);
    }

    #[test]
    fn prepend_on_dynamic_namespace_definitions() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module B; end
            A = Struct.new do
              prepend B
            end

            C = Class.new do
              prepend B
            end

            D = Module.new do
              prepend B
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "B", ["B"]);
        // TODO: this is a temporary hack to avoid crashing on `Struct.new`, `Class.new` and `Module.new`
        //assert_ancestors_eq!(context, "A", Vec::<&str>::new());
        assert_ancestors_eq!(context, "C", ["B", "C", "Object"]);
        assert_ancestors_eq!(context, "D", ["B", "D"]);
    }

    #[test]
    fn prepends_track_descendants() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Bar
              prepend Foo
            end
            class Baz
              prepend Bar
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_descendants!(context, "Foo", ["Bar", "Baz"]);
        assert_descendants!(context, "Bar", ["Baz"]);
    }

    #[test]
    fn cyclic_prepend() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              prepend Foo
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["Foo"]);
    }

    #[test]
    fn duplicate_prepends() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
            end

            module Bar
              prepend Foo
              prepend Foo
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Bar", ["Foo", "Bar"]);
    }

    #[test]
    fn indirect_duplicate_prepends() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end

            module B
              prepend A
            end

            module C
              prepend A
            end

            module Foo
              prepend B
              prepend C
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "A", ["A"]);
        assert_ancestors_eq!(context, "B", ["A", "B"]);
        assert_ancestors_eq!(context, "C", ["A", "C"]);
        assert_ancestors_eq!(context, "Foo", ["A", "C", "B", "Foo"]);
    }

    #[test]
    fn multiple_mixins_in_same_prepend() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end
            module B; end

            class Foo
              prepend A, B
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["A", "B", "Foo", "Object"]);
    }

    #[test]
    fn prepends_involving_parent_scopes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A
              module B
                module C; end
              end
            end

            module D
              prepend A::B::C
            end

            module Foo
              prepend D
              prepend A::B::C
            end

            module Bar
              prepend A::B::C
              prepend D
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["A::B::C", "D", "Foo"]);
        assert_ancestors_eq!(context, "Bar", ["A::B::C", "D", "Bar"]);
    }

    #[test]
    fn duplicate_prepends_in_parents() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end

            module B
              prepend A
            end

            class Parent
              prepend B
            end

            class Child < Parent
              prepend B
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Child", ["A", "B", "Child", "A", "B", "Parent", "Object"]);
    }

    #[test]
    fn prepended_modules_involved_in_definitions() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              module Bar; end
            end

            module Baz
              prepend Foo

              class Bar::Qux
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo::Bar", ["Qux"]);
        assert_owner_eq!(context, "Foo::Bar", "Foo");

        assert_no_members!(context, "Foo::Bar::Qux");
        assert_owner_eq!(context, "Foo::Bar::Qux", "Foo::Bar");
    }

    #[test]
    fn resolving_constant_references_involved_in_includes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Bar
              include Foo
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Bar", ["Bar", "Foo"]);
    }

    #[test]
    fn resolving_include_using_inherited_constant() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              module Bar; end
            end
            class Baz
              include Foo
              include Bar
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Baz", ["Baz", "Foo::Bar", "Foo", "Object"]);
    }

    #[test]
    fn linearizing_included_modules() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Bar
              prepend Foo
            end
            class Baz
              prepend Bar
            end
            class Qux < Baz; end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["Foo"]);
        assert_ancestors_eq!(context, "Bar", ["Foo", "Bar"]);
        assert_ancestors_eq!(context, "Qux", ["Qux", "Foo", "Bar", "Baz", "Object"]);
    }

    #[test]
    fn include_on_dynamic_namespace_definitions() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module B; end
            A = Struct.new do
              include B
            end

            C = Class.new do
              include B
            end

            D = Module.new do
              include B
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "B", ["B"]);
        // TODO: this is a temporary hack to avoid crashing on `Struct.new`, `Class.new` and `Module.new`
        //assert_ancestors_eq!(context, "A", Vec::<&str>::new());
        assert_ancestors_eq!(context, "C", ["C", "B", "Object"]);
        assert_ancestors_eq!(context, "D", ["D", "B"]);
    }

    #[test]
    fn cyclic_include() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              include Foo
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["Foo"]);
    }

    #[test]
    fn duplicate_includes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
            end

            module Bar
              include Foo
              include Foo
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Bar", ["Bar", "Foo"]);
    }

    #[test]
    fn indirect_duplicate_includes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end

            module B
              include A
            end

            module C
              include A
            end

            module Foo
              include B
              include C
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "A", ["A"]);
        assert_ancestors_eq!(context, "B", ["B", "A"]);
        assert_ancestors_eq!(context, "C", ["C", "A"]);
        assert_ancestors_eq!(context, "Foo", ["Foo", "C", "B", "A"]);
    }

    #[test]
    fn includes_involving_parent_scopes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A
              module B
                module C; end
              end
            end

            module D
              include A::B::C
            end

            module Foo
              include D
              include A::B::C
            end

            module Bar
              include A::B::C
              include D
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["Foo", "D", "A::B::C"]);
        assert_ancestors_eq!(context, "Bar", ["Bar", "D", "A::B::C"]);
    }

    #[test]
    fn duplicate_includes_in_parents() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end

            module B
              include A
            end

            class Parent
              include B
            end

            class Child < Parent
              include B
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Child", ["Child", "Parent", "B", "A", "Object"]);
    }

    #[test]
    fn included_modules_involved_in_definitions() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              module Bar; end
            end

            module Baz
              include Foo

              class Bar::Qux
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo::Bar", ["Qux"]);
        assert_owner_eq!(context, "Foo::Bar", "Foo");

        assert_no_members!(context, "Foo::Bar::Qux");
        assert_owner_eq!(context, "Foo::Bar::Qux", "Foo::Bar");
    }

    #[test]
    fn references_with_parent_scope_search_inheritance() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              module Bar; end
            end

            class Baz
              include Foo
            end

            Baz::Bar
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_reference_to!(context, "Foo::Bar", "file:///foo.rb:9:6-9:9");
    }

    #[test]
    fn duplicate_includes_and_prepends() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end

            class Foo
              prepend A
              include A
            end

            class Bar
              include A
              prepend A
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["A", "Foo", "Object"]);
        assert_ancestors_eq!(context, "Bar", ["A", "Bar", "A", "Object"]);
    }

    #[test]
    fn duplicate_indirect_includes_and_prepends() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end
            module B
              include A
            end
            module C
              prepend A
            end

            class Foo
              include C
              prepend B
              include A
            end

            class Bar
              include A
              prepend B
              include C
            end

            class Baz
              prepend B
              include C
              prepend A
            end

            class Qux
              prepend A
              include C
              prepend B
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["B", "A", "Foo", "A", "C", "Object"]);
        assert_ancestors_eq!(context, "Bar", ["B", "A", "Bar", "C", "A", "Object"]);
        assert_ancestors_eq!(context, "Baz", ["B", "A", "Baz", "C", "Object"]);
        assert_ancestors_eq!(context, "Qux", ["B", "A", "Qux", "C", "Object"]);
    }

    #[test]
    fn duplicate_includes_and_prepends_through_parents() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end

            class Parent
              include A
            end

            class Foo < Parent
              prepend A
            end

            class Bar < Parent
              include A
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["A", "Foo", "Parent", "A", "Object"]);
        assert_ancestors_eq!(context, "Bar", ["Bar", "Parent", "A", "Object"]);
    }

    #[test]
    fn multiple_mixins_in_same_include() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end
            module B; end

            class Foo
              include A, B
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["Foo", "A", "B", "Object"]);
    }

    #[test]
    fn descendants_are_tracked_for_includes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Bar
              include Foo
            end
            module Baz
              include Bar
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_descendants!(context, "Bar", ["Baz"]);
        assert_descendants!(context, "Foo", ["Bar", "Baz"]);
    }

    #[test]
    fn singleton_ancestors_for_classes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Qux; end
            module Zip; end
            class Bar; end

            class Baz < Bar
              extend Foo

              class << self
                include Qux

                class << self
                  include Zip
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // Note: the commented out parts require RBS indexing
        assert_ancestors_eq!(
            context,
            "Baz::<Baz>",
            [
                "Baz::<Baz>",
                "Qux",
                "Foo",
                "Bar::<Bar>",
                "Object::<Object>",
                // "BasicObject::<BasicObject>",
                "Class",
                // "Module",
                "Object",
                // "Kernel",
                // "BasicObject"
            ]
        );

        assert_ancestors_eq!(
            context,
            "Baz::<Baz>::<<Baz>>",
            [
                "Baz::<Baz>::<<Baz>>",
                "Zip",
                "Bar::<Bar>::<<Bar>>",
                "Object::<Object>::<<Object>>",
                // "BasicObject::<BasicObject>::<<BasicObject>>",
                "Class::<Class>",
                // "Module::<Module>",
                "Object::<Object>",
                // "BasicObject::<BasicObject>",
                "Class",
                // "Module",
                "Object",
                // "Kernel",
                // "BasicObject"
            ]
        );
    }

    #[test]
    fn singleton_ancestors_for_modules() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Qux; end
            module Zip; end
            class Bar; end

            module Baz
              extend Foo

              class << self
                include Qux

                class << self
                  include Zip
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // Note: the commented out parts require RBS indexing
        assert_ancestors_eq!(
            context,
            "Baz::<Baz>",
            [
                "Baz::<Baz>",
                "Qux",
                "Foo",
                "Module",
                "Object",
                // "Kernel",
                // "BasicObject"
            ]
        );
        assert_ancestors_eq!(
            context,
            "Baz::<Baz>::<<Baz>>",
            [
                "Baz::<Baz>::<<Baz>>",
                "Zip",
                "Module::<Module>",
                "Object::<Object>",
                // "BasicObject::<BasicObject>",
                "Class",
                // "Module",
                "Object",
                // "Kernel",
                // "BasicObject"
            ]
        );
    }

    #[test]
    fn singleton_ancestors_with_inherited_parent_modules() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Qux; end
            class Bar
              class << self
                include Foo
                prepend Qux
              end
            end

            class Baz < Bar
              class << self
                class << self
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // TODO: the commented out parts require RBS indexing
        assert_ancestors_eq!(
            context,
            "Bar::<Bar>",
            [
                "Qux",
                "Bar::<Bar>",
                "Foo",
                "Object::<Object>",
                // "BasicObject::<BasicObject>",
                "Class",
                // "Module",
                "Object",
                // "Kernel",
                // "BasicObject"
            ]
        );

        assert_ancestors_eq!(
            context,
            "Baz::<Baz>",
            [
                "Baz::<Baz>",
                "Qux",
                "Bar::<Bar>",
                "Foo",
                "Object::<Object>",
                // "BasicObject::<BasicObject>",
                "Class",
                // "Module",
                "Object",
                // "Kernel",
                // "BasicObject"
            ]
        );
        assert_ancestors_eq!(
            context,
            "Baz::<Baz>::<<Baz>>",
            [
                "Baz::<Baz>::<<Baz>>",
                "Bar::<Bar>::<<Bar>>",
                "Object::<Object>::<<Object>>",
                // "BasicObject::<BasicObject>::<<BasicObject>>",
                "Class::<Class>",
                // "Module::<Module>",
                "Object::<Object>",
                // "BasicObject::<BasicObject>",
                "Class",
                // "Module",
                "Object",
                // "Kernel",
                // "BasicObject"
            ]
        );
    }

    #[test]
    fn resolving_global_variable_alias_inside_method() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def setup
                alias $bar $baz
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // Global variable aliases should still be owned by Object, regardless of where defined
        assert_members_eq!(context, "Object", ["$bar", "Foo"]);
    }

    #[test]
    fn resolving_method_defined_inside_method() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def setup
                def inner_method; end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // inner_method should be owned by Foo, not by setup
        assert_members_eq!(context, "Foo", ["inner_method()", "setup()"]);
    }

    #[test]
    fn resolving_attr_accessors_inside_method() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def self.setup
                attr_reader :reader_attr
                attr_writer :writer_attr
                attr_accessor :accessor_attr
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo::<Foo>", ["setup()"]);

        // All attr_* should be owned by Foo, not by setup
        assert_members_eq!(context, "Foo", ["accessor_attr()", "reader_attr()", "writer_attr()"]);
    }

    #[test]
    fn resolving_constant_alias_to_module() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              CONST = 123
            end

            ALIAS = Foo
            ALIAS::CONST
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "ALIAS", "Foo");
        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:6:8-6:13");
    }

    #[test]
    fn resolving_constant_alias_to_nested_module() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              module Bar
                CONST = 123
              end
            end

            ALIAS = Foo::Bar
            ALIAS::CONST
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "ALIAS", "Foo::Bar");
        assert_constant_reference_to!(context, "Foo::Bar::CONST", "file:///foo.rb:8:8-8:13");
    }

    #[test]
    fn resolving_constant_alias_inside_module() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              CONST = 123
            end

            module Bar
              MyFoo = Foo
              MyFoo::CONST
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_constant_alias_target_eq!(context, "Bar::MyFoo", "Foo");
        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:7:10-7:15");
    }

    #[test]
    fn resolving_constant_alias_in_superclass() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              CONST = 123
            end

            class Bar < Foo
            end

            ALIAS = Bar
            ALIAS::CONST
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:9:8-9:13");
    }

    #[test]
    fn resolving_chained_constant_aliases() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              CONST = 123
            end

            ALIAS1 = Foo
            ALIAS2 = ALIAS1
            ALIAS2::CONST
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "ALIAS1", "Foo");
        assert_constant_alias_target_eq!(context, "ALIAS2", "ALIAS1");
        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:7:9-7:14");
    }

    #[test]
    fn resolving_constant_alias_to_non_existent_target() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            ALIAS_1 = NonExistent
            ALIAS_2 = ALIAS_1
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_constant_alias_target_eq!(context, "ALIAS_2", "ALIAS_1");
        assert_no_constant_alias_target!(context, "ALIAS_1");
    }

    #[test]
    fn resolving_constant_alias_to_value_in_constant_path() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            VALUE = 1
            ALIAS = VALUE
            ALIAS::NOPE
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "ALIAS", "VALUE");

        // NOPE can't be created because ALIAS points to a value constant, not a namespace
        assert_declaration_does_not_exist!(context, "VALUE::NOPE");
    }

    #[test]
    fn resolving_constant_alias_defined_before_target() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            ALIAS = Foo
            module Foo
              CONST = 1
            end
            ALIAS::CONST
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "ALIAS", "Foo");
        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:5:8-5:13");
    }

    #[test]
    fn resolving_constant_alias_to_value() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              CONST = 1
            end
            class Bar
              CONST = Foo::CONST
            end
            BAZ = Bar::CONST
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_constant_alias_target_eq!(context, "BAZ", "Bar::CONST");
        assert_constant_alias_target_eq!(context, "Bar::CONST", "Foo::CONST");
    }

    #[test]
    fn resolving_circular_constant_aliases() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            A = B
            B = C
            C = A
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_constant_alias_target_eq!(context, "A", "B");
        assert_constant_alias_target_eq!(context, "B", "C");
        assert_constant_alias_target_eq!(context, "C", "A");
    }

    #[test]
    fn resolving_circular_constant_aliases_cross_namespace() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A
              X = B::Y
            end
            module B
              Y = A::X
            end

            A::X::SOMETHING = 1
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_declaration_exists!(context, "A::X");
        assert_declaration_exists!(context, "B::Y");

        // SOMETHING can't be created because the circular alias can't resolve to a namespace
        assert_declaration_does_not_exist!(context, "A::X::SOMETHING");
    }

    #[test]
    fn resolving_constant_alias_ping_pong() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Left
              module Deep
                VALUE = 'left'
              end
            end

            module Right
              module Deep
                VALUE = 'right'
              end
            end

            Left::RIGHT_REF = Right
            Right::LEFT_REF = Left

            Left::RIGHT_REF::Deep::VALUE
            Left::RIGHT_REF::LEFT_REF::Deep::VALUE
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "Left::RIGHT_REF", "Right");
        assert_constant_alias_target_eq!(context, "Right::LEFT_REF", "Left");

        // Left::RIGHT_REF::Deep::VALUE
        assert_constant_reference_to!(context, "Right::Deep", "file:///foo.rb:16:18-16:22");
        assert_constant_reference_to!(context, "Right::Deep::VALUE", "file:///foo.rb:16:24-16:29");
        // Left::RIGHT_REF::LEFT_REF::Deep::VALUE
        assert_constant_reference_to!(context, "Left::Deep", "file:///foo.rb:17:28-17:32");
        assert_constant_reference_to!(context, "Left::Deep::VALUE", "file:///foo.rb:17:34-17:39");
    }

    #[test]
    fn resolving_constant_alias_self_referential() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module M
              SELF_REF = M

              class Thing
                CONST = 1
              end
            end

            M::SELF_REF::Thing::CONST
            M::SELF_REF::SELF_REF::Thing::CONST
            M::SELF_REF::SELF_REF::SELF_REF::Thing::CONST
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "M::SELF_REF", "M");

        // All 3 paths resolve to M::Thing::CONST
        assert_declaration_references_count_eq!(context, "M::Thing::CONST", 3);
        assert_declaration_references_count_eq!(context, "M::Thing", 3);

        // M::SELF_REF::Thing::CONST
        assert_constant_reference_to!(context, "M::Thing", "file:///foo.rb:9:14-9:19");
        assert_constant_reference_to!(context, "M::Thing::CONST", "file:///foo.rb:9:21-9:26");
        // M::SELF_REF::SELF_REF::Thing::CONST
        assert_constant_reference_to!(context, "M::Thing", "file:///foo.rb:10:24-10:29");
        assert_constant_reference_to!(context, "M::Thing::CONST", "file:///foo.rb:10:31-10:36");
        // M::SELF_REF::SELF_REF::SELF_REF::Thing::CONST
        assert_constant_reference_to!(context, "M::Thing", "file:///foo.rb:11:34-11:39");
        assert_constant_reference_to!(context, "M::Thing::CONST", "file:///foo.rb:11:41-11:46");
    }

    #[test]
    fn resolving_class_through_constant_alias() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Outer
              class Inner
              end
            end

            ALIAS = Outer
            Outer::NESTED = Outer::Inner

            class ALIAS::NESTED
              ADDED_CONST = 1
            end

            Outer::Inner::ADDED_CONST
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "ALIAS", "Outer");
        assert_constant_alias_target_eq!(context, "Outer::NESTED", "Outer::Inner");

        // ADDED_CONST should be in Outer::Inner (the resolved target)
        assert_declaration_exists!(context, "Outer::Inner::ADDED_CONST");

        assert_declaration_references_count_eq!(context, "Outer::Inner::ADDED_CONST", 1);
    }

    #[test]
    fn resolving_class_definition_through_constant_alias() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Outer
              CONST = 1
            end

            ALIAS = Outer

            class ALIAS::NewClass
              CLASS_CONST = 2
            end

            Outer::NewClass::CLASS_CONST
            ALIAS::NewClass::CLASS_CONST
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "ALIAS", "Outer");

        // NewClass should be declared under Outer, not ALIAS
        assert_declaration_exists!(context, "Outer::NewClass");
        assert_declaration_exists!(context, "Outer::NewClass::CLASS_CONST");

        // Outer::NewClass::CLASS_CONST
        assert_constant_reference_to!(context, "Outer::NewClass", "file:///foo.rb:11:8-11:16");
        assert_constant_reference_to!(context, "Outer::NewClass::CLASS_CONST", "file:///foo.rb:11:18-11:29");
        // ALIAS::NewClass::CLASS_CONST
        assert_constant_reference_to!(context, "Outer::NewClass", "file:///foo.rb:12:8-12:16");
        assert_constant_reference_to!(context, "Outer::NewClass::CLASS_CONST", "file:///foo.rb:12:18-12:29");
    }

    #[test]
    fn resolving_constant_alias_with_multiple_definitions() {
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", {
            r"
            module A; end
            FOO = A
            "
        });
        context.index_uri("file:///b.rb", {
            r"
            module B; end
            FOO = B
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // FOO should have 2 definitions pointing to different targets
        assert_declaration_definitions_count_eq!(context, "FOO", 2);

        assert_alias_targets_contain!(context, "FOO", "A", "B");
    }

    #[test]
    fn resolving_constant_alias_with_multiple_targets() {
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", {
            r"
            module A
              CONST_A = 1
            end
            FOO = A
            "
        });
        context.index_uri("file:///b.rb", {
            r"
            module B
              CONST_B = 2
            end
            FOO = B
            "
        });
        context.index_uri("file:///usage.rb", {
            r"
            FOO::CONST_A
            FOO::CONST_B
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_reference_to!(context, "A::CONST_A", "file:///usage.rb:1:6-1:13");
        assert_constant_reference_to!(context, "B::CONST_B", "file:///usage.rb:2:6-2:13");
    }

    #[test]
    fn resolving_constant_alias_multi_target_with_circular() {
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", {
            r"
            module A
              CONST = 1
            end
            ALIAS = A
            "
        });
        context.index_uri("file:///b.rb", "ALIAS = ALIAS");
        context.index_uri("file:///usage.rb", "ALIAS::CONST");
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        // ALIAS should have two targets: A and ALIAS (self-reference)
        assert_alias_targets_contain!(context, "ALIAS", "A", "ALIAS");

        // ALIAS::CONST should still resolve to A::CONST through the valid path
        assert_constant_reference_to!(context, "A::CONST", "file:///usage.rb:1:8-1:13");
    }

    #[test]
    fn resolving_constant_reference_through_chained_aliases() {
        let mut context = GraphTest::new();
        context.index_uri("file:///defs.rb", {
            r"
            module Foo
              CONST = 1
            end
            ALIAS1 = Foo
            ALIAS2 = ALIAS1
            "
        });
        context.index_uri("file:///usage.rb", "ALIAS2::CONST");
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "ALIAS1", "Foo");
        assert_constant_alias_target_eq!(context, "ALIAS2", "ALIAS1");

        assert_constant_reference_to!(context, "Foo::CONST", "file:///usage.rb:1:9-1:14");
    }

    #[test]
    fn resolving_constant_reference_through_top_level_alias_target() {
        let mut context = GraphTest::new();
        context.index_uri("file:///defs.rb", {
            r"
            module Foo
              CONST = 1
            end
            ALIAS = ::Foo
            "
        });
        context.index_uri("file:///usage.rb", "ALIAS::CONST");
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_reference_to!(context, "Foo::CONST", "file:///usage.rb:1:8-1:13");
    }

    // Regression test: defining singleton method on alias triggers get_or_create_singleton_class
    #[test]
    fn resolving_singleton_method_on_alias_does_not_panic() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo; end
            ALIAS = Foo
            def ALIAS.singleton_method; end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);
    }

    #[test]
    fn resolving_instance_variable_on_alias_does_not_panic() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo; end
            ALIAS = Foo
            def ALIAS.singleton_method
              @ivar = 123
            end
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context);
    }

    #[test]
    fn multi_target_alias_constant_added_to_primary_owner() {
        let mut context = GraphTest::new();
        context.index_uri("file:///modules.rb", {
            r"
            module Foo; end
            module Bar; end
            "
        });
        context.index_uri("file:///alias1.rb", {
            r"
            ALIAS ||= Foo
            "
        });
        context.index_uri("file:///alias2.rb", {
            r"
            ALIAS ||= Bar
            "
        });
        context.index_uri("file:///const.rb", {
            r"
            ALIAS::CONST = 123
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo", ["CONST"]);
        assert_no_members!(context, "Bar");
    }

    #[test]
    fn distinct_declarations_with_conflicting_string_ids() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def Array(); end
              class Array; end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // Both entries exist as unique members
        assert_members_eq!(context, "Foo", ["Array", "Array()"]);

        // Both declarations exist with unique IDs
        assert_declaration_exists!(context, "Foo::Array");
        assert_declaration_exists!(context, "Foo#Array()");
    }

    #[test]
    fn fully_qualified_names_are_unique() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              class Bar
                CONST = 1
                @class_ivar = 2

                attr_reader :baz
                attr_writer :qux
                attr_accessor :zip

                def instance_m
                  @@class_var = 3
                end

                def self.singleton_m
                  $global_var = 4
                end

                def Foo.another_singleton_m; end

                class << self
                  OTHER_CONST = 5
                  @other_class_ivar = 6
                  @@other_class_var = 7

                  def other_instance_m
                    @my_class_var = 8
                  end

                  def self.other_singleton_m
                    $other_global_var = 9
                  end
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // In the same order of appearence
        assert_declaration_exists!(context, "Foo");
        assert_declaration_exists!(context, "Foo::Bar");
        assert_declaration_exists!(context, "Foo::Bar::CONST");
        assert_declaration_exists!(context, "Foo::Bar::<Bar>#@class_ivar");
        assert_declaration_exists!(context, "Foo::Bar#baz()");
        // TODO: needs the fix for attributes
        // assert_declaration_exists!(context, "Foo::Bar#qux=()");
        assert_declaration_exists!(context, "Foo::Bar#zip()");
        // TODO: needs the fix for attributes
        // assert_declaration_exists!(context, "Foo::Bar#zip=()");
        assert_declaration_exists!(context, "Foo::Bar#instance_m()");
        assert_declaration_exists!(context, "Foo::Bar#@@class_var");
        assert_declaration_exists!(context, "Foo::Bar::<Bar>#singleton_m()");
        assert_declaration_exists!(context, "$global_var");
        assert_declaration_exists!(context, "Foo::<Foo>#another_singleton_m()");
        assert_declaration_exists!(context, "Foo::Bar::<Bar>::OTHER_CONST");
        assert_declaration_exists!(context, "Foo::Bar::<Bar>::<<Bar>>#@other_class_ivar");
        assert_declaration_exists!(context, "Foo::Bar#@@other_class_var");
        assert_declaration_exists!(context, "Foo::Bar::<Bar>#other_instance_m()");
        assert_declaration_exists!(context, "Foo::Bar::<Bar>#@my_class_var");
        assert_declaration_exists!(context, "Foo::Bar::<Bar>::<<Bar>>#other_singleton_m()");
        assert_declaration_exists!(context, "$other_global_var");
    }

    #[test]
    fn test_nested_same_names() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
              module Foo; end

              module Bar
                Foo

                module Foo
                  FOO = 42
                end
              end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        // FIXME: this is wrong, the reference is not to `Bar::Foo`, but to `Foo`
        assert_constant_reference_to!(context, "Bar::Foo", "file:///foo.rb:4:3-4:6");

        assert_ancestors_eq!(context, "Foo", &["Foo"]);
        assert_ancestors_eq!(context, "Bar::Foo", &["Bar::Foo"]);

        assert_no_members!(context, "Foo");
        assert_members_eq!(context, "Bar::Foo", ["FOO"]);
    }

    #[test]
    fn resolves_constant_with_ancestors_partial() {
        // B has Ancestors::Partial because its prepend is defined in another file.
        // X must wait for B's ancestors to resolve, then resolve to A::X.
        let mut context = GraphTest::new();
        context.index_uri("file:///1.rb", {
            r"
            module A
              X = 1
            end
            class B
              X = 2
            end
            class C < B
              X
            end
            "
        });
        context.index_uri("file:///2.rb", {
            r"
            class B
              prepend A
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "C", ["C", "A", "B", "Object"]);
        assert_constant_reference_to!(context, "A::X", "file:///1.rb:8:3-8:4");
    }

    #[test]
    fn resolves_constant_with_ancestor_partial() {
        // C has an Ancestor::Partial entry because O::A is defined in another file.
        // X must wait for O::A to resolve, then resolve to O::A::X.
        let mut context = GraphTest::new();
        context.index_uri("file:///1.rb", {
            r"
            class B
              X = 2
            end
            class C
              include B
              include O::A
              X
            end
            "
        });
        context.index_uri("file:///2.rb", {
            r"
            module O
              module A
                X = 1
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "C", ["C", "O::A", "B", "Object"]);
        assert_constant_reference_to!(context, "O::A::X", "file:///1.rb:7:3-7:4");
    }

    #[test]
    fn incomplete_method_calls_automatically_trigger_singleton_creation() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            "
            class Foo
            end

            Foo.
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context, &[Rule::ParseError]);

        assert_declaration_references_count_eq!(context, "Foo::<Foo>", 1);
        assert_ancestors_eq!(
            context,
            "Foo::<Foo>",
            &["Foo::<Foo>", "Object::<Object>", "Class", "Object"]
        );
    }

    #[test]
    fn singleton_class_calls_create_nested_singletons() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            "
            class Foo
            end

            Foo.singleton_class.singleton_class.to_s
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context);

        assert_declaration_references_count_eq!(context, "Foo::<Foo>::<<Foo>>::<<<Foo>>>", 1);
        assert_ancestors_eq!(
            context,
            "Foo::<Foo>::<<Foo>>::<<<Foo>>>",
            &[
                "Foo::<Foo>::<<Foo>>::<<<Foo>>>",
                "Object::<Object>::<<Object>>::<<<Object>>>",
                "Class::<Class>::<<Class>>",
                "Object::<Object>::<<Object>>",
                "Class::<Class>",
                "Object::<Object>",
                "Class",
                "Object"
            ]
        );
    }

    #[test]
    fn singleton_class_on_a_scoped_constant() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            "
            module Foo
              class Bar
              end
            end

            Foo::Bar.singleton_class.to_s
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context);

        assert_declaration_references_count_eq!(context, "Foo::Bar::<Bar>::<<Bar>>", 1);
        assert_ancestors_eq!(
            context,
            "Foo::Bar::<Bar>::<<Bar>>",
            &[
                "Foo::Bar::<Bar>::<<Bar>>",
                "Object::<Object>::<<Object>>",
                "Class::<Class>",
                "Object::<Object>",
                "Class",
                "Object"
            ]
        );
    }

    #[test]
    fn singleton_class_on_a_self_call() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            "
            class Foo
              class << self
                def bar
                  singleton_class.baz
                end
              end
            end
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context);

        assert_declaration_references_count_eq!(context, "Foo::<Foo>::<<Foo>>", 1);
        assert_ancestors_eq!(
            context,
            "Foo::<Foo>::<<Foo>>",
            &[
                "Foo::<Foo>::<<Foo>>",
                "Object::<Object>::<<Object>>",
                "Class::<Class>",
                "Object::<Object>",
                "Class",
                "Object"
            ]
        );
    }

    #[test]
    fn method_call_on_undefined_constant() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            "
            Foo.bar
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);
        assert_declaration_does_not_exist!(context, "Foo::<Foo>");
    }

    #[test]
    fn rbs_module_and_class_declarations() {
        let mut context = GraphTest::new();
        context.index_rbs_uri("file:///test.rbs", {
            r"
            module Foo
            end

            class Bar
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_declaration_exists!(context, "Foo");
        assert_declaration_exists!(context, "Bar");
    }

    #[test]
    fn rbs_nested_declarations() {
        let mut context = GraphTest::new();
        context.index_rbs_uri("file:///test.rbs", {
            r"
            module Foo
              module Bar
              end

              class Baz
                class Qux
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_owner_eq!(context, "Foo::Bar", "Foo");
        assert_owner_eq!(context, "Foo::Baz", "Foo");
        assert_owner_eq!(context, "Foo::Baz::Qux", "Foo::Baz");
        assert_members_eq!(context, "Foo", ["Bar", "Baz"]);
        assert_members_eq!(context, "Foo::Baz", ["Qux"]);
    }

    #[test]
    fn rbs_qualified_module_name() {
        let mut context = GraphTest::new();
        context.index_rbs_uri("file:///parents.rbs", {
            r"
            module Foo
              module Bar
              end
            end
            "
        });
        context.index_rbs_uri("file:///test.rbs", {
            r"
            module Foo::Bar::Baz
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_declaration_exists!(context, "Foo::Bar::Baz");
    }

    #[test]
    fn rbs_qualified_name_inside_nested_module() {
        let mut context = GraphTest::new();
        context.index_rbs_uri("file:///foo.rbs", {
            r"
            module Outer
              module Foo
              end
            end
            "
        });
        context.index_rbs_uri("file:///test.rbs", {
            r"
            module Outer
              module Foo::Bar
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_owner_eq!(context, "Outer::Foo::Bar", "Outer::Foo");
    }

    #[test]
    fn rbs_superclass_resolution() {
        let mut context = GraphTest::new();
        context.index_rbs_uri("file:///test.rbs", {
            r"
            class Foo
            end

            class Bar < Foo
            end

            module Baz
              class Base
              end

              class Child < Base
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Bar", ["Bar", "Foo", "Object"]);
        assert_ancestors_eq!(context, "Baz::Child", ["Baz::Child", "Baz::Base", "Object"]);
    }

    #[test]
    fn rbs_constant_declarations() {
        let mut context = GraphTest::new();
        context.index_rbs_uri("file:///test.rbs", {
            r"
            FOO: String

            class Bar
              BAZ: Integer
            end

            Bar::QUX: String
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_declaration_exists!(context, "FOO");
        assert_declaration_kind_eq!(context, "FOO", "Constant");

        assert_declaration_exists!(context, "Bar::BAZ");
        assert_declaration_kind_eq!(context, "Bar::BAZ", "Constant");
        assert_owner_eq!(context, "Bar::BAZ", "Bar");

        assert_declaration_exists!(context, "Bar::QUX");
        assert_declaration_kind_eq!(context, "Bar::QUX", "Constant");
    }

    #[test]
    fn rbs_global_declaration() {
        let mut context = GraphTest::new();
        context.index_rbs_uri("file:///test.rbs", "$foo: String");
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Object", ["$foo"]);
    }

    #[test]
    fn rbs_mixin_resolution() {
        let mut context = GraphTest::new();
        context.index_rbs_uri("file:///test.rbs", {
            r"
            module Bar
            end

            module Baz
            end

            class Foo
              include Bar
              include Baz
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["Foo", "Baz", "Bar", "Object"]);
    }

    #[test]
    fn resolving_meta_programming_class_reopened() {
        // It's often not possible to provide first-class support to meta-programming constructs, but we have to prevent
        // the implementation from crashing in cases like these.
        //
        // Here we use some meta-programming method call to define a class and then re-open it using the `class`
        // keyword. The first definition of Bar is considered a constant because we don't know `dynamic_class` returns a
        // new class. The second definition is a class.
        //
        // We need to ensure that the associated Declaration for Bar is transformed into a class if any of its
        // definitions represent one, otherwise we have no place to store the includes and ancestors
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Baz; end

            Bar = dynamic_class do
            end

            class Bar
              include Baz
            end
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_ancestors_eq!(context, "Bar", ["Bar", "Baz", "Object"]);
    }

    #[test]
    fn resolving_accessing_meta_programming_class() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            Foo = Protobuf.some_dynamic_class
            Foo::Bar = Protobuf.some_other_dynamic_class
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
    }

    #[test]
    fn inheriting_from_dynamic_class() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            Foo = some_dynamic_class
            class Bar < Foo
            end
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_ancestors_eq!(context, "Bar", ["Bar", "Object"]);
    }

    #[test]
    fn including_dynamic_module() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            Foo = some_dynamic_module
            class Bar
              include Foo
            end
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_ancestors_eq!(context, "Bar", ["Bar", "Object"]);
    }

    #[test]
    fn prepending_dynamic_module() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            Foo = some_dynamic_module
            class Bar
              prepend Foo
            end
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_ancestors_eq!(context, "Bar", ["Bar", "Object"]);
    }

    #[test]
    fn extending_dynamic_module() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            Foo = some_dynamic_module
            class Bar
              extend Foo

              class << self
              end
            end
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_ancestors_eq!(
            context,
            "Bar::<Bar>",
            [
                "Bar::<Bar>",
                "Object::<Object>",
                // "BasicObject::<BasicObject>",
                "Class",
                // "Module",
                "Object",
                // "Kernel",
                // "BasicObject"
            ]
        );
    }

    #[test]
    fn non_promotable_constant_not_promoted_to_class_with_members() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            FOO = 42
            class FOO
              def bar; end
            end
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_declaration_exists!(context, "FOO");
    }

    #[test]
    fn non_promotable_constant_not_promoted_to_module() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r#"
            FOO = "hello"
            module FOO
            end
            "#
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_declaration_kind_eq!(context, "FOO", "Constant");
    }

    #[test]
    fn promotable_constant_is_promoted_to_class() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Baz; end

            Bar = some_call

            class Bar
              include Baz
            end
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_ancestors_eq!(context, "Bar", ["Bar", "Baz", "Object"]);
    }

    #[test]
    fn mixed_promotable_and_non_promotable_blocks_promotion() {
        // If the same constant has both a promotable and non-promotable definition,
        // promotion should be blocked
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", "Foo = some_call");
        context.index_uri("file:///b.rb", "Foo = 42");
        context.index_uri("file:///c.rb", "class Foo; end");

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_declaration_kind_eq!(context, "Foo", "Constant");
    }

    #[test]
    fn promotable_constant_promoted_to_module() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Baz; end

            Bar = some_call

            module Bar
              include Baz
            end
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_ancestors_eq!(context, "Bar", ["Bar", "Baz"]);
    }

    #[test]
    fn class_first_then_constant_stays_namespace() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo; end
            Foo = some_call
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_declaration_kind_eq!(context, "Foo", "Class");
    }

    #[test]
    fn promotable_constant_path_write() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end
            A::B = some_factory_call
            class A::B; end
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_declaration_exists!(context, "A::B");
    }

    #[test]
    fn ancestor_operations_on_meta_programming_class() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Bar; end

            Qux = dynamic_class do
              include Foo
              prepend Bar
            end
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
    }

    #[test]
    fn method_call_on_promotable_constant() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            Qux = some_factory_call
            Qux.foo
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_declaration_exists!(context, "Qux::<Qux>");
    }

    #[test]
    fn singleton_method_on_non_promotable_constant_does_not_crash() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            FOO = 42
            FOO.bar
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_declaration_does_not_exist!(context, "FOO::<FOO>");
    }

    #[test]
    fn def_self_on_promotable_constant() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            Qux = some_factory_call
            def Qux.foo; end
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_declaration_exists!(context, "Qux::<Qux>");
    }

    #[test]
    fn promoted_constant_has_correct_ancestors() {
        // When a promotable constant is auto-promoted via singleton class access, we conservatively
        // promote to a module (not a class) since we don't know what the call returns.
        // Modules don't inherit from Object.
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            Foo = some_factory_call
            Foo.bar
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_ancestors_eq!(context, "Foo", ["Foo"]);
    }

    #[test]
    fn method_call_on_namespace_alias() {
        // When a method call occurs in a constant alias to a namespace, the singleton class has to be created for the
        // target namespace and not for the alias
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def self.bar; end
            end

            ALIAS = Foo
            ALIAS.bar
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_declaration_exists!(context, "Foo::<Foo>");
        assert_declaration_does_not_exist!(context, "ALIAS::<ALIAS>");
    }

    #[test]
    fn method_def_on_namespace_alias() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
            end

            ALIAS = Foo

            def ALIAS.bar
            end
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_declaration_exists!(context, "Foo::<Foo>");
        assert_declaration_exists!(context, "Foo::<Foo>#bar()");
        assert_declaration_does_not_exist!(context, "ALIAS::<ALIAS>");
    }

    #[test]
    fn meta_programming_class_with_members() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            Foo = dynamic_class do
              def bar; end
            end
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_declaration_exists!(context, "Foo");
        assert_declaration_does_not_exist!(context, "Foo#bar()");
    }

    #[test]
    fn re_opening_constant_alias_as_class() {
        let mut context = GraphTest::new();
        context.index_uri("file:///alias.rb", {
            r"
            module Foo
              class Bar; end
            end

            Baz = Foo::Bar
            "
        });
        context.index_uri("file:///reopen.rb", {
            r"
            CONST = 1

            class Baz
              class Other
                CONST
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);
        assert_declaration_exists!(context, "Baz");
        assert_constant_reference_to!(context, "CONST", "file:///reopen.rb:5:5-5:10");
    }
}
