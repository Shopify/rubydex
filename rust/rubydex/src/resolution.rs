use std::{
    collections::{HashSet, VecDeque, hash_map::Entry},
    hash::BuildHasher,
};

use crate::diagnostic::{Diagnostic, Rule};
use crate::model::{
    built_in::{BASIC_OBJECT_ID, CLASS_ID, KERNEL_ID, MODULE_ID, OBJECT_ID},
    declaration::{
        Ancestor, Ancestors, ClassDeclaration, ClassVariableDeclaration, ConstantAliasDeclaration, ConstantDeclaration,
        Declaration, GlobalVariableDeclaration, InstanceVariableDeclaration, MethodDeclaration, ModuleDeclaration,
        Namespace, SingletonClassDeclaration, TodoDeclaration,
    },
    definitions::{Definition, Mixin, Receiver},
    graph::{Graph, Unit},
    identity_maps::{IdentityHashBuilder, IdentityHashMap, IdentityHashSet},
    ids::{ConstantReferenceId, DeclarationId, DefinitionId, NameId, StringId},
    name::{Name, NameRef, ParentScope},
};

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

    /// Finalize this linearization context for the given declaration. This is intended to be invoked whenever we finish
    /// the linearization algorithm, regardless of whether we are returning a cached result or a freshly built ancestor
    /// chain
    fn finalize(&mut self, declaration_id: DeclarationId) {
        self.descendants.remove(&declaration_id);
        self.seen_ids.remove(&declaration_id);
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

    /// Resolves all pending work and cleans up synthetic declarations.
    /// `prepare_units()` classifies pending work into the namespace/reference
    /// fixpoint (`unit_queue`) and non-namespace definitions (`other_ids`).
    /// After the fixpoint stalls, any leftovers spill back to `pending_work`
    /// for the next `resolve()` call, `other_ids` are handled, and finally
    /// `cleanup_stale_todos` sweeps orphan TODOs.
    ///
    /// # Panics
    ///
    /// Can panic if there's inconsistent data in the graph
    pub fn resolve(&mut self) {
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

        // unit_queue is ephemeral (lives on Resolver), but pending_work persists
        // on Graph across resolve() calls. With incremental invalidation, items
        // can be temporarily unresolvable (e.g. a reference whose target was just
        // deleted but will be re-added). Drain leftovers back to pending_work so
        // they're retried on the next resolve() call.
        self.graph.extend_work(std::mem::take(&mut self.unit_queue));

        self.handle_remaining_definitions(other_ids);

        // See `Graph::tracked_todos` for why this runs here.
        self.graph.cleanup_stale_todos();
    }

    /// Resolves a single constant against the graph. This method is not meant to be used by the resolution phase, but by
    /// the Ruby API
    pub fn resolve_constant(&mut self, name_id: NameId) -> Option<DeclarationId> {
        match self.resolve_constant_internal(name_id) {
            Outcome::Resolved(id, _) => Some(id),
            Outcome::Unresolved(_) | Outcome::Retry(_) => None,
        }
    }

    /// Handles a unit of work for resolving a constant definition or singleton method
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
            Definition::Method(method) if matches!(method.receiver(), Some(Receiver::SelfReceiver(_))) => {
                let Some(Receiver::SelfReceiver(def_id)) = method.receiver() else {
                    unreachable!()
                };
                let str_id = *method.str_id();
                match self.graph.definition_id_to_declaration_id(*def_id) {
                    Some(&owner_decl_id) => match self.get_or_create_singleton_class(owner_decl_id, false) {
                        Some(singleton_id) => {
                            self.create_declaration(str_id, id, singleton_id, |name| {
                                Declaration::Method(Box::new(MethodDeclaration::new(name, singleton_id)))
                            });
                            Outcome::Resolved(singleton_id, None)
                        }
                        // Owner is a non-promotable constant — method is orphaned
                        None => Outcome::Unresolved(None),
                    },
                    // Owning class not resolved yet — retry next pass
                    None => Outcome::Retry(None),
                }
            }
            _ => panic!("Expected constant or singleton method definitions"),
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
    fn handle_reference_unit(&mut self, unit_id: Unit, id: ConstantReferenceId) {
        let constant_ref = self.graph.constant_references().get(&id).unwrap();

        match self.resolve_constant_internal(*constant_ref.name_id()) {
            Outcome::Retry(None) | Outcome::Unresolved(None) => {
                // Retry: dependencies not resolved yet, or name genuinely unknown
                // (which can be temporary during incremental invalidation when the
                // parent namespace was deleted but will be re-added).
                self.unit_queue.push_back(unit_id);
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
        let mut method_visibility_ids = Vec::new();

        for id in other_ids {
            match self.graph.definitions().get(&id).unwrap() {
                Definition::Method(method_definition) => {
                    let str_id = *method_definition.str_id();
                    // SelfReceiver methods are handled in the convergence loop
                    // (handle_definition_unit) to allow singleton class ancestor
                    // linearization. Only ConstantReceiver and regular methods here.
                    let owner_id = match method_definition.receiver() {
                        Some(Receiver::SelfReceiver(_)) => {
                            unreachable!("SelfReceiver methods should be routed to handle_definition_unit");
                        }
                        Some(Receiver::ConstantReceiver(name_id)) => {
                            let Some(receiver_decl_id) = self.resolve_constant_receiver(*name_id, id) else {
                                continue;
                            };

                            let Some(singleton_id) = self.get_or_create_singleton_class(receiver_decl_id, true) else {
                                continue;
                            };

                            singleton_id
                        }
                        None => {
                            let lexical = *method_definition.lexical_nesting_id();
                            let Some(resolved) = self.resolve_lexical_owner(lexical, id) else {
                                continue;
                            };
                            resolved
                        }
                    };

                    self.create_declaration(str_id, id, owner_id, |name| {
                        Declaration::Method(Box::new(MethodDeclaration::new(name, owner_id)))
                    });
                }
                Definition::AttrAccessor(attr) => {
                    let lexical = *attr.lexical_nesting_id();
                    let str_id = *attr.str_id();
                    let Some(owner_id) = self.resolve_lexical_owner(lexical, id) else {
                        continue;
                    };

                    self.create_declaration(str_id, id, owner_id, |name| {
                        Declaration::Method(Box::new(MethodDeclaration::new(name, owner_id)))
                    });
                }
                Definition::AttrReader(attr) => {
                    let lexical = *attr.lexical_nesting_id();
                    let str_id = *attr.str_id();
                    let Some(owner_id) = self.resolve_lexical_owner(lexical, id) else {
                        continue;
                    };

                    self.create_declaration(str_id, id, owner_id, |name| {
                        Declaration::Method(Box::new(MethodDeclaration::new(name, owner_id)))
                    });
                }
                Definition::AttrWriter(attr) => {
                    let lexical = *attr.lexical_nesting_id();
                    let str_id = *attr.str_id();
                    let Some(owner_id) = self.resolve_lexical_owner(lexical, id) else {
                        continue;
                    };

                    self.create_declaration(str_id, id, owner_id, |name| {
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
                                        let Some(receiver_decl_id) = self.resolve_constant_receiver(*name_id, id)
                                        else {
                                            continue;
                                        };
                                        receiver_decl_id
                                    }
                                };

                                // Instance variable in singleton method - owned by the receiver's singleton class
                                let Some(owner_id) = self.get_or_create_singleton_class(receiver_decl_id, true) else {
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
                            let Some(method_owner_id) = self.resolve_lexical_owner(*method.lexical_nesting_id(), id)
                            else {
                                continue;
                            };

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

                            let Some(owner_id) = self.get_or_create_singleton_class(nesting_decl_id, true) else {
                                continue;
                            };
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
                            // The singleton's declaration may be missing (e.g. its receiver was
                            // just deleted). Re-queue and let the next resolve place `@bar` on
                            // the right owner instead of falling back to Object.
                            let Some(&singleton_class_decl_id) = self.graph.definition_id_to_declaration_id(nesting_id)
                            else {
                                self.graph.push_work(Unit::Definition(id));
                                continue;
                            };
                            let owner_id = self
                                .get_or_create_singleton_class(singleton_class_decl_id, true)
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
                    // Method aliases operate on instance methods. The SelfReceiver arm is for
                    // RBS `alias self.x self.y`.
                    let new_name_str_id = *alias.new_name_str_id();
                    let owner_id = match alias.receiver() {
                        Some(Receiver::SelfReceiver(def_id)) => {
                            let decl_id = *self
                                .graph
                                .definition_id_to_declaration_id(*def_id)
                                .expect("SelfReceiver definition should have a declaration");
                            let Some(owner_id) = self.get_or_create_singleton_class(decl_id, true) else {
                                continue;
                            };
                            owner_id
                        }
                        Some(Receiver::ConstantReceiver(name_id)) => {
                            let Some(resolved) = self.resolve_constant_receiver(*name_id, id) else {
                                continue;
                            };
                            resolved
                        }
                        None => {
                            let lexical = *alias.lexical_nesting_id();
                            let Some(resolved) = self.resolve_lexical_owner(lexical, id) else {
                                continue;
                            };
                            resolved
                        }
                    };

                    self.create_declaration(new_name_str_id, id, owner_id, |name| {
                        Declaration::Method(Box::new(MethodDeclaration::new(name, owner_id)))
                    });
                }
                Definition::GlobalVariableAlias(alias) => {
                    self.create_declaration(*alias.new_name_str_id(), id, *OBJECT_ID, |name| {
                        Declaration::GlobalVariable(Box::new(GlobalVariableDeclaration::new(name, *OBJECT_ID)))
                    });
                }
                Definition::ConstantVisibility(constant_visibility) => {
                    // Both `private_constant` and `public_constant` can only target direct members.
                    // Inheritance or surrounding lexical scopes are not taken into account.
                    let receiver = *constant_visibility.receiver();
                    let target = *constant_visibility.target();
                    let uri_id = *constant_visibility.uri_id();
                    let offset = constant_visibility.offset().clone();
                    let lexical_nesting_id = *constant_visibility.lexical_nesting_id();
                    let constant_name = self.graph.strings().get(&target).unwrap().as_str().to_string();

                    let owner_id = if let Some(receiver_name_id) = receiver {
                        let NameRef::Resolved(resolved_receiver) = self.graph.names().get(&receiver_name_id).unwrap()
                        else {
                            continue;
                        };
                        let Some(namespace_id) = self.resolve_to_namespace(*resolved_receiver.declaration_id()) else {
                            continue;
                        };
                        namespace_id
                    } else {
                        let Some(decl_id) = self.resolve_lexical_owner(lexical_nesting_id, id) else {
                            continue;
                        };
                        decl_id
                    };

                    let Some(Declaration::Namespace(namespace)) = self.graph.declarations().get(&owner_id) else {
                        continue;
                    };

                    if let Some(member) = namespace
                        .member(&target)
                        .and_then(|member_id| self.graph.declarations().get(member_id))
                        && matches!(
                            member,
                            Declaration::Constant(_)
                                | Declaration::ConstantAlias(_)
                                | Declaration::Namespace(Namespace::Class(_) | Namespace::Module(_))
                        )
                    {
                        // `add_declaration` deduplicates by fully qualified name, so this appends
                        // the visibility definition to the existing constant declaration.
                        self.graph.add_declaration(id, member.name().to_string(), |name| {
                            Declaration::Constant(Box::new(ConstantDeclaration::new(name, owner_id)))
                        });
                    } else {
                        let diagnostic = Diagnostic::new(
                            Rule::UndefinedConstantVisibilityTarget,
                            uri_id,
                            offset,
                            format!(
                                "undefined constant `{constant_name}` for visibility change in `{}`",
                                namespace.name()
                            ),
                        );
                        self.graph.add_document_diagnostic(uri_id, diagnostic);
                    }
                }
                Definition::MethodVisibility(_) => {
                    method_visibility_ids.push(id);
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

        self.resolve_method_visibilities(method_visibility_ids);
    }

    /// Resolves retroactive method visibility changes (`private :foo`, `protected :foo`, `public :foo`).
    ///
    /// Runs as a second pass after all methods/attrs are declared, so `private :bar` works
    /// regardless of whether `def bar` appeared before or after it in source.
    fn resolve_method_visibilities(&mut self, visibility_ids: Vec<DefinitionId>) {
        let mut pending_work = Vec::new();

        for id in visibility_ids {
            let Definition::MethodVisibility(method_visibility) = self.graph.definitions().get(&id).unwrap() else {
                unreachable!()
            };

            let str_id = *method_visibility.str_id();
            let uri_id = *method_visibility.uri_id();
            let offset = method_visibility.offset().clone();
            let lexical_nesting_id = *method_visibility.lexical_nesting_id();

            let Some(owner_id) = self.resolve_lexical_owner(lexical_nesting_id, id) else {
                continue;
            };

            let Some(Declaration::Namespace(namespace)) = self.graph.declarations().get(&owner_id) else {
                continue;
            };

            let mut visibility_applied = false;
            let mut has_partial = false;

            for ancestor in namespace.ancestors() {
                match ancestor {
                    Ancestor::Complete(ancestor_id) => {
                        let has_member = self
                            .graph
                            .declarations()
                            .get(ancestor_id)
                            .and_then(|decl| decl.as_namespace())
                            .and_then(|ns| ns.member(&str_id))
                            .is_some();

                        if has_member {
                            // Direct member: `create_declaration`'s fully qualified name dedup attaches
                            // this visibility definition to the existing method declaration.
                            // Inherited: a new child-owned declaration is created.
                            self.create_declaration(str_id, id, owner_id, |name| {
                                Declaration::Method(Box::new(MethodDeclaration::new(name, owner_id)))
                            });
                            visibility_applied = true;
                            break;
                        }
                    }
                    Ancestor::Partial(_) => has_partial = true,
                }
            }

            if visibility_applied {
                continue;
            }

            if has_partial {
                // Method might exist on an unresolved ancestor — requeue for retry.
                pending_work.push(Unit::Definition(id));
            } else {
                // Ancestors are fully resolved — method definitively doesn't exist.
                let method_name = self.graph.strings().get(&str_id).unwrap().as_str().to_string();
                let owner_name = self.graph.declarations().get(&owner_id).unwrap().name().to_string();
                let diagnostic = Diagnostic::new(
                    Rule::UndefinedMethodVisibilityTarget,
                    uri_id,
                    offset,
                    format!("undefined method `{method_name}` for visibility change in `{owner_name}`"),
                );
                self.graph
                    .declarations_mut()
                    .get_mut(&owner_id)
                    .unwrap()
                    .add_diagnostic(diagnostic);
            }
        }

        // Must extend work here so incremental resolution can resolve previously unresolved visibility operations
        self.graph.extend_work(pending_work);
    }

    /// Resolves a constant receiver for `handle_remaining_definitions`.
    /// If the receiver name is unresolved, preserve the definition for a later
    /// resolve cycle instead of dropping work during an incremental delete/re-add gap.
    fn resolve_constant_receiver(&mut self, name_id: NameId, id: DefinitionId) -> Option<DeclarationId> {
        match self.graph.names().get(&name_id).unwrap() {
            NameRef::Resolved(resolved) => Some(*resolved.declaration_id()),
            NameRef::Unresolved(_) => {
                self.graph.push_work(Unit::Definition(id));
                None
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

    /// Resolves owner for class variables, bypassing singleton classes. Returns `None` if the owner can't be
    /// determined (e.g., unresolved constant alias).
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
        let declaration_id = current_nesting.and_then(|id| self.graph.definition_id_to_declaration_id(id).copied())?;

        // If the declaration is a constant alias, follow the alias chain to find the
        // target namespace. Returns None if the alias target is unresolved.
        if matches!(
            self.graph.declarations().get(&declaration_id),
            Some(Declaration::ConstantAlias(_))
        ) {
            self.resolve_to_namespace(declaration_id)
        } else {
            Some(declaration_id)
        }
    }

    /// Resolves owner from lexical nesting.
    ///
    /// If the owner cannot be resolved yet, re-queues the current definition so
    /// a later resolve cycle can retry instead of permanently dropping it.
    fn resolve_lexical_owner(
        &mut self,
        lexical_nesting_id: Option<DefinitionId>,
        definition_id: DefinitionId,
    ) -> Option<DeclarationId> {
        let mut current_nesting = lexical_nesting_id;

        let resolved = loop {
            let Some(id) = current_nesting else {
                break Some(*OBJECT_ID);
            };

            // If no declaration exists yet for this definition, walk up the lexical chain.
            // This handles the case where attr_* definitions inside methods are processed
            // before the method definition itself. A SingletonClass with no declaration
            // is an exception: returning the surrounding scope would attach its members to
            // the wrong owner (e.g. `Object`) and never recover, so retry later instead.
            let Some(declaration_id) = self.graph.definition_id_to_declaration_id(id) else {
                let definition = self.graph.definitions().get(&id).unwrap();
                if matches!(definition, Definition::SingletonClass(_)) {
                    break None;
                }
                current_nesting = *definition.lexical_nesting_id();
                continue;
            };

            let decl = self.graph.declarations().get(declaration_id).unwrap();

            // If the associated declaration is a namespace that can own things, we found the right owner. Otherwise, we might
            // have found something nested inside something else (like a method), in which case we have to walk up until we find
            // the appropriate owner.
            if matches!(
                decl,
                Declaration::Namespace(Namespace::Class(_) | Namespace::Module(_) | Namespace::SingletonClass(_))
            ) {
                break Some(*declaration_id);
            }

            if matches!(decl, Declaration::ConstantAlias(_)) {
                // Follow the alias chain to find the target namespace. If the alias is unresolved,
                // the definition cannot be properly owned yet and should be retried later.
                break self.resolve_to_namespace(*declaration_id);
            }

            let definition = self.graph.definitions().get(&id).unwrap();
            current_nesting = *definition.lexical_nesting_id();
        };

        if resolved.is_none() {
            self.graph.push_work(Unit::Definition(definition_id));
        }

        resolved
    }

    /// Gets or creates a singleton class declaration for a given class/module declaration.  For class `Foo`, this
    /// returns the declaration for `Foo::<Foo>`.
    ///
    /// If the declaration is a `Constant` with all-promotable definitions, it is automatically promoted to a `Class`
    /// namespace before creating the singleton. Returns `None` if the declaration is not a namespace and cannot be
    /// promoted (e.g., `FOO = 42`).
    /// When `eager_ancestors` is `true`, ancestor chains are linearized inline (used after the convergence loop when all
    /// namespaces are resolved). When `false`, a `Unit::Ancestors` item is enqueued for the convergence loop to process.
    fn get_or_create_singleton_class(
        &mut self,
        attached_id: DeclarationId,
        eager_ancestors: bool,
    ) -> Option<DeclarationId> {
        let attached_decl = self.graph.declarations().get(&attached_id).unwrap();

        // If the attached object is a constant alias, follow the alias chain to find the actual namespace
        if matches!(attached_decl, Declaration::ConstantAlias(_)) {
            return match self.resolve_to_namespace(attached_id) {
                Some(id) => self.get_or_create_singleton_class(id, eager_ancestors),
                None => None,
            };
        }

        if matches!(attached_decl, Declaration::Constant(_)) {
            if self.graph.all_definitions_promotable(attached_decl) {
                self.graph.promote_constant_to_namespace(attached_id, |name, owner_id| {
                    Declaration::Namespace(Namespace::Module(Box::new(ModuleDeclaration::new(name, owner_id))))
                });

                if eager_ancestors {
                    let _ = self.ancestors_of(attached_id);
                } else {
                    self.unit_queue.push_back(Unit::Ancestors(attached_id));
                }
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

        if eager_ancestors {
            let _ = self.ancestors_of(decl_id);
        } else {
            self.unit_queue.push_back(Unit::Ancestors(decl_id));
        }

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

                context.finalize(declaration_id);
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

                context.finalize(declaration_id);
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

        let is_singleton_class = matches!(declaration, Declaration::Namespace(Namespace::SingletonClass(_)));

        // If we're linearizing a singleton class, add the extends of the attached class to the list of mixins to process
        if is_singleton_class {
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

        // Collect prepends and includes for the current declaration, noting if any extends exist
        let mut has_extends = false;

        for definition_id in declaration.definitions() {
            if let Some(def_mixins) = self.mixins_of(*definition_id) {
                for mixin in def_mixins {
                    match mixin {
                        Mixin::Prepend(_) | Mixin::Include(_) => mixins.push(mixin),
                        Mixin::Extend(_) => has_extends = true,
                    }
                }
            }
        }

        // Ensure that we create the singleton and enqueue it for linearization if we see an extend
        if has_extends && !is_singleton_class {
            self.get_or_create_singleton_class(declaration_id, false);
        }

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

        context.finalize(declaration_id);
        result
    }

    fn linearize_parent_ancestors(
        &mut self,
        declaration_id: DeclarationId,
        context: &mut LinearizationContext,
    ) -> Option<Vec<Ancestor>> {
        if declaration_id == *BASIC_OBJECT_ID {
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
                            let Some(module_id) = self.resolve_to_namespace(*resolved.declaration_id()) else {
                                continue;
                            };

                            let ids = match self.linearize_ancestors(module_id, context) {
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
                            let Some(module_id) = self.resolve_to_namespace(*resolved.declaration_id()) else {
                                continue;
                            };

                            let mut ids = match self.linearize_ancestors(module_id, context) {
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

        let outcome = match self.name_owner_id(name_id, singleton) {
            // name_owner_id returns Unresolved(None) only when the parent scope is genuinely
            // unknown (e.g., `class A::B::C` where `A` doesn't exist). For regular declarations
            // we create Todo placeholders for the missing parent chain so `B::C` can still be
            // placed; Todos get promoted when real definitions appear later.
            //
            // Singleton classes are the exception: `class << UndefinedReceiver` attaches via
            // `set_singleton_class_id`, not `add_member`, so a TODO receiver would never gain a
            // member. Emit Retry so the unit is preserved for a later resolve where the receiver
            // may exist.
            Outcome::Unresolved(None) if singleton => Outcome::Retry(None),
            Outcome::Unresolved(None) if !singleton => Outcome::Resolved(self.create_todo_for_parent(name_id), None),
            other => other,
        };

        // The name of the declaration is determined by the name of its owner, which may or may not require resolution
        // depending on whether the name has a parent scope
        match outcome {
            Outcome::Resolved(owner_id, id_needing_linearization) => {
                let mut fully_qualified_name = self.graph.strings().get(&str_id).unwrap().to_string();

                // If the owner is a promotable constant and something is being defined inside it, promote it to a
                // module
                {
                    let owner = self.graph.declarations().get(&owner_id).unwrap();
                    let is_promotable_constant =
                        matches!(owner, Declaration::Constant(_)) && self.graph.all_definitions_promotable(owner);

                    if is_promotable_constant {
                        self.graph.promote_constant_to_namespace(owner_id, |name, owner_id| {
                            Declaration::Namespace(Namespace::Module(Box::new(ModuleDeclaration::new(name, owner_id))))
                        });
                        self.unit_queue.push_back(Unit::Ancestors(owner_id));
                    }
                }

                let owner = self.graph.declarations().get(&owner_id).unwrap();
                let owner_is_namespace = owner.as_namespace().is_some();

                // Skip creating singletons when the target is a not a namespace or not promotable. For example:
                // Foo = 1
                // class << Foo; end
                if singleton && !owner_is_namespace {
                    return Outcome::Unresolved(None);
                }

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
    //
    // When `preserve_retry` is true, Retry from resolve_constant_internal is NOT folded into
    // Unresolved(None). This is used by the singleton path so the unit can retry when the
    // receiver might resolve later rather than being dropped.
    fn name_owner_id(&mut self, name_id: NameId, preserve_retry: bool) -> Outcome {
        let name_ref = self.graph.names().get(&name_id).unwrap();

        if let Some(&parent_scope) = name_ref.parent_scope().as_ref() {
            // If we have `A::B`, the owner of `B` is whatever `A` resolves to.
            // If `A` is an alias, resolve through to get the actual namespace.
            match self.resolve_constant_internal(parent_scope) {
                Outcome::Resolved(id, linearization) => self.resolve_to_primary_namespace(id, linearization),
                // The parent scope is genuinely unknown — not a circular alias or pending
                // linearization, but a name that doesn't exist anywhere in the graph.
                Outcome::Unresolved(None) => Outcome::Unresolved(None),
                Outcome::Retry(None) if !preserve_retry => Outcome::Unresolved(None),
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

    /// For `class A::B::C` where `A` can't be resolved, creates a Todo declaration for `A`
    /// so `B::C` can still be placed. Recurses for multi-level cases. Todos get promoted
    /// when real definitions appear later.
    fn create_todo_for_parent(&mut self, name_id: NameId) -> DeclarationId {
        let name_ref = self.graph.names().get(&name_id).unwrap();
        let parent_scope = *name_ref.parent_scope().as_ref().unwrap();

        let parent_name = self.graph.names().get(&parent_scope).unwrap();
        let parent_str_id = *parent_name.str();
        let parent_has_parent_scope = parent_name.parent_scope().as_ref().is_some();
        // Non-Lexical Lifetimes: borrow of parent_name ends here

        // For `class A::B::C` where `A` is bare (no `::` prefix), place the Todo under
        // Object so it becomes top-level `A`. This way `module A; end` appearing later
        // promotes it correctly. Using nesting would incorrectly create `SomeModule::A`.
        let parent_owner_id = if parent_has_parent_scope {
            match self.name_owner_id(parent_scope, false) {
                Outcome::Resolved(id, _) => id,
                _ => self.create_todo_for_parent(parent_scope),
            }
        } else {
            *OBJECT_ID
        };

        // Ensure we follow constant aliases if that's the parent
        let parent_owner_id = match self.resolve_to_primary_namespace(parent_owner_id, None) {
            Outcome::Resolved(id, _) => id,
            _ => *OBJECT_ID,
        };

        let fully_qualified_name = if parent_owner_id == *OBJECT_ID {
            self.graph.strings().get(&parent_str_id).unwrap().to_string()
        } else {
            format!(
                "{}::{}",
                self.graph.declarations().get(&parent_owner_id).unwrap().name(),
                self.graph.strings().get(&parent_str_id).unwrap().as_str()
            )
        };

        let declaration_id = DeclarationId::from(&fully_qualified_name);

        if let Entry::Vacant(e) = self.graph.declarations_mut().entry(declaration_id) {
            e.insert(Declaration::Namespace(Namespace::Todo(Box::new(TodoDeclaration::new(
                fully_qualified_name,
                parent_owner_id,
            )))));
            self.graph.add_member(&parent_owner_id, declaration_id, parent_str_id);
            self.graph.track_todo(declaration_id);
        }

        declaration_id
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
                        let result = self.search_ancestors(*OBJECT_ID, *name.str());

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
                        let Some(singleton_id) = self.get_or_create_singleton_class(target_decl_id, false) else {
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

    /// If `declaration_id` is already a namespace, returns it directly. If it's a `ConstantAlias`, follows the alias
    /// chain and returns the first namespace found. Returns `None` for all other declaration types or unresolved alias
    /// chains.
    fn resolve_to_namespace(&self, declaration_id: DeclarationId) -> Option<DeclarationId> {
        match self.graph.declarations().get(&declaration_id)? {
            Declaration::Namespace(_) => Some(declaration_id),
            Declaration::ConstantAlias(_) => self
                .resolve_alias_chains(declaration_id)
                .into_iter()
                .find(|id| self.graph.is_namespace(id)),
            _ => None,
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

        if let Some(nesting) = name.nesting() {
            let scope_outcome = self.search_lexical_scopes(name, str_id);

            // If we already resolved or need to retry, return early
            if scope_outcome.is_resolved_or_retry() {
                return scope_outcome;
            }

            let (ancestor_outcome, nesting_decl_id) = match self.graph.names().get(nesting).unwrap() {
                NameRef::Resolved(nesting_name_ref) => {
                    let resolved_ids = self.resolve_alias_chains(*nesting_name_ref.declaration_id());
                    let mut result = Outcome::Unresolved(None);
                    let mut decl_id = None;

                    for &id in &resolved_ids {
                        match self.graph.declarations().get(&id) {
                            Some(Declaration::ConstantAlias(_)) => {
                                result = Outcome::Retry(None);
                                break;
                            }
                            Some(Declaration::Namespace(_)) => {
                                decl_id = Some(id);
                                result = self.search_ancestors(id, str_id);
                                break;
                            }
                            _ => {}
                        }
                    }

                    (result, decl_id)
                }
                NameRef::Unresolved(_) => (Outcome::Retry(None), None),
            };

            if matches!(ancestor_outcome, Outcome::Resolved(..)) {
                return ancestor_outcome;
            }

            // Modules don't inherit from Object, but Ruby gives them a special fallback to Object's ancestors.
            // For incomplete ancestor chains, we also try Object as a tentative resolution to avoid unnecessary retries.
            let is_module = nesting_decl_id.is_some_and(|id| {
                matches!(
                    self.graph.declarations().get(&id),
                    Some(Declaration::Namespace(Namespace::Module(_) | Namespace::Todo(_)))
                )
            });
            let chain_incomplete = matches!(ancestor_outcome, Outcome::Retry(Some(_)) | Outcome::Unresolved(Some(_)));

            if is_module || chain_incomplete {
                let object_outcome = self.search_ancestors(*OBJECT_ID, str_id);

                if let Outcome::Resolved(decl_id, _) = object_outcome {
                    // Preserve the linearization ID so the chain gets re-checked once complete
                    let linearization_id = match ancestor_outcome {
                        Outcome::Retry(id) | Outcome::Unresolved(id) => id,
                        Outcome::Resolved(..) => unreachable!("guarded by early return above"),
                    };
                    return Outcome::Resolved(decl_id, linearization_id);
                }
            }

            return ancestor_outcome;
        }

        // When there's no nesting, we're working at the top level of a script. The top level is the magic `<main>`
        // object, which is an instance of `Object`. To resolve constants at the top level, we need to search the
        // ancestors of `Object`
        self.search_ancestors(*OBJECT_ID, str_id)
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

                if let Some(namespace_id) = self.resolve_to_namespace(declaration_id)
                    && let Some(namespace) = self.graph.declarations().get(&namespace_id).unwrap().as_namespace()
                    && let Some(member) = namespace.member(&str_id)
                {
                    return Outcome::Resolved(*member, None);
                }

                current_name = nesting_name_ref.name();
            } else {
                return Outcome::Retry(None);
            }
        }

        Outcome::Unresolved(None)
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
    /// Compute the depth of a name in the graph by recursively summing the depths of its
    /// `parent_scope` and `nesting` chains. Results are memoized in `cache` (`NameId` → depth)
    /// so each name is computed at most once across all calls.
    ///
    /// Depth represents the total complexity of a name's position in the namespace hierarchy.
    /// For example, in `module Foo; module Bar; class Baz; end; end; end`, Foo has depth 1
    /// (top-level), Bar has depth 2, and Baz has depth 3.
    ///
    /// # Panics
    ///
    /// Will panic if there is inconsistent data in the graph
    fn name_depth(
        name_id: NameId,
        names: &IdentityHashMap<NameId, NameRef>,
        cache: &mut IdentityHashMap<NameId, u32>,
    ) -> u32 {
        if let Some(&depth) = cache.get(&name_id) {
            return depth;
        }

        let name = names.get(&name_id).unwrap();

        let depth = if name.parent_scope().is_top_level() {
            1
        } else {
            let parent_depth = name.parent_scope().map_or(0, |id| Self::name_depth(*id, names, cache));

            let nesting_depth = name.nesting().map_or(0, |id| Self::name_depth(id, names, cache));

            parent_depth + nesting_depth + 1
        };

        cache.insert(name_id, depth);
        depth
    }

    /// Pre-compute name depths for all names into a `NameId → depth` map. Each name's depth is
    /// computed once via memoized recursion, then used as an O(1) lookup key during sorting in
    /// `prepare_units`.
    fn compute_name_depths(names: &IdentityHashMap<NameId, NameRef>) -> IdentityHashMap<NameId, u32> {
        let mut cache = IdentityHashMap::with_capacity_and_hasher(names.len(), IdentityHashBuilder);

        for &name_id in names.keys() {
            Self::name_depth(name_id, names, &mut cache);
        }

        cache
    }

    /// Drains `pending_work` and classifies items into the resolution queue.
    /// Namespace definitions and constant references are sorted by name depth for deterministic
    /// resolution order. Non-namespace definitions (methods, attrs, variables) are returned
    /// separately for `handle_remaining_definitions`.
    fn prepare_units(&mut self) -> Vec<DefinitionId> {
        let work = self.graph.take_pending_work();
        let estimated = work.len() / 2;
        let mut definitions = Vec::with_capacity(estimated);
        let mut others = Vec::with_capacity(estimated);
        let mut singleton_methods = Vec::new();
        let mut const_refs = Vec::new();
        let mut ancestors = vec![*BASIC_OBJECT_ID, *KERNEL_ID, *OBJECT_ID, *MODULE_ID, *CLASS_ID];
        let names = self.graph.names();
        let depths = Self::compute_name_depths(names);

        // Dedup: when multiple files are indexed before resolution runs, pending_work accumulates
        // and the same definition/reference ID can be enqueued more than once.
        let mut seen_defs = IdentityHashSet::<DefinitionId>::default();
        let mut seen_references = IdentityHashSet::<ConstantReferenceId>::default();
        let mut seen_ancestors = IdentityHashSet::<DeclarationId>::default();

        for unit in work {
            match unit {
                Unit::Definition(id) => {
                    if !seen_defs.insert(id) {
                        continue;
                    }
                    // Definition may have been removed by remove_document_data — skip stale items
                    let Some(definition) = self.graph.definitions().get(&id) else {
                        continue;
                    };
                    let uri = self.graph.documents().get(definition.uri_id()).unwrap().uri();

                    match definition {
                        Definition::Class(def) => {
                            definitions.push((Unit::Definition(id), (*def.name_id(), uri, definition.offset())));
                        }
                        Definition::Module(def) => {
                            definitions.push((Unit::Definition(id), (*def.name_id(), uri, definition.offset())));
                        }
                        Definition::Constant(def) => {
                            definitions.push((Unit::Definition(id), (*def.name_id(), uri, definition.offset())));
                        }
                        Definition::ConstantAlias(def) => {
                            definitions.push((Unit::Definition(id), (*def.name_id(), uri, definition.offset())));
                        }
                        Definition::SingletonClass(def) => {
                            definitions.push((Unit::Definition(id), (*def.name_id(), uri, definition.offset())));
                        }
                        // SelfReceiver methods create singleton classes, which need
                        // ancestor linearization. Process them in the convergence loop
                        // so Unit::Ancestors items are handled naturally.
                        Definition::Method(method) if matches!(method.receiver(), Some(Receiver::SelfReceiver(_))) => {
                            singleton_methods.push(Unit::Definition(id));
                        }
                        _ => {
                            others.push(id);
                        }
                    }
                }
                Unit::ConstantRef(id) => {
                    if !seen_references.insert(id) {
                        continue;
                    }
                    // Reference may have been removed by remove_document_data — skip stale items
                    let Some(constant_ref) = self.graph.constant_references().get(&id) else {
                        continue;
                    };
                    let uri = self.graph.documents().get(&constant_ref.uri_id()).unwrap().uri();
                    const_refs.push((
                        Unit::ConstantRef(id),
                        (*constant_ref.name_id(), uri, constant_ref.offset()),
                    ));
                }
                Unit::Ancestors(id) => {
                    if !seen_ancestors.insert(id) {
                        continue;
                    }
                    // Declaration may have been removed by invalidation — skip stale items
                    if self.graph.declarations().contains_key(&id) {
                        ancestors.push(id);
                    }
                }
            }
        }

        // Sort namespaces based on their name complexity so that simpler names are always first
        // When the depth is the same, sort by URI and offset to maintain determinism
        definitions.sort_unstable_by(|(_, (name_a, uri_a, offset_a)), (_, (name_b, uri_b, offset_b))| {
            (depths.get(name_a).unwrap(), uri_a, offset_a).cmp(&(depths.get(name_b).unwrap(), uri_b, offset_b))
        });

        const_refs.sort_unstable_by(|(_, (name_a, uri_a, offset_a)), (_, (name_b, uri_b, offset_b))| {
            (depths.get(name_a).unwrap(), uri_a, offset_a).cmp(&(depths.get(name_b).unwrap(), uri_b, offset_b))
        });

        // Definitions first, then constant refs, then singleton methods, then ancestors
        self.unit_queue.extend(definitions.into_iter().map(|(id, _)| id));
        self.unit_queue.extend(const_refs.into_iter().map(|(id, _)| id));
        self.unit_queue.extend(singleton_methods);
        self.unit_queue.extend(ancestors.into_iter().map(Unit::Ancestors));

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
        // Base case: if we reached `BasicObject`, then the parent is `Class`
        if attached_id == *BASIC_OBJECT_ID {
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
                    self.get_or_create_singleton_class(inner_parent, false)
                        .expect("singleton parent should always be a namespace"),
                    partial,
                )
            }
            Declaration::Namespace(Namespace::Class(_)) => {
                // For classes (the regular case), we need to return the singleton class of its parent
                let definition_ids = decl.definitions().to_vec();

                let (picked_parent, unresolved_parent) = self.get_parent_class(&definition_ids);
                (
                    self.get_or_create_singleton_class(picked_parent, false)
                        .expect("parent class should always be a namespace"),
                    unresolved_parent.is_some(),
                )
            }
            _ => {
                // Other declaration types (constants, methods, etc.) shouldn't reach here,
                // but default to Object's singleton parent
                (*CLASS_ID, false)
            }
        }
    }

    fn get_parent_class(&self, definition_ids: &[DefinitionId]) -> (DeclarationId, Option<NameId>) {
        let mut explicit_parents = Vec::new();
        let mut unresolved_parent = None;

        for definition_id in definition_ids {
            let definition = self.graph.definitions().get(definition_id).unwrap();

            if let Definition::Class(class) = definition
                && let Some(superclass) = class.superclass_ref()
            {
                let constant_reference = self.graph.constant_references().get(superclass).unwrap();
                let name = self.graph.names().get(constant_reference.name_id()).unwrap();

                match name {
                    NameRef::Resolved(resolved) => {
                        if let Some(parent_id) = self.resolve_to_namespace(*resolved.declaration_id()) {
                            explicit_parents.push(parent_id);
                        }
                    }
                    NameRef::Unresolved(_) => {
                        unresolved_parent = Some(*constant_reference.name_id());
                    }
                }
            }
        }

        // If there's more than one parent class that isn't `Object` and they are different, then there's a superclass
        // mismatch error. TODO: We should add a diagnostic here
        (
            explicit_parents.first().copied().unwrap_or(*OBJECT_ID),
            unresolved_parent,
        )
    }

    fn linearize_parent_class(
        &mut self,
        definition_ids: &[DefinitionId],
        context: &mut LinearizationContext,
    ) -> Ancestors {
        let (picked_parent, unresolved_parent) = self.get_parent_class(definition_ids);
        let mut result = self.linearize_ancestors(picked_parent, context);

        if let Some(name_id) = unresolved_parent {
            context.partial = true;

            // Insert the unresolved parent as a Partial ancestor at the front of the chain, so it
            // appears before the default Object ancestors
            let ancestors = match &mut result {
                Ancestors::Complete(ids) | Ancestors::Cyclic(ids) | Ancestors::Partial(ids) => ids,
            };
            ancestors.insert(0, Ancestor::Partial(name_id));

            result.to_partial()
        } else {
            result
        }
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
#[path = "resolution_tests.rs"]
mod tests;
