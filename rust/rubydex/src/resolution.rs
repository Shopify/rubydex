use std::{
    collections::{HashSet, VecDeque},
    hash::BuildHasher,
    ops::Deref,
};

use crate::{
    diagnostic::{Diagnostic, Rule},
    model::{
        declaration::{
            Ancestor, Ancestors, ClassDeclaration, ClassVariableDeclaration, ConstantAliasDeclaration,
            ConstantDeclaration, Declaration, DeclarationKind, GlobalVariableDeclaration, InstanceVariableDeclaration,
            MethodDeclaration, ModuleDeclaration, Namespace, SingletonClassDeclaration,
        },
        definitions::{Definition, Mixin},
        graph::{CLASS_ID, Graph, MODULE_ID, OBJECT_ID},
        identity_maps::{IdentityHashMap, IdentityHashSet},
        ids::{DeclarationId, DefinitionId, NameId, ReferenceId, StringId},
        name::{Name, NameRef},
    },
};

pub enum Unit {
    /// A definition that defines a constant and might require resolution
    Definition(DefinitionId),
    /// A constant reference that needs to be resolved
    Reference(ReferenceId),
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
    /// this back in the queue to retry once we have progressed further
    Retry,
}

impl Outcome {
    fn is_resolved_or_retry(&self) -> bool {
        matches!(self, Outcome::Resolved(_, _) | Outcome::Retry)
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
}

impl<'a> Resolver<'a> {
    pub fn new(graph: &'a mut Graph) -> Self {
        Self { graph }
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

        let (mut unit_queue, other_ids) = self.sorted_units();

        loop {
            // Flag to ensure the end of the resolution loop. We go through all items in the queue based on its current
            // length. If we made any progress in this pass of the queue, we can continue because we're unlocking more work
            // to be done
            let mut made_progress = false;

            // Loop through the current length of the queue, which won't change during this pass. Retries pushed to the back
            // are only processed in the next pass, so that we can assess whether we made any progress
            for _ in 0..unit_queue.len() {
                let Some(unit_id) = unit_queue.pop_front() else {
                    break;
                };

                match unit_id {
                    Unit::Definition(id) => {
                        self.handle_definition_unit(&mut unit_queue, &mut made_progress, unit_id, id);
                    }
                    Unit::Reference(id) => {
                        self.handle_reference_unit(&mut unit_queue, &mut made_progress, unit_id, id);
                    }
                    Unit::Ancestors(id) => {
                        self.handle_ancestor_unit(&mut unit_queue, &mut made_progress, id);
                    }
                }
            }

            if !made_progress || unit_queue.is_empty() {
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
            Outcome::Unresolved(_) | Outcome::Retry => None,
        }
    }

    /// Handles a unit of work for resolving a constant definition
    fn handle_definition_unit(
        &mut self,
        unit_queue: &mut VecDeque<Unit>,
        made_progress: &mut bool,
        unit_id: Unit,
        id: DefinitionId,
    ) {
        let outcome = match self.graph.definitions().get(&id).unwrap() {
            Definition::Class(class) => {
                self.handle_constant_declaration(*class.name_id(), id, false, |name, owner_id| {
                    Declaration::Namespace(Namespace::Class(Box::new(ClassDeclaration::new(name, owner_id))))
                })
            }
            Definition::Module(module) => {
                self.handle_constant_declaration(*module.name_id(), id, false, |name, owner_id| {
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
                    Declaration::Namespace(Namespace::SingletonClass(Box::new(SingletonClassDeclaration::new(
                        name, owner_id,
                    ))))
                })
            }
            _ => panic!("Expected constant definitions"),
        };

        match outcome {
            Outcome::Retry => {
                // There might be dependencies we haven't figured out yet, so we need to retry
                unit_queue.push_back(unit_id);
            }
            Outcome::Unresolved(None) => {
                // We couldn't resolve this name. Emit a diagnostic
            }
            Outcome::Unresolved(Some(id_needing_linearization)) => {
                unit_queue.push_back(unit_id);
                unit_queue.push_back(Unit::Ancestors(id_needing_linearization));
            }
            Outcome::Resolved(id, None) => {
                unit_queue.push_back(Unit::Ancestors(id));
                *made_progress = true;
            }
            Outcome::Resolved(_, Some(id_needing_linearization)) => {
                unit_queue.push_back(Unit::Ancestors(id_needing_linearization));
                *made_progress = true;
            }
        }
    }

    /// Handles a unit of work for resolving a constant reference
    fn handle_reference_unit(
        &mut self,
        unit_queue: &mut VecDeque<Unit>,
        made_progress: &mut bool,
        unit_id: Unit,
        id: ReferenceId,
    ) {
        let constant_ref = self.graph.constant_references().get(&id).unwrap();

        match self.resolve_constant_internal(*constant_ref.name_id()) {
            Outcome::Retry => {
                // There might be dependencies we haven't figured out yet, so we need to retry
                unit_queue.push_back(unit_id);
            }
            Outcome::Unresolved(None) => {
                // We couldn't resolve this name. Emit a diagnostic
            }
            Outcome::Unresolved(Some(id_needing_linearization)) => {
                unit_queue.push_back(unit_id);
                unit_queue.push_back(Unit::Ancestors(id_needing_linearization));
            }
            Outcome::Resolved(declaration_id, None) => {
                self.graph.record_resolved_reference(id, declaration_id);
                *made_progress = true;
            }
            Outcome::Resolved(resolved_id, Some(id_needing_linearization)) => {
                self.graph.record_resolved_reference(id, resolved_id);
                *made_progress = true;
                unit_queue.push_back(Unit::Ancestors(id_needing_linearization));
            }
        }
    }

    /// Handles a unit of work for linearizing ancestors of a declaration
    fn handle_ancestor_unit(&mut self, unit_queue: &mut VecDeque<Unit>, made_progress: &mut bool, id: DeclarationId) {
        match self.ancestors_of(id) {
            Ancestors::Complete(_) | Ancestors::Cyclic(_) => {
                // We succeeded in some capacity this time
                *made_progress = true;
            }
            Ancestors::Partial(_) => {
                // We still couldn't linearize ancestors, but there's a chance that this will succeed next time. We
                // re-enqueue for another try, but we don't consider it as making progress
                unit_queue.push_back(Unit::Ancestors(id));
            }
        }
    }

    /// Handle other definitions that don't require resolution, but need to have their declarations and membership created
    #[allow(clippy::too_many_lines)]
    fn handle_remaining_definitions(
        &mut self,
        other_ids: Vec<crate::model::id::Id<crate::model::ids::DefinitionMarker>>,
    ) {
        for id in other_ids {
            match self.graph.definitions().get(&id).unwrap() {
                Definition::Method(method_definition) => {
                    let str_id = *method_definition.str_id();
                    let owner_id = if let Some(receiver) = method_definition.receiver() {
                        let receiver_decl_id = match self.graph.names().get(receiver).unwrap() {
                            NameRef::Resolved(resolved) => *resolved.declaration_id(),
                            NameRef::Unresolved(_) => {
                                // Error diagnostic: if we couldn't resolve the constant being used, then we don't know
                                // where this method is being defined. For example:
                                //
                                // def Foo.bar; end
                                //
                                // where Foo is undefined
                                continue;
                            }
                        };

                        self.get_or_create_singleton_class(receiver_decl_id)
                    } else {
                        self.resolve_lexical_owner(*method_definition.lexical_nesting_id())
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
                    let declaration_id = DeclarationId::from(&name);

                    self.graph.add_declaration(declaration_id, id, || {
                        Declaration::GlobalVariable(Box::new(GlobalVariableDeclaration::new(name, owner_id)))
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
                            // Method has explicit receiver (def self.foo or def Foo.bar)
                            if let Some(receiver_name_id) = method.receiver() {
                                let Some(NameRef::Resolved(resolved)) = self.graph.names().get(receiver_name_id) else {
                                    // TODO: add diagnostic for unresolved receiver
                                    continue;
                                };
                                let receiver_decl_id = *resolved.declaration_id();

                                // Instance variable in singleton method - owned by the receiver's singleton class
                                let owner_id = self.get_or_create_singleton_class(receiver_decl_id);
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
                            let owner_id = self.get_or_create_singleton_class(nesting_decl_id);
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
                            let owner_id = self.get_or_create_singleton_class(singleton_class_decl_id);
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
        let declaration_id = DeclarationId::from(&fully_qualified_name);

        self.graph.add_declaration(declaration_id, definition_id, || {
            declaration_builder(fully_qualified_name)
        });
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

    /// Gets or creates a singleton class declaration for a given class/module declaration.
    /// For class `Foo`, this returns the declaration for `Foo::<Foo>`.
    fn get_or_create_singleton_class(&mut self, attached_id: DeclarationId) -> DeclarationId {
        let (decl_id, name) = {
            let attached_decl = self.graph.declarations().get(&attached_id).unwrap();

            // TODO: the constant check is a temporary hack. We need to implement proper handling for `Struct.new`, `Class.new`
            // and `Module.new`, which now seem like constants, but are actually namespaces
            if !matches!(attached_decl, Declaration::Constant(_) | Declaration::ConstantAlias(_))
                && let Some(singleton_id) = attached_decl.as_namespace().unwrap().singleton_class()
            {
                return *singleton_id;
            }

            let name = format!("{}::<{}>", attached_decl.name(), attached_decl.unqualified_name());
            (DeclarationId::from(&name), name)
        };

        // TODO: the constant check is a temporary hack. We need to implement proper handling for `Struct.new`, `Class.new`
        // and `Module.new`, which now seem like constants, but are actually namespaces
        if !matches!(
            self.graph.declarations().get(&attached_id).unwrap(),
            Declaration::Constant(_) | Declaration::ConstantAlias(_)
        ) {
            self.graph
                .declarations_mut()
                .get_mut(&attached_id)
                .unwrap()
                .as_namespace_mut()
                .unwrap()
                .set_singleton_class_id(decl_id);
        }

        self.graph.declarations_mut().insert(
            decl_id,
            Declaration::Namespace(Namespace::SingletonClass(Box::new(SingletonClassDeclaration::new(
                name,
                attached_id,
            )))),
        );

        decl_id
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
    #[allow(clippy::too_many_lines)]
    fn linearize_ancestors(&mut self, declaration_id: DeclarationId, context: &mut LinearizationContext) -> Ancestors {
        {
            let declaration = self.graph.declarations_mut().get_mut(&declaration_id).unwrap();

            // TODO: this is a temporary hack. We need to implement proper handling for `Struct.new`, `Class.new` and
            // `Module.new`, which now seem like constants, but are actually namespaces
            if matches!(declaration, Declaration::Constant(_) | Declaration::ConstantAlias(_)) {
                return Ancestors::Complete(vec![]);
            }

            // Add this declaration to the descendants so that we capture transitive descendant relationships
            context.descendants.insert(declaration_id);

            // Return the cached ancestors if we already computed them. If they are partial ancestors, ignore the cache to try
            // again
            if declaration.as_namespace().unwrap().has_complete_ancestors() {
                let cached = declaration.as_namespace().unwrap().ancestors();
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

        // TODO: this check is against `Object` for now to avoid infinite recursion. After RBS indexing, we need to change
        // this to `BasicObject` since it's the only class that cannot have a parent
        let parent_ancestors = self.linearize_parent_ancestors(declaration_id, context);

        let declaration = self.graph.declarations().get(&declaration_id).unwrap();

        let mut mixins = Vec::new();

        // If we're linearizing a singleton class, add the extends of the attached class to the list of mixins to process
        if let Declaration::Namespace(Namespace::SingletonClass(_)) = declaration {
            let attached_decl = self.graph.declarations().get(declaration.owner_id()).unwrap();

            let attached_mixins = attached_decl
                .definitions()
                .iter()
                .filter_map(|definition_id| {
                    self.mixins_of(*definition_id)
                        .map(|mixins| mixins.into_iter().map(|mixin| (mixin, *definition_id)))
                })
                .flatten()
                .collect::<Vec<(Mixin, DefinitionId)>>();

            mixins.extend(
                attached_mixins
                    .into_iter()
                    .map(|(mixin, _)| mixin)
                    .filter(|mixin| matches!(mixin, Mixin::Extend(_))),
            );
        }

        // Consider only prepends and includes for the current declaration
        let decl_mixins = declaration
            .definitions()
            .iter()
            .filter_map(|definition_id| {
                self.mixins_of(*definition_id)
                    .map(|mixins| mixins.into_iter().map(|mixin| (mixin, *definition_id)))
            })
            .flatten()
            .collect::<Vec<(Mixin, DefinitionId)>>();

        mixins.extend(
            decl_mixins
                .into_iter()
                .map(|(mixin, _)| mixin)
                .filter(|mixin| matches!(mixin, Mixin::Prepend(_) | Mixin::Include(_))),
        );

        let (linearized_prepends, linearized_includes) =
            self.linearize_mixins(declaration_id, context, &mixins, parent_ancestors.as_ref());

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

                Some(
                    match self.linearize_parent_class(declaration_id, &definition_ids, context) {
                        Ancestors::Complete(ids) => ids,
                        Ancestors::Cyclic(ids) => {
                            context.cyclic = true;
                            ids
                        }
                        Ancestors::Partial(ids) => {
                            context.partial = true;
                            ids
                        }
                    },
                )
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
    #[allow(clippy::too_many_lines)]
    fn linearize_mixins(
        &mut self,
        declaration_id: DeclarationId,
        context: &mut LinearizationContext,
        mixins: &Vec<Mixin>,
        parent_ancestors: Option<&Vec<Ancestor>>,
    ) -> (VecDeque<Ancestor>, VecDeque<Ancestor>) {
        let mut linearized_prepends = VecDeque::new();
        let mut linearized_includes = VecDeque::new();
        let mut diagnostics = Vec::new();

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
                            let mixin_declaration = self.graph.declarations().get(resolved.declaration_id()).unwrap();

                            if mixin_declaration.kind() != DeclarationKind::Module {
                                diagnostics.push(Diagnostic::new(
                                    Rule::NonModuleMixin,
                                    constant_reference.uri_id(),
                                    constant_reference.offset().clone(),
                                    format!(
                                        "Mixin `{}` is not a module",
                                        self.graph.strings().get(resolved.name().str()).unwrap().deref()
                                    ),
                                ));
                            }

                            let ids = match self.linearize_ancestors(*resolved.declaration_id(), context) {
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
                            let mixin_declaration = self.graph.declarations().get(resolved.declaration_id()).unwrap();

                            if mixin_declaration.kind() != DeclarationKind::Module {
                                diagnostics.push(Diagnostic::new(
                                    Rule::NonModuleMixin,
                                    constant_reference.uri_id(),
                                    constant_reference.offset().clone(),
                                    format!(
                                        "Mixin `{}` is not a module",
                                        self.graph.strings().get(resolved.name().str()).unwrap().deref()
                                    ),
                                ));
                            }

                            let mut ids = match self.linearize_ancestors(*resolved.declaration_id(), context) {
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

        self.graph
            .declarations_mut()
            .get_mut(&declaration_id)
            .unwrap()
            .diagnostics_mut()
            .extend(diagnostics);

        if context.cyclic {
            for mixin in mixins {
                let constant_reference = self
                    .graph
                    .constant_references()
                    .get(mixin.constant_reference_id())
                    .unwrap();

                let diagnostic = Diagnostic::new(
                    Rule::CircularDependency,
                    constant_reference.uri_id(),
                    constant_reference.offset().clone(),
                    format!(
                        "Circular dependency: `{}` is parent of itself",
                        self.graph.declarations().get(&declaration_id).unwrap().name()
                    ),
                );

                self.graph
                    .declarations_mut()
                    .get_mut(&declaration_id)
                    .unwrap()
                    .add_diagnostic(diagnostic);
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

                {
                    let owner = self.graph.declarations().get(&owner_id).unwrap();

                    // We don't prefix declarations with `Object::`
                    if owner_id != *OBJECT_ID {
                        fully_qualified_name.insert_str(0, "::");
                        fully_qualified_name.insert_str(0, owner.name());
                    }
                }

                let declaration_id = DeclarationId::from(&fully_qualified_name);

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

                self.graph.add_declaration(declaration_id, definition_id, || {
                    declaration_builder(fully_qualified_name, owner_id)
                });
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

        if let Some(parent_scope) = name_ref.parent_scope() {
            // If we have `A::B`, the owner of `B` is whatever `A` resolves to.
            // If `A` is an alias, resolve through to get the actual namespace.
            match self.resolve_constant_internal(*parent_scope) {
                Outcome::Resolved(id, linearization) => self.resolve_to_primary_namespace(id, linearization),
                other => other,
            }
        } else if let Some(nesting_id) = name_ref.nesting() {
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
                    Outcome::Retry
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
            return Outcome::Retry;
        };

        // Check if the primary result is still an unresolved alias
        if matches!(
            self.graph.declarations().get(&primary_id),
            Some(Declaration::ConstantAlias(_))
        ) {
            return Outcome::Retry;
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
                // If there's a parent scope for this constant, it means it's a constant path. We must first resolve the
                // outer most parent, so that we can then continue resolution from there, recording the results along the
                // way
                if let Some(parent_scope_id) = name.parent_scope() {
                    if let NameRef::Resolved(parent_scope) = self.graph.names().get(parent_scope_id).unwrap() {
                        let declaration = self.graph.declarations().get(parent_scope.declaration_id()).unwrap();

                        // TODO: this is a temporary hack. We need to implement proper handling for `Struct.new`, `Class.new` and
                        // `Module.new`, which now seem like constants, but are actually namespaces
                        if matches!(declaration, Declaration::Constant(_)) {
                            return Outcome::Unresolved(None);
                        }

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
                                    return Outcome::Retry;
                                }
                                Some(Declaration::Namespace(_)) => {
                                    found_namespace = true;
                                    match self.search_ancestors(id, *name.str()) {
                                        Outcome::Resolved(declaration_id, _) => {
                                            self.graph.record_resolved_name(name_id, declaration_id);
                                            return Outcome::Resolved(declaration_id, None);
                                        }
                                        Outcome::Unresolved(Some(needs_linearization_id)) => {
                                            missing_linearization_id.get_or_insert(needs_linearization_id);
                                        }
                                        Outcome::Unresolved(None) => {}
                                        Outcome::Retry => unreachable!("search_ancestors never returns Retry"),
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
                        return missing_linearization_id.map_or(Outcome::Retry, |id| Outcome::Unresolved(Some(id)));
                    }

                    return Outcome::Retry;
                }

                // Otherwise, it's a simple constant read and we can resolve it directly
                let result = self.run_resolution(&name);

                if let Outcome::Resolved(declaration_id, _) = result {
                    self.graph.record_resolved_name(name_id, declaration_id);
                }

                result
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
                NameRef::Unresolved(_) => Outcome::Retry,
            };
            match ancestor_outcome {
                Outcome::Resolved(_, _) | Outcome::Retry => return ancestor_outcome,
                Outcome::Unresolved(Some(needs_linearization_id)) => {
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
                Outcome::Retry => {
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
            Ancestors::Partial(ids) => ids
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
                            .map(|id| Outcome::Resolved(*id, Some(declaration_id)))
                    } else {
                        None
                    }
                })
                .unwrap_or(Outcome::Unresolved(Some(declaration_id))),
        }
    }

    /// Look for the constant in the lexical scopes that are a part of its nesting
    fn search_lexical_scopes(&self, name: &Name, str_id: StringId) -> Outcome {
        let mut current_name = name;

        while let Some(nesting_id) = current_name.nesting() {
            if let NameRef::Resolved(nesting_name_ref) = self.graph.names().get(nesting_id).unwrap() {
                if let Some(declaration) = self.graph.declarations().get(nesting_name_ref.declaration_id())
                    && !matches!(declaration, Declaration::Constant(_) | Declaration::ConstantAlias(_)) // TODO: temporary hack to avoid crashing on `Struct.new`
                    && let Some(member) = declaration.as_namespace().unwrap().member(&str_id)
                {
                    return Outcome::Resolved(*member, None);
                }

                current_name = nesting_name_ref.name();
            } else {
                return Outcome::Retry;
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
        let parent_depth = name.parent_scope().map_or(0, |id| {
            let name_ref = names.get(&id).unwrap();
            Self::name_depth(name_ref, names)
        });
        let nesting_depth = name.nesting().map_or(0, |id| {
            let name_ref = names.get(&id).unwrap();
            Self::name_depth(name_ref, names)
        });

        parent_depth + nesting_depth + 1
    }

    /// Returns a tuple of 2 vectors:
    /// - The first one contains all constants, sorted in order for resolution (less complex constant names first)
    /// - The second one contains all other definitions, in no particular order
    #[must_use]
    fn sorted_units(&self) -> (VecDeque<Unit>, Vec<DefinitionId>) {
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

        let mut references = self
            .graph
            .constant_references()
            .iter()
            .map(|(id, constant_ref)| {
                let uri = self.graph.documents().get(&constant_ref.uri_id()).unwrap().uri();

                (
                    Unit::Reference(*id),
                    (names.get(constant_ref.name_id()).unwrap(), uri, constant_ref.offset()),
                )
            })
            .collect::<Vec<_>>();

        // Sort constant references based on their name complexity so that simpler names are always first
        references.sort_by(|(_, (name_a, uri_a, offset_a)), (_, (name_b, uri_b, offset_b))| {
            (Self::name_depth(name_a, names), uri_a, offset_a).cmp(&(Self::name_depth(name_b, names), uri_b, offset_b))
        });

        let mut units = definitions.into_iter().map(|(id, _)| id).collect::<VecDeque<_>>();
        units.extend(references.into_iter().map(|(id, _)| id).collect::<VecDeque<_>>());

        others.shrink_to_fit();

        (units, others)
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
                (self.get_or_create_singleton_class(inner_parent), partial)
            }
            Declaration::Namespace(Namespace::Class(_)) => {
                // For classes (the regular case), we need to return the singleton class of its parent
                let definition_ids = decl.definitions().to_vec();

                let (picked_parent, _parent_info, partial) = self.get_parent_class(attached_id, &definition_ids);
                (self.get_or_create_singleton_class(picked_parent), partial)
            }
            _ => {
                // Other declaration types (constants, methods, etc.) shouldn't reach here,
                // but default to Object's singleton parent
                (*CLASS_ID, false)
            }
        }
    }

    fn get_parent_class(
        &mut self,
        declaration_id: DeclarationId,
        definition_ids: &[DefinitionId],
    ) -> (DeclarationId, Option<(ReferenceId, DefinitionId)>, bool) {
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
                        explicit_parents.push((*resolved.declaration_id(), *superclass, *definition_id));
                    }
                    NameRef::Unresolved(_) => {
                        partial = true;
                    }
                }
            }
        }

        // Dedup explicit parents that resolve to the same declaration
        explicit_parents.dedup_by(|(name_a, _, _), (name_b, _, _)| name_a == name_b);

        for (parent_declaration_id, superclass_ref_id, definition_id) in &explicit_parents {
            let parent_declaration = self.graph.declarations().get(parent_declaration_id).unwrap();

            if parent_declaration.kind() != DeclarationKind::Class {
                let diagnostic = Diagnostic::new(
                    Rule::NonClassSuperclass,
                    *self.graph.definitions().get(definition_id).unwrap().uri_id(),
                    self.graph
                        .constant_references()
                        .get(superclass_ref_id)
                        .unwrap()
                        .offset()
                        .clone(),
                    format!(
                        "Superclass `{}` of `{}` is not a class (found `{}`)",
                        parent_declaration.name(),
                        self.graph.declarations().get(&declaration_id).unwrap().name(),
                        parent_declaration.kind()
                    ),
                );

                self.graph
                    .declarations_mut()
                    .get_mut(&declaration_id)
                    .unwrap()
                    .add_diagnostic(diagnostic);
            }
        }

        if explicit_parents.len() > 1 {
            let (first_parent_id, _first_superclass_ref_id, _first_definition_id) = explicit_parents[0];

            for (parent_declaration_id, superclass_ref_id, definition_id) in explicit_parents.iter().skip(1) {
                let diagnostic = Diagnostic::new(
                    Rule::ParentRedefinition,
                    *self.graph.definitions().get(definition_id).unwrap().uri_id(),
                    self.graph
                        .constant_references()
                        .get(superclass_ref_id)
                        .unwrap()
                        .offset()
                        .clone(),
                    format!(
                        "Parent of class `{}` redefined from `{}` to `{}`",
                        self.graph.declarations().get(&declaration_id).unwrap().name(),
                        self.graph.declarations().get(&first_parent_id).unwrap().name(),
                        self.graph.declarations().get(parent_declaration_id).unwrap().name()
                    ),
                );

                self.graph
                    .declarations_mut()
                    .get_mut(&declaration_id)
                    .unwrap()
                    .add_diagnostic(diagnostic);
            }
        }

        explicit_parents.first().map_or(
            (*OBJECT_ID, None, partial),
            |(parent_id, superclass_ref_id, definition_id)| {
                (*parent_id, Some((*superclass_ref_id, *definition_id)), partial)
            },
        )
    }

    fn linearize_parent_class(
        &mut self,
        declaration_id: DeclarationId,
        definition_ids: &[DefinitionId],
        context: &mut LinearizationContext,
    ) -> Ancestors {
        let (picked_parent, parent_info, partial) = self.get_parent_class(declaration_id, definition_ids);

        let result = self.linearize_ancestors(picked_parent, context);

        if matches!(result, Ancestors::Cyclic(_))
            && let Some((superclass_ref_id, definition_id)) = parent_info
        {
            let diagnostic = Diagnostic::new(
                Rule::CircularDependency,
                *self.graph.definitions().get(&definition_id).unwrap().uri_id(),
                self.graph
                    .constant_references()
                    .get(&superclass_ref_id)
                    .unwrap()
                    .offset()
                    .clone(),
                format!(
                    "Circular dependency: `{}` is parent of itself",
                    self.graph.declarations().get(&declaration_id).unwrap().name()
                ),
            );

            self.graph
                .declarations_mut()
                .get_mut(&declaration_id)
                .unwrap()
                .add_diagnostic(diagnostic);
        }

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
    use crate::model::ids::UriId;
    use crate::test_utils::GraphTest;

    macro_rules! assert_constant_alias_target_eq {
        ($context:expr, $alias_name:expr, $target_name:expr) => {{
            let decl_id = DeclarationId::from($alias_name);
            let target = $context
                .graph()
                .alias_targets(&decl_id)
                .and_then(|t| t.first().copied());
            assert_eq!(
                target,
                Some(DeclarationId::from($target_name)),
                "Expected alias '{}' to have primary target '{}'",
                $alias_name,
                $target_name
            );
        }};
    }

    macro_rules! assert_no_constant_alias_target {
        ($context:expr, $alias_name:expr) => {{
            let decl_id = DeclarationId::from($alias_name);
            let targets = $context.graph().alias_targets(&decl_id).unwrap_or_default();
            assert!(
                targets.is_empty(),
                "Expected no alias target for '{}', but found {:?}",
                $alias_name,
                targets
            );
        }};
    }

    macro_rules! assert_alias_targets_contain {
        ($context:expr, $alias_name:expr, $($target_name:expr),+ $(,)?) => {{
            let decl_id = DeclarationId::from($alias_name);
            let targets = $context.graph().alias_targets(&decl_id).unwrap_or_default();
            $(
                let expected_id = DeclarationId::from($target_name);
                assert!(
                    targets.contains(&expected_id),
                    "Expected alias '{}' to contain target '{}', but targets were {:?}",
                    $alias_name,
                    $target_name,
                    targets
                );
            )+
        }};
    }

    /// Asserts that a declaration has a constant reference at the specified location
    ///
    /// This macro:
    /// 1. Parses the location string into `(uri, start_offset, end_offset)`
    /// 2. Finds the declaration by name
    /// 3. Finds a constant reference to that declaration at the given uri and start offset
    /// 4. Asserts the end offset matches
    ///
    /// Location format: "uri:start_line:start_column-end_line:end_column"
    /// Example: `<file:///foo.rb:3:0-3:5>`
    macro_rules! assert_constant_reference_to {
        ($context:expr, $declaration_name:expr, $location:expr) => {
            let (uri, start, end) = $context.parse_location($location);

            let declaration = $context
                .graph()
                .declarations()
                .get(&DeclarationId::from($declaration_name))
                .expect(&format!("Declaration '{}' not found in graph", $declaration_name));

            let constant = declaration
                .references()
                .iter()
                .filter_map(|r| {
                    let reference = $context
                        .graph()
                        .constant_references()
                        .get(r)
                        .expect("Reference should exist");
                    if let NameRef::Resolved(_) = $context
                        .graph()
                        .names()
                        .get(reference.name_id())
                        .expect("Name should exist")
                    {
                        Some(reference)
                    } else {
                        None
                    }
                })
                .find(|c| c.uri_id() == UriId::from(&uri) && c.offset().start() == start)
                .expect(&format!(
                    "Declaration '{}' does not have a reference at {} starting at offset {}",
                    $declaration_name, $location, start
                ));

            $context.assert_offset_matches(
                &uri,
                constant.offset(),
                start,
                end,
                &format!("reference to '{}'", $declaration_name),
                $location,
            );
        };
    }

    macro_rules! assert_ancestors_eq {
        ($context:expr, $name:expr, $expected:expr) => {
            let declaration = $context
                .graph()
                .declarations()
                .get(&DeclarationId::from($name))
                .unwrap();

            match declaration.as_namespace().unwrap().ancestors() {
                Ancestors::Cyclic(ancestors) | Ancestors::Complete(ancestors) => {
                    assert_eq!(
                        $expected
                            .iter()
                            .map(|n| Ancestor::Complete(DeclarationId::from(*n)))
                            .collect::<Vec<_>>(),
                        ancestors,
                        "Incorrect ancestors {}",
                        ancestors
                            .iter()
                            .filter_map(|id| {
                                if let Ancestor::Complete(id) = id {
                                    let name = {
                                        $context
                                            .graph()
                                            .declarations()
                                            .get(id)
                                            .unwrap()
                                            .name()
                                            .to_string()
                                    };
                                    Some(name)
                                } else {
                                    None
                                }
                            })
                            .collect::<Vec<_>>()
                            .join(", ")
                    );
                }
                Ancestors::Partial(_) => {
                    panic!("Expected ancestors to be resolved for {}", declaration.name());
                }
            }
        };
    }

    macro_rules! assert_descendants {
        ($context:expr, $parent:expr, $descendants:expr) => {
            let parent = $context
                .graph()
                .declarations()
                .get(&DeclarationId::from($parent))
                .unwrap();
            let actual = match parent {
                Declaration::Namespace(Namespace::Class(class)) => {
                    class.descendants().iter().cloned().collect::<Vec<_>>()
                }
                Declaration::Namespace(Namespace::Module(module)) => {
                    module.descendants().iter().cloned().collect::<Vec<_>>()
                }
                Declaration::Namespace(Namespace::SingletonClass(singleton)) => {
                    singleton.descendants().iter().cloned().collect::<Vec<_>>()
                }
                _ => panic!("Tried to get descendants for a declaration that isn't a namespace"),
            };

            for descendant in &$descendants {
                let descendant_id = DeclarationId::from(*descendant);

                assert!(
                    actual.contains(&descendant_id),
                    "Expected '{}' to be a descendant of '{}'",
                    $context
                        .graph()
                        .declarations()
                        .get(&descendant_id)
                        .unwrap()
                        .name(),
                    parent.name()
                );
            }
        };
    }

    macro_rules! assert_members_eq {
        ($context:expr, $declaration_id:expr, $expected_members:expr) => {
            let mut actual_members = $context
                .graph()
                .declarations()
                .get(&DeclarationId::from($declaration_id))
                .unwrap()
                .as_namespace()
                .unwrap()
                .members()
                .iter()
                .map(|(str_id, _)| $context.graph().strings().get(str_id).unwrap().as_str())
                .collect::<Vec<_>>();

            actual_members.sort();

            assert_eq!($expected_members, actual_members);
        };
    }

    macro_rules! assert_no_members {
        ($context:expr, $declaration_id:expr) => {
            assert_members_eq!($context, $declaration_id, vec![] as Vec<&str>);
        };
    }

    macro_rules! assert_owner_eq {
        ($context:expr, $declaration_id:expr, $expected_owner_name:expr) => {
            let actual_owner_id = $context
                .graph()
                .declarations()
                .get(&DeclarationId::from($declaration_id))
                .unwrap()
                .owner_id();

            let actual_owner_name = $context
                .graph()
                .declarations()
                .get(actual_owner_id)
                .unwrap()
                .name();

            assert_eq!($expected_owner_name, actual_owner_name);
        };
    }

    macro_rules! assert_singleton_class_eq {
        ($context:expr, $declaration_id:expr, $expected_singleton_class_name:expr) => {
            let declaration = $context
                .graph()
                .declarations()
                .get(&DeclarationId::from($declaration_id))
                .unwrap();

            assert_eq!(
                $expected_singleton_class_name,
                $context
                    .graph()
                    .declarations()
                    .get(declaration.as_namespace().unwrap().singleton_class().unwrap())
                    .unwrap()
                    .name()
            );
        };
    }

    macro_rules! assert_instance_variables_eq {
        ($context:expr, $declaration_id:expr, $expected_instance_variables:expr) => {
            let mut actual_instance_variables = $context
                .graph()
                .declarations()
                .get(&DeclarationId::from($declaration_id))
                .unwrap()
                .as_namespace()
                .unwrap()
                .members()
                .iter()
                .filter_map(
                    |(str_id, member_id)| match $context.graph().declarations().get(member_id) {
                        Some(Declaration::InstanceVariable(_)) => {
                            Some($context.graph().strings().get(str_id).unwrap().as_str())
                        }
                        _ => None,
                    },
                )
                .collect::<Vec<_>>();

            actual_instance_variables.sort();

            assert_eq!($expected_instance_variables, actual_instance_variables);
        };
    }

    fn format_diagnostics(context: &GraphTest, ignore_rules: &[Rule]) -> Vec<String> {
        let mut diagnostics: Vec<_> = context
            .graph()
            .all_diagnostics()
            .into_iter()
            .filter(|d| !ignore_rules.contains(d.rule()))
            .collect();

        diagnostics.sort_by_key(|d| {
            let uri = context.graph().documents().get(d.uri_id()).unwrap().uri();
            (uri, d.offset())
        });

        diagnostics
            .iter()
            .map(|d| {
                let document = context.graph().documents().get(d.uri_id()).unwrap();
                d.formatted(document)
            })
            .collect()
    }

    macro_rules! assert_diagnostics_eq {
        ($context:expr, $expected_diagnostics:expr) => {{
            assert_eq!($expected_diagnostics, format_diagnostics($context, &[]));
        }};
        ($context:expr, $expected_diagnostics:expr, $ignore_rules:expr) => {{
            assert_eq!($expected_diagnostics, format_diagnostics($context, $ignore_rules));
        }};
    }

    macro_rules! assert_no_diagnostics {
        ($context:expr) => {{
            let diagnostics = format_diagnostics($context, &[]);
            assert!(diagnostics.is_empty(), "expected no diagnostics, got {:?}", diagnostics);
        }};
        ($context:expr, $ignore_rules:expr) => {{
            let diagnostics = format_diagnostics($context, $ignore_rules);
            assert!(diagnostics.is_empty(), "expected no diagnostics, got {:?}", diagnostics);
        }};
    }

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

        assert_constant_reference_to!(context, "Bar", "file:///bar.rb:2:2-2:5");
        assert_constant_reference_to!(context, "Bar", "file:///bar.rb:3:0-3:3");
        assert_constant_reference_to!(context, "Bar", "file:///foo.rb:1:4-1:7");
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

        assert_constant_reference_to!(context, "Foo::CONST", "file:///bar.rb:4:4-4:9");
        assert_constant_reference_to!(context, "Foo::CONST", "file:///bar.rb:5:9-5:14");
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

        assert_constant_reference_to!(context, "Baz", "file:///bar.rb:4:4-4:7");
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

        assert_constant_reference_to!(context, "Foo::Bar", "file:///bar.rb:4:5-4:8");
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

        assert_members_eq!(context, "Foo", vec!["Bar", "Baz"]);
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

        assert_members_eq!(context, "Foo", vec!["@name", "initialize()"]);
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

        assert_members_eq!(context, "Bar", vec!["Baz"]);
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

        assert_members_eq!(context, "Bar", vec!["Baz"]);
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
        assert!(
            context
                .graph()
                .declarations()
                .get(&DeclarationId::from("Foo"))
                .is_none()
        );
        assert!(
            context
                .graph()
                .declarations()
                .get(&DeclarationId::from("Foo::Bar"))
                .is_none()
        );
        assert!(
            context
                .graph()
                .declarations()
                .get(&DeclarationId::from("Foo::Bar::Baz"))
                .is_none()
        );

        assert_no_diagnostics!(&context);
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
            vec![
                "Top", "Foo", "Qux", "AfterTop", "Bar", "Baz", "Zip", "Zap", "Zop", "Boop"
            ],
            names
                .iter()
                .map(|n| context.graph().strings().get(n.str()).unwrap().as_str())
                .collect::<Vec<_>>()
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

        assert_members_eq!(context, "Foo::<Foo>", vec!["BAZ", "bar()"]);
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

        assert_members_eq!(context, "Foo::<Foo>::<<Foo>>", vec!["baz()"]);
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

        assert_members_eq!(context, "Foo::<Foo>", vec!["Baz", "baz()"]);
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

        assert_members_eq!(context, "Foo", vec!["@@bar", "@@baz"]);
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

        assert_members_eq!(context, "Foo", vec!["@@baz", "bar()"]);
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
        assert_members_eq!(context, "Bar", vec!["@@cvar1", "@@cvar2"]);
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
        assert!(
            context
                .graph()
                .declarations()
                .get(&DeclarationId::from("Object::@@var"))
                .is_none()
        );
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

        assert!(
            context
                .graph()
                .declarations()
                .get(&DeclarationId::from("Foo::<Foo>"))
                .is_some()
        );

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

        assert_members_eq!(context, "Foo::<Foo>", vec!["bar()", "baz()"]);
        assert_owner_eq!(context, "Foo::<Foo>", "Foo");

        assert_members_eq!(context, "Foo::<Foo>::<<Foo>>", vec!["nested_bar()"]);
        assert_owner_eq!(context, "Foo::<Foo>::<<Foo>>", "Foo::<Foo>");

        assert_members_eq!(context, "Bar::<Bar>", vec!["qux()"]);
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

        assert_diagnostics_eq!(
            &context,
            vec![
                "circular-dependency: Circular dependency: `Foo` is parent of itself (1:13-1:16)",
                "circular-dependency: Circular dependency: `Bar` is parent of itself (2:13-2:16)",
                "circular-dependency: Circular dependency: `Baz` is parent of itself (3:13-3:16)"
            ]
        );

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

        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:5:2-5:7");
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
            declaration.as_namespace().unwrap().ancestors(),
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

        assert_constant_reference_to!(context, "Foo::Bar::Baz::CONST", "file:///foo.rb:8:2-8:7");
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

        assert_instance_variables_eq!(context, "Foo", vec!["@bar"]);
        // @qux in `class << self; def qux` - self is Foo when called, so @qux belongs to Foo's singleton class
        assert_instance_variables_eq!(context, "Foo::<Foo>", vec!["@baz", "@foo", "@qux"]);
        assert_instance_variables_eq!(context, "Foo::<Foo>::<<Foo>>", vec!["@nested"]);
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

        assert_instance_variables_eq!(context, "Foo::<Foo>", vec!["@foo"]);
        assert_instance_variables_eq!(context, "Bar::<Bar>", vec!["@baz"]);
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
        assert_instance_variables_eq!(context, "Bar::Baz::<Baz>", vec!["@baz"]);
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

        assert_instance_variables_eq!(context, "Foo::<Foo>::<<Foo>>", vec!["@bar"]);
        assert_instance_variables_eq!(context, "Foo::<Foo>::<<Foo>>::<<<Foo>>>", vec!["@baz"]);
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
        let foo_decl = context.graph().declarations().get(&DeclarationId::from("Object::@foo"));
        assert!(foo_decl.is_none(), "Object::@foo declaration should not exist");
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
            vec!["dynamic-singleton-definition: Dynamic receiver for singleton method definition (2:3-4:6)",]
        );

        // Instance variable in method with unresolved receiver should not create a declaration
        let baz_decl = context.graph().declarations().get(&DeclarationId::from("Object::@baz"));
        assert!(baz_decl.is_none(), "@baz declaration should not exist");

        let foo_baz_decl = context.graph().declarations().get(&DeclarationId::from("Foo::@baz"));
        assert!(foo_baz_decl.is_none(), "Foo::@baz declaration should not exist");
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

        assert_members_eq!(context, "Foo", vec!["bar()", "foo()"]);
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

        assert_members_eq!(context, "Object", vec!["$bar", "$foo"]);
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

        assert_diagnostics_eq!(
            &context,
            vec!["circular-dependency: Circular dependency: `Foo` is parent of itself (2:11-2:14)"]
        );

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

        assert_members_eq!(context, "Foo::Bar", vec!["Qux"]);
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

        assert_diagnostics_eq!(
            &context,
            vec!["circular-dependency: Circular dependency: `Foo` is parent of itself (2:11-2:14)"]
        );

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

        assert_members_eq!(context, "Foo::Bar", vec!["Qux"]);
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

        assert_constant_reference_to!(context, "Foo::Bar", "file:///foo.rb:8:5-8:8");
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
        assert_members_eq!(context, "Object", vec!["$bar", "Foo"]);
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
        assert_members_eq!(context, "Foo", vec!["inner_method()", "setup()"]);
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

        assert_members_eq!(context, "Foo::<Foo>", vec!["setup()"]);

        // All attr_* should be owned by Foo, not by setup
        assert_members_eq!(
            context,
            "Foo",
            vec!["accessor_attr()", "reader_attr()", "writer_attr()"]
        );
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
        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:5:7-5:12");
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
        assert_constant_reference_to!(context, "Foo::Bar::CONST", "file:///foo.rb:7:7-7:12");
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
        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:6:9-6:14");
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

        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:8:7-8:12");
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
        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:6:8-6:13");
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
        assert!(
            !context
                .graph()
                .declarations()
                .contains_key(&DeclarationId::from("VALUE::NOPE"))
        );
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
        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:4:7-4:12");
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

        assert!(
            context
                .graph()
                .declarations()
                .contains_key(&DeclarationId::from("A::X"))
        );
        assert!(
            context
                .graph()
                .declarations()
                .contains_key(&DeclarationId::from("B::Y"))
        );

        // SOMETHING can't be created because the circular alias can't resolve to a namespace
        assert!(
            !context
                .graph()
                .declarations()
                .contains_key(&DeclarationId::from("A::X::SOMETHING"))
        );
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
        assert_constant_reference_to!(context, "Right::Deep", "file:///foo.rb:15:17-15:21");
        assert_constant_reference_to!(context, "Right::Deep::VALUE", "file:///foo.rb:15:23-15:28");
        // Left::RIGHT_REF::LEFT_REF::Deep::VALUE
        assert_constant_reference_to!(context, "Left::Deep", "file:///foo.rb:16:27-16:31");
        assert_constant_reference_to!(context, "Left::Deep::VALUE", "file:///foo.rb:16:33-16:38");
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

        let m_thing_const = context
            .graph()
            .declarations()
            .get(&DeclarationId::from("M::Thing::CONST"))
            .unwrap();
        let m_thing = context
            .graph()
            .declarations()
            .get(&DeclarationId::from("M::Thing"))
            .unwrap();

        // All 3 paths resolve to M::Thing::CONST
        assert_eq!(m_thing_const.references().len(), 3);
        assert_eq!(m_thing.references().len(), 3);

        // M::SELF_REF::Thing::CONST
        assert_constant_reference_to!(context, "M::Thing", "file:///foo.rb:8:13-8:18");
        assert_constant_reference_to!(context, "M::Thing::CONST", "file:///foo.rb:8:20-8:25");
        // M::SELF_REF::SELF_REF::Thing::CONST
        assert_constant_reference_to!(context, "M::Thing", "file:///foo.rb:9:23-9:28");
        assert_constant_reference_to!(context, "M::Thing::CONST", "file:///foo.rb:9:30-9:35");
        // M::SELF_REF::SELF_REF::SELF_REF::Thing::CONST
        assert_constant_reference_to!(context, "M::Thing", "file:///foo.rb:10:33-10:38");
        assert_constant_reference_to!(context, "M::Thing::CONST", "file:///foo.rb:10:40-10:45");
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
        assert_diagnostics_eq!(
            &context,
            vec![
                "kind-redefinition: Redefining `Outer::NESTED` as `class`, previously defined as `constant alias` (9:1-11:4)"
            ],
            &[Rule::ParseWarning]
        );

        assert_constant_alias_target_eq!(context, "ALIAS", "Outer");
        assert_constant_alias_target_eq!(context, "Outer::NESTED", "Outer::Inner");

        // ADDED_CONST should be in Outer::Inner (the resolved target)
        assert!(
            context
                .graph()
                .declarations()
                .contains_key(&DeclarationId::from("Outer::Inner::ADDED_CONST"))
        );
        assert!(
            context
                .graph()
                .declarations()
                .contains_key(&DeclarationId::from("Outer::Inner::ADDED_CONST"))
        );

        let added_const = context
            .graph()
            .declarations()
            .get(&DeclarationId::from("Outer::Inner::ADDED_CONST"))
            .unwrap();
        assert_eq!(added_const.references().len(), 1);
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
        assert!(
            context
                .graph()
                .declarations()
                .contains_key(&DeclarationId::from("Outer::NewClass"))
        );
        assert!(
            context
                .graph()
                .declarations()
                .contains_key(&DeclarationId::from("Outer::NewClass::CLASS_CONST"))
        );

        // Outer::NewClass::CLASS_CONST
        assert_constant_reference_to!(context, "Outer::NewClass", "file:///foo.rb:10:7-10:15");
        assert_constant_reference_to!(context, "Outer::NewClass::CLASS_CONST", "file:///foo.rb:10:17-10:28");
        // ALIAS::NewClass::CLASS_CONST
        assert_constant_reference_to!(context, "Outer::NewClass", "file:///foo.rb:11:7-11:15");
        assert_constant_reference_to!(context, "Outer::NewClass::CLASS_CONST", "file:///foo.rb:11:17-11:28");
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
        assert_eq!(
            context
                .graph()
                .declarations()
                .get(&DeclarationId::from("FOO"))
                .unwrap()
                .definitions()
                .len(),
            2
        );

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

        // FOO::CONST_A should resolve to A::CONST_A
        assert_constant_reference_to!(context, "A::CONST_A", "file:///usage.rb:0:5-0:12");
        // FOO::CONST_B should resolve to B::CONST_B
        assert_constant_reference_to!(context, "B::CONST_B", "file:///usage.rb:1:5-1:12");
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
        assert_constant_reference_to!(context, "A::CONST", "file:///usage.rb:0:7-0:12");
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

        assert_constant_reference_to!(context, "Foo::CONST", "file:///usage.rb:0:8-0:13");
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

        assert_constant_reference_to!(context, "Foo::CONST", "file:///usage.rb:0:7-0:12");
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

        assert_members_eq!(context, "Foo", vec!["CONST"]);
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
        assert_members_eq!(context, "Foo", vec!["Array", "Array()"]);

        // Both declarations exist with unique IDs
        assert!(
            context
                .graph()
                .declarations()
                .get(&DeclarationId::from("Foo::Array"))
                .is_some()
        );
        assert!(
            context
                .graph()
                .declarations()
                .get(&DeclarationId::from("Foo#Array()"))
                .is_some()
        );
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

        let declarations = context.graph().declarations();

        // In the same order of appearence
        assert!(declarations.contains_key(&DeclarationId::from("Foo")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar::CONST")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar::<Bar>#@class_ivar")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar#baz()")));
        // TODO: needs the fix for attributes
        // assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar#qux=()")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar#zip()")));
        // TODO: needs the fix for attributes
        // assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar#zip=()")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar#instance_m()")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar#@@class_var")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar::<Bar>#singleton_m()")));
        assert!(declarations.contains_key(&DeclarationId::from("$global_var")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::<Foo>#another_singleton_m()")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar::<Bar>::OTHER_CONST")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar::<Bar>::<<Bar>>#@other_class_ivar")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar#@@other_class_var")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar::<Bar>#other_instance_m()")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar::<Bar>#@my_class_var")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar::<Bar>::<<Bar>>#other_singleton_m()")));
        assert!(declarations.contains_key(&DeclarationId::from("$other_global_var")));
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
        assert_constant_reference_to!(context, "Bar::Foo", "file:///foo.rb:3:2-3:5");

        assert_ancestors_eq!(context, "Foo", &["Foo"]);
        assert_ancestors_eq!(context, "Bar::Foo", &["Bar::Foo"]);

        assert_no_members!(context, "Foo");
        assert_members_eq!(context, "Bar::Foo", vec!["FOO"]);
    }

    // Diagnostics tests

    #[test]
    fn resolution_diagnostics_for_kind_redefinition() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            class Foo; end

            class Bar; end
            module Bar; end

            class Baz; end
            Baz = 123

            module Qux; end
            module Qux; end

            def foo; end
            attr_reader :foo

            class Qaz
              class Array; end
              def Array; end
            end
            "
        });

        context.resolve();

        assert_diagnostics_eq!(
            &context,
            vec![
                "kind-redefinition: Redefining `Foo` as `class`, previously defined as `module` (2:1-2:15)",
                "kind-redefinition: Redefining `Bar` as `module`, previously defined as `class` (5:1-5:16)",
                "kind-redefinition: Redefining `Baz` as `constant`, previously defined as `class` (8:1-8:4)",
            ]
        );
    }

    #[test]
    fn resolution_diagnostics_for_parent_redefinition() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Parent1; end
            class Parent2; end

            class Child1; end
            class Child1; end
            class Child1 < Object; end
            class Child1 < ::Object; end

            class Child2; end
            class Child2 < Parent1; end
            class ::Child2 < ::Parent1; end

            class Child3; end
            class Child3 < Parent1; end
            class Child3 < Parent2; end
            "
        });

        context.resolve();

        assert_diagnostics_eq!(
            &context,
            vec!["parent-redefinition: Parent of class `Child3` redefined from `Parent1` to `Parent2` (15:16-15:23)",]
        );
    }

    #[test]
    fn resolution_diagnostics_for_non_class_superclass() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Parent1; end
            Parent2 = 42

            class Child1 < Parent1; end
            class Child2 < Parent2; end
            "
        });

        context.resolve();

        assert_diagnostics_eq!(
            &context,
            vec![
                "non-class-superclass: Superclass `Parent1` of `Child1` is not a class (found `module`) (4:16-4:23)",
                "non-class-superclass: Superclass `Parent2` of `Child2` is not a class (found `constant`) (5:16-5:23)",
            ]
        );
    }

    #[test]
    fn resolution_diagnostics_for_non_module_mixin() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Mixin; end

            class Child
              include Mixin;
              prepend Mixin;
              extend Mixin;
            end
            "
        });

        context.resolve();

        assert_diagnostics_eq!(
            &context,
            vec![
                "non-module-mixin: Mixin `Mixin` is not a module (4:11-4:16)",
                "non-module-mixin: Mixin `Mixin` is not a module (5:11-5:16)",
                // "non-module-mixin: Mixin `Mixin` is not a module (6:10-6:15)", FIXME: we should report this once we linearize the ancestors of singleton classes
            ]
        );
    }
}
