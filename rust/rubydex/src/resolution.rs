use std::{
    collections::{HashSet, VecDeque},
    hash::BuildHasher,
};

use crate::model::{
    declaration::{Ancestor, Ancestors, Declaration, NamespaceDeclaration, SimpleDeclaration},
    definitions::{Definition, Mixin},
    graph::{CLASS_ID, Graph, MODULE_ID, OBJECT_ID},
    identity_maps::{IdentityHashMap, IdentityHashSet},
    ids::{DeclarationId, DefinitionId, NameId, ReferenceId, StringId},
    name::{Name, NameRef},
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
pub fn resolve_all(graph: &mut Graph) {
    // TODO: temporary code while we don't have synchronization. We clear all declarations instead of doing the minimal
    // amount of work
    graph.clear_declarations();
    // Ensure that Object exists ahead of time so that we can associate top level declarations with the right membership

    {
        let mut write_lock = graph.declarations().write().unwrap();
        write_lock.insert(
            *OBJECT_ID,
            Declaration::Class(Box::new(NamespaceDeclaration::new("Object".to_string(), *OBJECT_ID))),
        );
        write_lock.insert(
            *MODULE_ID,
            Declaration::Class(Box::new(NamespaceDeclaration::new("Module".to_string(), *OBJECT_ID))),
        );
        write_lock.insert(
            *CLASS_ID,
            Declaration::Class(Box::new(NamespaceDeclaration::new("Class".to_string(), *OBJECT_ID))),
        );
    }

    let (mut unit_queue, other_ids) = sorted_units(graph);

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
                    handle_definition_unit(graph, &mut unit_queue, &mut made_progress, unit_id, id);
                }
                Unit::Reference(id) => {
                    handle_reference_unit(graph, &mut unit_queue, &mut made_progress, unit_id, id);
                }
                Unit::Ancestors(id) => {
                    handle_ancestor_unit(graph, &mut unit_queue, &mut made_progress, id);
                }
            }
        }

        if !made_progress || unit_queue.is_empty() {
            break;
        }
    }

    handle_remaining_definitions(graph, other_ids);
}

/// Handles a unit of work for resolving a constant definition
fn handle_definition_unit(
    graph: &mut Graph,
    unit_queue: &mut VecDeque<Unit>,
    made_progress: &mut bool,
    unit_id: Unit,
    id: DefinitionId,
) {
    let outcome = match graph.definitions().get(&id).unwrap() {
        Definition::Class(class) => {
            handle_constant_declaration(graph, *class.name_id(), id, false, |name, owner_id| {
                Declaration::Class(Box::new(NamespaceDeclaration::new(name, owner_id)))
            })
        }
        Definition::Module(module) => {
            handle_constant_declaration(graph, *module.name_id(), id, false, |name, owner_id| {
                Declaration::Module(Box::new(NamespaceDeclaration::new(name, owner_id)))
            })
        }
        Definition::Constant(constant) => {
            handle_constant_declaration(graph, *constant.name_id(), id, false, |name, owner_id| {
                Declaration::Constant(Box::new(SimpleDeclaration::new(name, owner_id)))
            })
        }
        Definition::SingletonClass(singleton) => {
            handle_constant_declaration(graph, *singleton.name_id(), id, true, |name, owner_id| {
                Declaration::SingletonClass(Box::new(NamespaceDeclaration::new(name, owner_id)))
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
        Outcome::Resolved(_, None) => *made_progress = true,
        Outcome::Resolved(_, Some(id_needing_linearization)) => {
            unit_queue.push_back(Unit::Ancestors(id_needing_linearization));
            *made_progress = true;
        }
    }
}

/// Handles a unit of work for resolving a constant reference
fn handle_reference_unit(
    graph: &mut Graph,
    unit_queue: &mut VecDeque<Unit>,
    made_progress: &mut bool,
    unit_id: Unit,
    id: ReferenceId,
) {
    let constant_ref = graph.constant_references().get(&id).unwrap();

    match resolve_constant(graph, *constant_ref.name_id()) {
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
            graph.record_resolved_reference(id, declaration_id);
            *made_progress = true;
        }
        Outcome::Resolved(resolved_id, Some(id_needing_linearization)) => {
            graph.record_resolved_reference(id, resolved_id);
            *made_progress = true;
            unit_queue.push_back(Unit::Ancestors(id_needing_linearization));
        }
    }
}

/// Handles a unit of work for linearizing ancestors of a declaration
fn handle_ancestor_unit(
    graph: &mut Graph,
    unit_queue: &mut VecDeque<Unit>,
    made_progress: &mut bool,
    id: DeclarationId,
) {
    match ancestors_of(graph, id) {
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
    graph: &mut Graph,
    other_ids: Vec<crate::model::id::Id<crate::model::ids::DefinitionMarker>>,
) {
    for id in other_ids {
        match graph.definitions().get(&id).unwrap() {
            Definition::Method(method_definition) => {
                let str_id = *method_definition.str_id();
                let owner_id = if let Some(receiver) = method_definition.receiver() {
                    let receiver_decl_id = match graph.names().get(receiver).unwrap() {
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

                    get_or_create_singleton_class(graph, receiver_decl_id)
                } else {
                    resolve_lexical_owner(graph, *method_definition.lexical_nesting_id())
                };

                create_declaration(graph, str_id, id, owner_id, |name| {
                    Declaration::Method(Box::new(SimpleDeclaration::new(name, owner_id)))
                });
            }
            Definition::AttrAccessor(attr) => {
                let owner_id = resolve_lexical_owner(graph, *attr.lexical_nesting_id());

                create_declaration(graph, *attr.str_id(), id, owner_id, |name| {
                    Declaration::Method(Box::new(SimpleDeclaration::new(name, owner_id)))
                });
            }
            Definition::AttrReader(attr) => {
                let owner_id = resolve_lexical_owner(graph, *attr.lexical_nesting_id());

                create_declaration(graph, *attr.str_id(), id, owner_id, |name| {
                    Declaration::Method(Box::new(SimpleDeclaration::new(name, owner_id)))
                });
            }
            Definition::AttrWriter(attr) => {
                let owner_id = resolve_lexical_owner(graph, *attr.lexical_nesting_id());

                create_declaration(graph, *attr.str_id(), id, owner_id, |name| {
                    Declaration::Method(Box::new(SimpleDeclaration::new(name, owner_id)))
                });
            }
            Definition::GlobalVariable(var) => {
                let owner_id = *OBJECT_ID;
                create_declaration(graph, *var.str_id(), id, owner_id, |name| {
                    Declaration::GlobalVariable(Box::new(SimpleDeclaration::new(name, owner_id)))
                });
            }
            Definition::InstanceVariable(var) => {
                let str_id = *var.str_id();

                // Top-level instance variables belong to the `<main>` object, not `Object`.
                // We can't represent `<main>` yet, so skip creating declarations for these.
                // TODO: Make sure we introduce `<main>` representation later and update this
                let Some(nesting_id) = *var.lexical_nesting_id() else {
                    continue;
                };

                let Some(nesting_def) = graph.definitions().get(&nesting_id) else {
                    continue;
                };

                match nesting_def {
                    // When the instance variable is inside a method body, we determine the owner based on the method's receiver
                    Definition::Method(method) => {
                        // Method has explicit receiver (def self.foo or def Foo.bar)
                        if let Some(receiver_name_id) = method.receiver() {
                            let Some(NameRef::Resolved(resolved)) = graph.names().get(receiver_name_id) else {
                                // TODO: add diagnostic for unresolved receiver
                                continue;
                            };
                            let receiver_decl_id = *resolved.declaration_id();

                            // Instance variable in singleton method - owned by the receiver's singleton class
                            let owner_id = get_or_create_singleton_class(graph, receiver_decl_id);
                            {
                                let declarations = graph.declarations().read().unwrap();
                                debug_assert!(
                                    matches!(declarations.get(&owner_id), Some(Declaration::SingletonClass(_))),
                                    "Instance variable in singleton method should be owned by a SingletonClass"
                                );
                            }
                            create_declaration(graph, str_id, id, owner_id, |name| {
                                Declaration::InstanceVariable(Box::new(SimpleDeclaration::new(name, owner_id)))
                            });
                            continue;
                        }

                        // If the method has no explicit receiver, we resolve the owner based on the lexical nesting
                        let method_owner_id = resolve_lexical_owner(graph, *method.lexical_nesting_id());

                        // If the method is in a singleton class, the instance variable belongs to the class object
                        // Like `class << Foo; def bar; @bar = 1; end; end`, where `@bar` is owned by `Foo::<Foo>`
                        let declarations = graph.declarations().read().unwrap();
                        if let Some(decl) = declarations.get(&method_owner_id)
                            && matches!(decl, Declaration::SingletonClass(_))
                        {
                            drop(declarations);
                            // Method in singleton class - owner is the singleton class itself
                            create_declaration(graph, str_id, id, method_owner_id, |name| {
                                Declaration::InstanceVariable(Box::new(SimpleDeclaration::new(name, method_owner_id)))
                            });
                        } else {
                            drop(declarations);
                            // Regular instance method
                            // Create an instance variable declaration for the method's owner
                            create_declaration(graph, str_id, id, method_owner_id, |name| {
                                Declaration::InstanceVariable(Box::new(SimpleDeclaration::new(name, method_owner_id)))
                            });
                        }
                    }
                    // If the instance variable is directly in a class/module body, it belongs to the class object
                    // and is owned by the singleton class of that class/module
                    Definition::Class(_) | Definition::Module(_) => {
                        let nesting_decl_id = graph
                            .definitions_to_declarations()
                            .get(&nesting_id)
                            .copied()
                            .unwrap_or(*OBJECT_ID);
                        let owner_id = get_or_create_singleton_class(graph, nesting_decl_id);
                        {
                            let declarations = graph.declarations().read().unwrap();
                            debug_assert!(
                                matches!(declarations.get(&owner_id), Some(Declaration::SingletonClass(_))),
                                "Instance variable in class/module body should be owned by a SingletonClass"
                            );
                        }
                        create_declaration(graph, str_id, id, owner_id, |name| {
                            Declaration::InstanceVariable(Box::new(SimpleDeclaration::new(name, owner_id)))
                        });
                    }
                    // If in a singleton class body directly, the owner is the singleton class's singleton class
                    // Like `class << Foo; @bar = 1; end`, where `@bar` is owned by `Foo::<Foo>::<<Foo>>`
                    Definition::SingletonClass(_) => {
                        let singleton_class_decl_id = graph
                            .definitions_to_declarations()
                            .get(&nesting_id)
                            .copied()
                            .unwrap_or(*OBJECT_ID);
                        let owner_id = get_or_create_singleton_class(graph, singleton_class_decl_id);
                        {
                            let declarations = graph.declarations().read().unwrap();
                            debug_assert!(
                                matches!(declarations.get(&owner_id), Some(Declaration::SingletonClass(_))),
                                "Instance variable in singleton class body should be owned by a SingletonClass"
                            );
                        }
                        create_declaration(graph, str_id, id, owner_id, |name| {
                            Declaration::InstanceVariable(Box::new(SimpleDeclaration::new(name, owner_id)))
                        });
                    }
                    _ => {
                        panic!("Unexpected lexical nesting for instance variable: {nesting_def:?}");
                    }
                }
            }
            Definition::ClassVariable(var) => {
                // TODO: add diagnostic on the else branch. Defining class variables at the top level crashes
                if let Some(owner_id) = resolve_class_variable_owner(graph, *var.lexical_nesting_id()) {
                    create_declaration(graph, *var.str_id(), id, owner_id, |name| {
                        Declaration::ClassVariable(Box::new(SimpleDeclaration::new(name, owner_id)))
                    });
                }
            }
            Definition::Class(_) | Definition::SingletonClass(_) | Definition::Module(_) | Definition::Constant(_) => {
                panic!("Unexpected definition type in non-constant resolution. This shouldn't happen")
            }
            Definition::MethodAlias(alias) => {
                let owner_id = resolve_lexical_owner(graph, *alias.lexical_nesting_id());

                create_declaration(graph, *alias.new_name_str_id(), id, owner_id, |name| {
                    Declaration::Method(Box::new(SimpleDeclaration::new(name, owner_id)))
                });
            }
            Definition::GlobalVariableAlias(alias) => {
                create_declaration(graph, *alias.new_name_str_id(), id, *OBJECT_ID, |name| {
                    Declaration::GlobalVariable(Box::new(SimpleDeclaration::new(name, *OBJECT_ID)))
                });
            }
        }
    }
}

fn create_declaration<F>(
    graph: &mut Graph,
    str_id: StringId,
    definition_id: DefinitionId,
    owner_id: DeclarationId,
    declaration_builder: F,
) where
    F: FnOnce(String) -> Declaration,
{
    let fully_qualified_name = {
        let read_lock = graph.declarations().read().unwrap();
        let owner = read_lock.get(&owner_id).unwrap();
        let name_str = graph.strings().get(&str_id).unwrap();
        format!("{}::{name_str}", owner.name())
    };
    let declaration_id = DeclarationId::from(&fully_qualified_name);

    graph.add_declaration(declaration_id, definition_id, || {
        declaration_builder(fully_qualified_name)
    });
    graph.add_member(&owner_id, declaration_id, str_id);
}

/// Resolves owner for class variables, bypassing singleton classes.
fn resolve_class_variable_owner(graph: &Graph, lexical_nesting_id: Option<DefinitionId>) -> Option<DeclarationId> {
    let mut current_nesting = lexical_nesting_id;
    while let Some(nesting_id) = current_nesting {
        if let Some(nesting_def) = graph.definitions().get(&nesting_id)
            && matches!(nesting_def, Definition::SingletonClass(_))
        {
            current_nesting = *nesting_def.lexical_nesting_id();
        } else {
            break;
        }
    }
    current_nesting.and_then(|id| graph.definitions_to_declarations().get(&id).copied())
}

/// Resolves owner from lexical nesting.
fn resolve_lexical_owner(graph: &Graph, lexical_nesting_id: Option<DefinitionId>) -> DeclarationId {
    let Some(id) = lexical_nesting_id else {
        return *OBJECT_ID;
    };

    // If no declaration exists yet for this definition, walk up the lexical chain.
    // This handles the case where attr_* definitions inside methods are processed
    // before the method definition itself.
    let Some(declaration_id) = graph.definitions_to_declarations().get(&id) else {
        let definition = graph.definitions().get(&id).unwrap();
        return resolve_lexical_owner(graph, *definition.lexical_nesting_id());
    };

    let declarations = graph.declarations().read().unwrap();

    // If the associated declaration is a namespace that can own things, we found the right owner. Otherwise, we might
    // have found something nested inside something else (like a method), in which case we have to recurse until we find
    // the appropriate owner
    if matches!(
        declarations.get(declaration_id).unwrap(),
        Declaration::Class(_) | Declaration::Module(_) | Declaration::SingletonClass(_)
    ) {
        *declaration_id
    } else {
        let definition = graph.definitions().get(&id).unwrap();
        resolve_lexical_owner(graph, *definition.lexical_nesting_id())
    }
}

/// Gets or creates a singleton class declaration for a given class/module declaration.
/// For class `Foo`, this returns the declaration for `Foo::<Foo>`.
fn get_or_create_singleton_class(graph: &Graph, attached_id: DeclarationId) -> DeclarationId {
    let (decl_id, name) = {
        let lock = graph.declarations().read().unwrap();
        let attached_decl = lock.get(&attached_id).unwrap();

        // TODO: the constant check is a temporary hack. We need to implement proper handling for `Struct.new`, `Class.new`
        // and `Module.new`, which now seem like constants, but are actually namespaces
        if !matches!(attached_decl, Declaration::Constant(_))
            && let Some(singleton_id) = singleton_class_id(attached_decl)
        {
            return *singleton_id;
        }

        let name = format!("{}::<{}>", attached_decl.name(), attached_decl.unqualified_name());
        (DeclarationId::from(&name), name)
    };

    let mut write_lock = graph.declarations().write().unwrap();
    let attached_decl = write_lock.get_mut(&attached_id).unwrap();

    // TODO: the constant check is a temporary hack. We need to implement proper handling for `Struct.new`, `Class.new`
    // and `Module.new`, which now seem like constants, but are actually namespaces
    if !matches!(attached_decl, Declaration::Constant(_)) {
        set_singleton_class_id(attached_decl, decl_id);
    }

    write_lock
        .entry(decl_id)
        .or_insert_with(|| Declaration::SingletonClass(Box::new(NamespaceDeclaration::new(name, attached_id))));

    decl_id
}

/// Linearizes the ancestors of a declaration, returning the list of ancestor declaration IDs
///
/// # Panics
///
/// Can panic if there's inconsistent data in the graph
#[must_use]
pub fn ancestors_of(graph: &Graph, declaration_id: DeclarationId) -> Ancestors {
    let mut context = LinearizationContext::new();
    linearize_ancestors(graph, declaration_id, &mut context)
}

/// Linearizes the ancestors of a declaration, returning the list of ancestor declaration IDs
///
/// # Panics
///
/// Can panic if there's inconsistent data in the graph
#[must_use]
fn linearize_ancestors(graph: &Graph, declaration_id: DeclarationId, context: &mut LinearizationContext) -> Ancestors {
    {
        let declarations = graph.declarations().read().unwrap();
        let declaration = declarations.get(&declaration_id).unwrap();

        // TODO: this is a temporary hack. We need to implement proper handling for `Struct.new`, `Class.new` and
        // `Module.new`, which now seem like constants, but are actually namespaces
        if matches!(declaration, Declaration::Constant(_)) {
            return Ancestors::Complete(vec![]);
        }

        // Add this declaration to the descendants so that we capture transitive descendant relationships
        context.descendants.insert(declaration_id);

        // Return the cached ancestors if we already computed them. If they are partial ancestors, ignore the cache to try
        // again
        if has_complete_ancestors(declaration) {
            let cached = clone_ancestors(declaration);
            propagate_descendants(graph, &mut context.descendants, &cached);
            context.descendants.remove(&declaration_id);
            return cached;
        }

        // Automatically track descendants as we recurse. This has to happen before checking the cache since we may have
        // already linearized the parent's ancestors, but it's the first time we're discovering the descendant
        for descendant in &context.descendants {
            add_descendant(declaration, *descendant);
        }

        if !context.seen_ids.insert(declaration_id) {
            // If we find a cycle when linearizing ancestors, it's an error that the programmer must fix. However, we try to
            // still approximate features by assuming that it must inherit from `Object` at some point (which is what most
            // classes/modules inherit from). This is not 100% correct, but it allows us to provide a bit better IDE support
            // for these cases
            let estimated_ancestors = if matches!(declaration, Declaration::Class(_)) {
                Ancestors::Cyclic(vec![Ancestor::Complete(*OBJECT_ID)])
            } else {
                Ancestors::Cyclic(vec![])
            };
            set_ancestors(declaration, estimated_ancestors.clone());
            context.descendants.remove(&declaration_id);
            return estimated_ancestors;
        }
    }

    // TODO: this check is against `Object` for now to avoid infinite recursion. After RBS indexing, we need to change
    // this to `BasicObject` since it's the only class that cannot have a parent
    let parent_ancestors = linearize_parent_ancestors(graph, declaration_id, context);

    let declarations = graph.declarations().read().unwrap();
    let declaration = declarations.get(&declaration_id).unwrap();
    let definitions = declaration
        .definitions()
        .iter()
        .filter_map(|def_id| graph.definitions().get(def_id))
        .collect::<Vec<_>>();

    let mut mixins = Vec::new();

    // If we're linearizing a singleton class, add the extends of the attached class to the list of mixins to process
    if let Declaration::SingletonClass(_) = declaration {
        let attached_decl = declarations.get(declaration.owner_id()).unwrap();

        let attached_definitions = attached_decl
            .definitions()
            .iter()
            .filter_map(|def_id| graph.definitions().get(def_id))
            .collect::<Vec<_>>();

        mixins.extend(
            attached_definitions
                .iter()
                .filter_map(|def| mixins_of(def))
                .flatten()
                .filter(|mixin| matches!(mixin, Mixin::Extend(_))),
        );
    }

    // Consider only prepends and includes for the current declaration
    mixins.extend(
        definitions
            .iter()
            .filter_map(|def| mixins_of(def))
            .flatten()
            .filter(|mixin| matches!(mixin, Mixin::Prepend(_) | Mixin::Include(_))),
    );

    let (linearized_prepends, linearized_includes) =
        linearize_mixins(graph, context, &mixins, parent_ancestors.as_ref());

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
    set_ancestors(declaration, result.clone());

    context.descendants.remove(&declaration_id);
    result
}

fn linearize_parent_ancestors(
    graph: &Graph,
    declaration_id: DeclarationId,
    context: &mut LinearizationContext,
) -> Option<Vec<Ancestor>> {
    if declaration_id == *OBJECT_ID {
        return None;
    }

    let declarations = graph.declarations().read().unwrap();
    let declaration = declarations.get(&declaration_id).unwrap();

    match declaration {
        Declaration::Class(_) => {
            let definitions: Vec<_> = declaration
                .definitions()
                .iter()
                .filter_map(|def_id| graph.definitions().get(def_id))
                .collect();
            // Release read lock before calling functions that may acquire write locks
            drop(declarations);

            Some(match linearize_parent_class(graph, &definitions, context) {
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
        Declaration::SingletonClass(_) => {
            let owner_id = *declaration.owner_id();
            // Release read lock before calling functions that may acquire write locks
            drop(declarations);

            let (singleton_parent_id, partial_singleton) = singleton_parent_id(graph, owner_id);
            if partial_singleton {
                context.partial = true;
            }

            Some(match linearize_ancestors(graph, singleton_parent_id, context) {
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
    graph: &Graph,
    context: &mut LinearizationContext,
    mixins: &[&Mixin],
    parent_ancestors: Option<&Vec<Ancestor>>,
) -> (VecDeque<Ancestor>, VecDeque<Ancestor>) {
    let mut linearized_prepends = VecDeque::new();
    let mut linearized_includes = VecDeque::new();

    // IMPORTANT! In the slice of mixins we receive, extends are the ones that occurred in the attached object, which we
    // collect ahead of time. This is the reason why we apparently treat an extend like an include, because an extend in
    // the attached object is equivalent to an include in the singleton class
    for mixin in mixins {
        match mixin {
            Mixin::Prepend(name_id) => {
                match graph.names().get(name_id).unwrap() {
                    NameRef::Resolved(resolved) => {
                        let ids = match linearize_ancestors(graph, *resolved.declaration_id(), context) {
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
                        linearized_prepends.push_front(Ancestor::Partial(*name_id));
                    }
                }
            }
            Mixin::Include(name_id) | Mixin::Extend(name_id) => {
                match graph.names().get(name_id).unwrap() {
                    NameRef::Resolved(resolved) => {
                        let mut ids = match linearize_ancestors(graph, *resolved.declaration_id(), context) {
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
                        linearized_includes.push_front(Ancestor::Partial(*name_id));
                    }
                }
            }
        }
    }

    (linearized_prepends, linearized_includes)
}

/// Propagate descendants to all cached ancestors
fn propagate_descendants<S: BuildHasher>(
    graph: &Graph,
    descendants: &mut HashSet<DeclarationId, S>,
    cached: &Ancestors,
) {
    if !descendants.is_empty() {
        let read_lock = graph.declarations().read().unwrap();

        for ancestor in cached {
            if let Ancestor::Complete(ancestor_id) = ancestor
                && let Some(ancestor_decl) = read_lock.get(ancestor_id)
            {
                for descendant in descendants.iter() {
                    add_descendant(ancestor_decl, *descendant);
                }
            }
        }
    }
}

// Handles the resolution of the namespace name, the creation of the declaration and membership
fn handle_constant_declaration<F>(
    graph: &mut Graph,
    name_id: NameId,
    definition_id: DefinitionId,
    singleton: bool,
    declaration_builder: F,
) -> Outcome
where
    F: FnOnce(String, DeclarationId) -> Declaration,
{
    let name_ref = graph.names().get(&name_id).unwrap();
    let str_id = *name_ref.str();

    // The name of the declaration is determined by the name of its owner, which may or may not require resolution
    // depending on whether the name has a parent scope
    match name_owner_id(graph, name_id) {
        Outcome::Resolved(owner_id, id_needing_linearization) => {
            let mut fully_qualified_name = graph.strings().get(&str_id).unwrap().clone();

            {
                let read_lock = graph.declarations().read().unwrap();
                let owner = read_lock.get(&owner_id).unwrap();

                // We don't prefix declarations with `Object::`
                if owner_id != *OBJECT_ID {
                    fully_qualified_name.insert_str(0, "::");
                    fully_qualified_name.insert_str(0, owner.name());
                }
            }

            let declaration_id = DeclarationId::from(&fully_qualified_name);

            if singleton {
                let mut write_lock = graph.declarations().write().unwrap();
                let owner = write_lock.get_mut(&owner_id).unwrap();
                set_singleton_class_id(owner, declaration_id);
            } else {
                graph.add_member(&owner_id, declaration_id, str_id);
            }

            graph.add_declaration(declaration_id, definition_id, || {
                declaration_builder(fully_qualified_name, owner_id)
            });
            graph.record_resolved_name(name_id, declaration_id);
            Outcome::Resolved(declaration_id, id_needing_linearization)
        }
        other => other,
    }
}

// Returns the owner declaration ID for a given name. If the name is simple and has no parent scope, then the owner is
// either the nesting or Object. If the name has a parent scope, we attempt to resolve the reference and that should be
// the name's owner
fn name_owner_id(graph: &mut Graph, name_id: NameId) -> Outcome {
    let name_ref = graph.names().get(&name_id).unwrap();

    if let Some(parent_scope) = name_ref.parent_scope() {
        // If we have `A::B`, the owner of `B` is whatever `A` resolves to
        resolve_constant(graph, *parent_scope)
    } else if let Some(nesting_id) = name_ref.nesting() {
        let nesting_name_ref = graph.names().get(nesting_id).unwrap();

        match nesting_name_ref {
            NameRef::Resolved(resolved) => Outcome::Resolved(*resolved.declaration_id(), None),
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

/// Attempts to resolve a constant reference against the graph. Returns the fully qualified declaration ID that the
/// reference is related to or `None`. This method mutates the graph to remember which constants have already been
/// resolved
fn resolve_constant(graph: &mut Graph, name_id: NameId) -> Outcome {
    let name_ref = graph.names().get(&name_id).unwrap();

    match name_ref {
        NameRef::Unresolved(name) => {
            // If there's a parent scope for this constant, it means it's a constant path. We must first resolve the
            // outer most parent, so that we can then continue resolution from there, recording the results along the
            // way
            if let Some(parent_scope_id) = name.parent_scope() {
                if let NameRef::Resolved(parent_scope) = graph.names().get(parent_scope_id).unwrap() {
                    let outcome = {
                        let read_lock = graph.declarations().read().unwrap();
                        let declaration = read_lock.get(parent_scope.declaration_id()).unwrap();

                        // TODO: this is a temporary hack. We need to implement proper handling for `Struct.new`, `Class.new` and
                        // `Module.new`, which now seem like constants, but are actually namespaces
                        if matches!(declaration, Declaration::Constant(_)) {
                            return Outcome::Unresolved(None);
                        }

                        search_ancestors(graph, *parent_scope_id, *name.str())
                    };

                    if let Outcome::Resolved(member_id, _) = outcome {
                        graph.record_resolved_name(name_id, member_id);
                    }

                    return outcome;
                }

                return Outcome::Retry;
            }

            // Otherwise, it's a simple constant read and we can resolve it directly
            let result = run_resolution(graph, name);

            if let Outcome::Resolved(declaration_id, _) = result {
                graph.record_resolved_name(name_id, declaration_id);
            }

            result
        }
        NameRef::Resolved(resolved) => Outcome::Resolved(*resolved.declaration_id(), None),
    }
}

fn run_resolution(graph: &Graph, name: &Name) -> Outcome {
    let str_id = *name.str();
    let mut missing_linearization_id = None;

    if let Some(nesting) = name.nesting() {
        let scope_outcome = search_lexical_scopes(graph, name, str_id);

        // If we already resolved or need to retry, return early
        if scope_outcome.is_resolved_or_retry() {
            return scope_outcome;
        }

        // Search inheritance chain
        let ancestor_outcome = search_ancestors(graph, *nesting, str_id);
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
    let outcome = search_top_level(graph, str_id);

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

fn search_ancestors(graph: &Graph, nesting_name_id: NameId, str_id: StringId) -> Outcome {
    match graph.names().get(&nesting_name_id).unwrap() {
        NameRef::Resolved(nesting_name_ref) => {
            let nesting_id = *nesting_name_ref.declaration_id();

            match ancestors_of(graph, nesting_id) {
                Ancestors::Complete(ids) | Ancestors::Cyclic(ids) => ids
                    .iter()
                    .find_map(|ancestor_id| {
                        if let Ancestor::Complete(ancestor_id) = ancestor_id {
                            let read_lock = graph.declarations().read().unwrap();
                            let declaration = read_lock.get(ancestor_id).unwrap();
                            get_member(declaration, str_id).map(|id| Outcome::Resolved(*id, None))
                        } else {
                            None
                        }
                    })
                    .unwrap_or(Outcome::Unresolved(None)),
                Ancestors::Partial(ids) => ids
                    .iter()
                    .find_map(|ancestor_id| {
                        if let Ancestor::Complete(ancestor_id) = ancestor_id {
                            let read_lock = graph.declarations().read().unwrap();
                            let declaration = read_lock.get(ancestor_id).unwrap();
                            get_member(declaration, str_id).map(|id| Outcome::Resolved(*id, Some(nesting_id)))
                        } else {
                            None
                        }
                    })
                    .unwrap_or(Outcome::Unresolved(Some(nesting_id))),
            }
        }
        NameRef::Unresolved(_) => Outcome::Retry,
    }
}

/// Look for the constant in the lexical scopes that are a part of its nesting
fn search_lexical_scopes(graph: &Graph, name: &Name, str_id: StringId) -> Outcome {
    let mut current_name = name;

    while let Some(nesting_id) = current_name.nesting() {
        if let NameRef::Resolved(nesting_name_ref) = graph.names().get(nesting_id).unwrap() {
            let read_lock = graph.declarations().read().unwrap();

            if let Some(declaration) = read_lock.get(nesting_name_ref.declaration_id())
            && !matches!(declaration, Declaration::Constant(_)) // TODO: temporary hack to avoid crashing on `Struct.new`
                && let Some(member) = members_of(declaration).get(&str_id)
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
fn search_top_level(graph: &Graph, str_id: StringId) -> Outcome {
    let read_lock = graph.declarations().read().unwrap();
    let object = read_lock.get(&OBJECT_ID).unwrap();

    match members_of(object).get(&str_id) {
        Some(member_id) => Outcome::Resolved(*member_id, None),
        None => Outcome::Unresolved(None),
    }
}

fn members_of(decl: &Declaration) -> &IdentityHashMap<StringId, DeclarationId> {
    decl.as_namespace()
        .expect("Tried to get members for a declaration that isn't a namespace")
        .members()
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
        name_depth(name_ref, names)
    });
    let nesting_depth = name.nesting().map_or(0, |id| {
        let name_ref = names.get(&id).unwrap();
        name_depth(name_ref, names)
    });

    parent_depth + nesting_depth + 1
}

/// Returns a tuple of 2 vectors:
/// - The first one contains all constants, sorted in order for resolution (less complex constant names first)
/// - The second one contains all other definitions, in no particular order
#[must_use]
fn sorted_units(graph: &Graph) -> (VecDeque<Unit>, Vec<DefinitionId>) {
    let estimated_length = graph.definitions().len() / 2;
    let mut constants = Vec::with_capacity(estimated_length);
    let mut others = Vec::with_capacity(estimated_length);
    let names = graph.names();

    for (id, definition) in graph.definitions() {
        match definition {
            Definition::Class(def) => {
                constants.push((Unit::Definition(*id), names.get(def.name_id()).unwrap()));
            }
            Definition::Module(def) => {
                constants.push((Unit::Definition(*id), names.get(def.name_id()).unwrap()));
            }
            Definition::Constant(def) => {
                constants.push((Unit::Definition(*id), names.get(def.name_id()).unwrap()));
            }
            Definition::SingletonClass(def) => {
                constants.push((Unit::Definition(*id), names.get(def.name_id()).unwrap()));
            }
            _ => {
                others.push(*id);
            }
        }
    }

    for (id, constant_ref) in graph.constant_references() {
        constants.push((Unit::Reference(*id), names.get(constant_ref.name_id()).unwrap()));
    }

    // Sort namespaces based on their name complexity so that simpler names are always first
    constants.sort_by(|(_, name_a), (_, name_b)| name_depth(name_a, names).cmp(&name_depth(name_b, names)));

    others.shrink_to_fit();
    (constants.into_iter().map(|(id, _)| id).collect::<VecDeque<_>>(), others)
}

/// Returns the singleton parent ID for an attached object ID. A singleton class' parent depends on what the attached
/// object is:
///
/// - Module: parent is the `Module` class
/// - Class: parent is the singleton class of the original parent class
/// - Singleton class: recurse as many times as necessary to wrap the original attached object's parent class
fn singleton_parent_id(graph: &Graph, attached_id: DeclarationId) -> (DeclarationId, bool) {
    // Base case: if we reached `Object`, then the parent is `Class`
    if attached_id == *OBJECT_ID {
        return (*CLASS_ID, false);
    }

    let declarations = graph.declarations().read().unwrap();
    let decl = declarations.get(&attached_id).unwrap();

    match decl {
        Declaration::Module(_) => (*MODULE_ID, false),
        Declaration::SingletonClass(_) => {
            // For singleton classes, we keep recursively wrapping parents until we can reach the original attached
            // object
            let owner_id = *decl.owner_id();
            // Release read lock before calling functions that may acquire write locks
            drop(declarations);
            let (inner_parent, partial) = singleton_parent_id(graph, owner_id);
            (get_or_create_singleton_class(graph, inner_parent), partial)
        }
        Declaration::Class(_) => {
            // For classes (the regular case), we need to return the singleton class of its parent
            let definitions: Vec<_> = decl
                .definitions()
                .iter()
                .filter_map(|def_id| graph.definitions().get(def_id))
                .collect();
            // Release read lock before calling functions that may acquire write locks
            drop(declarations);

            let (picked_parent, partial) = get_parent_class(graph, &definitions);
            (get_or_create_singleton_class(graph, picked_parent), partial)
        }
        _ => {
            // Other declaration types (constants, methods, etc.) shouldn't reach here,
            // but default to Object's singleton parent
            drop(declarations);
            (*CLASS_ID, false)
        }
    }
}

fn get_parent_class(graph: &Graph, definitions: &[&Definition]) -> (DeclarationId, bool) {
    let mut explicit_parents = Vec::new();
    let mut partial = false;

    for def in definitions {
        if let Definition::Class(class) = def
            && let Some(superclass) = class.superclass_ref()
        {
            let name = graph.names().get(&superclass).unwrap();

            match name {
                NameRef::Resolved(resolved) => {
                    explicit_parents.push(*resolved.declaration_id());
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

fn linearize_parent_class(graph: &Graph, definitions: &[&Definition], context: &mut LinearizationContext) -> Ancestors {
    let (picked_parent, partial) = get_parent_class(graph, definitions);
    let result = linearize_ancestors(graph, picked_parent, context);
    if partial { result.to_partial() } else { result }
}

fn add_descendant(declaration: &Declaration, descendant_id: DeclarationId) {
    declaration
        .as_namespace()
        .expect("Tried to add descendant for a declaration that isn't a namespace")
        .add_descendant(descendant_id);
}

fn clone_ancestors(declaration: &Declaration) -> Ancestors {
    declaration
        .as_namespace()
        .expect("Tried to get ancestors for a declaration that isn't a namespace")
        .clone_ancestors()
}

fn has_complete_ancestors(declaration: &Declaration) -> bool {
    declaration
        .as_namespace()
        .expect("Tried to check complete ancestors for a declaration that isn't a namespace")
        .has_complete_ancestors()
}

fn set_ancestors(declaration: &Declaration, ancestors: Ancestors) {
    declaration
        .as_namespace()
        .expect("Tried to set ancestors for a declaration that isn't a namespace")
        .set_ancestors(ancestors);
}

fn get_member(declaration: &Declaration, str_id: StringId) -> Option<&DeclarationId> {
    declaration
        .as_namespace()
        .expect("Tried to get member for a declaration that isn't a namespace")
        .get_member(&str_id)
}

fn mixins_of(definition: &Definition) -> Option<&[Mixin]> {
    match definition {
        Definition::Class(class) => Some(class.mixins()),
        Definition::SingletonClass(class) => Some(class.mixins()),
        Definition::Module(module) => Some(module.mixins()),
        _ => None,
    }
}

fn singleton_class_id(declaration: &Declaration) -> Option<&DeclarationId> {
    declaration
        .as_namespace()
        .expect("Tried to get singleton class ID for a declaration that isn't a namespace")
        .singleton_class_id()
}

fn set_singleton_class_id(declaration: &mut Declaration, id: DeclarationId) {
    declaration
        .as_namespace_mut()
        .expect("Tried to set singleton class ID for a declaration that isn't a namespace")
        .set_singleton_class_id(id);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::ids::UriId;
    use crate::test_utils::GraphTest;

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

            let read_lock = $context.graph().declarations().read().unwrap();
            let declaration = read_lock
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
            match ancestors_of(&$context.graph(), DeclarationId::from($name)) {
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
                                        let read_lock = $context.graph().declarations().read().unwrap();
                                        read_lock.get(id).unwrap().name().to_string()
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
                    panic!("Expected ancestors to be resolved");
                }
            }
        };
    }

    macro_rules! assert_descendants {
        ($context:expr, $parent:expr, $descendants:expr) => {
            let read_lock = $context.graph().declarations().read().unwrap();
            let parent = read_lock.get(&DeclarationId::from($parent)).unwrap();
            let actual = match parent {
                Declaration::Class(class) => class.descendants().iter().cloned().collect::<Vec<_>>(),
                Declaration::Module(module) => module.descendants().iter().cloned().collect::<Vec<_>>(),
                Declaration::SingletonClass(singleton) => singleton.descendants().iter().cloned().collect::<Vec<_>>(),
                _ => panic!("Tried to get descendants for a declaration that isn't a namespace"),
            };

            for descendant in &$descendants {
                let descendant_id = DeclarationId::from(*descendant);
                let read_lock = $context.graph().declarations().read().unwrap();

                assert!(
                    actual.contains(&descendant_id),
                    "Expected '{}' to be a descendant of '{}'",
                    read_lock.get(&descendant_id).unwrap().name(),
                    parent.name()
                );
            }
        };
    }

    fn assert_members(decl: &Declaration, members: &[&str]) {
        let actual_members = match decl {
            Declaration::Class(class) => class.members(),
            Declaration::Module(module) => module.members(),
            Declaration::SingletonClass(singleton) => singleton.members(),
            _ => panic!("Tried to assert members for a declaration that isn't a namespace"),
        };

        for member in members {
            assert!(
                actual_members.contains_key(&StringId::from(*member)),
                "Expected member '{member}' not found"
            );
        }
    }

    fn assert_owner(decl: &Declaration, owner_name: &str) {
        assert_eq!(
            decl.owner_id(),
            &DeclarationId::from(owner_name),
            "Expected owner '{owner_name}' but found different owner"
        );
    }

    fn assert_instance_variable(context: &GraphTest, fqn: &str, owner: &str) {
        let declarations = context.graph().declarations().read().unwrap();
        let decl = declarations
            .get(&DeclarationId::from(fqn))
            .unwrap_or_else(|| panic!("{fqn} declaration should exist"));
        assert!(
            matches!(decl, Declaration::InstanceVariable(_)),
            "{fqn} should be InstanceVariable, got {decl:?}"
        );
        assert_owner(decl, owner);
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

        let read_lock = context.graph().declarations().read().unwrap();
        let foo = read_lock.get(&DeclarationId::from("Foo")).unwrap();
        assert_members(foo, &["Bar", "Baz"]);
        assert_owner(foo, "Object");

        let bar = read_lock.get(&DeclarationId::from("Foo::Bar")).unwrap();
        assert_members(bar, &[]);
        assert_owner(bar, "Foo");

        let baz = read_lock.get(&DeclarationId::from("Foo::Baz")).unwrap();
        assert_members(baz, &[]);
        assert_owner(baz, "Foo");
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

        let read_lock = context.graph().declarations().read().unwrap();
        let foo = read_lock.get(&DeclarationId::from("Foo")).unwrap();
        assert_members(foo, &["initialize", "@name"]);
        assert_owner(foo, "Object");
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

        let read_lock = context.graph().declarations().read().unwrap();
        let foo = read_lock.get(&DeclarationId::from("Foo")).unwrap();
        assert_members(foo, &[]);
        assert_owner(foo, "Object");

        let bar = read_lock.get(&DeclarationId::from("Bar")).unwrap();
        assert_members(bar, &["Baz"]);
        assert_owner(bar, "Object");
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

        let read_lock = context.graph().declarations().read().unwrap();
        let foo = read_lock.get(&DeclarationId::from("Foo")).unwrap();
        assert_members(foo, &[]);
        assert_owner(foo, "Object");

        let bar = read_lock.get(&DeclarationId::from("Bar")).unwrap();
        assert_members(bar, &["Baz"]);
        assert_owner(bar, "Object");

        let baz = read_lock.get(&DeclarationId::from("Bar::Baz")).unwrap();
        assert_members(baz, &[]);
        assert_owner(baz, "Bar");
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
        let read_lock = context.graph().declarations().read().unwrap();
        assert!(read_lock.get(&DeclarationId::from("Foo")).is_none());
        assert!(read_lock.get(&DeclarationId::from("Foo::Bar")).is_none());
        assert!(read_lock.get(&DeclarationId::from("Foo::Bar::Baz")).is_none());
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

        names.sort_by_key(|a| name_depth(a, context.graph().names()));

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

        let read_lock = context.graph().declarations().read().unwrap();
        let foo = read_lock.get(&DeclarationId::from("Foo")).unwrap();
        assert_members(foo, &[]);
        assert_owner(foo, "Object");
        assert_eq!(&DeclarationId::from("Foo::<Foo>"), singleton_class_id(foo).unwrap());

        let singleton = read_lock.get(&DeclarationId::from("Foo::<Foo>")).unwrap();
        assert_members(singleton, &["bar", "BAZ"]);
        assert_owner(singleton, "Foo");
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

        let read_lock = context.graph().declarations().read().unwrap();
        let foo = read_lock.get(&DeclarationId::from("Foo")).unwrap();
        assert_members(foo, &[]);
        assert_eq!(&DeclarationId::from("Foo::<Foo>"), singleton_class_id(foo).unwrap());

        let singleton = read_lock.get(&DeclarationId::from("Foo::<Foo>")).unwrap();
        assert_members(singleton, &[]);
        assert_eq!(
            &DeclarationId::from("Foo::<Foo>::<<Foo>>"),
            singleton_class_id(singleton).unwrap()
        );

        let nested_singleton = read_lock.get(&DeclarationId::from("Foo::<Foo>::<<Foo>>")).unwrap();
        assert_members(nested_singleton, &["baz"]);
        assert_owner(nested_singleton, "Foo::<Foo>");
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

        let read_lock = context.graph().declarations().read().unwrap();
        let foo = read_lock.get(&DeclarationId::from("Foo")).unwrap();
        assert_members(foo, &[]);
        assert_owner(foo, "Object");
        assert_eq!(&DeclarationId::from("Foo::<Foo>"), singleton_class_id(foo).unwrap());

        let bar = read_lock.get(&DeclarationId::from("Bar")).unwrap();
        assert_members(bar, &[]);
        assert_owner(bar, "Object");

        let singleton = read_lock.get(&DeclarationId::from("Foo::<Foo>")).unwrap();
        assert_members(singleton, &["baz", "Baz"]);
        assert_owner(singleton, "Foo");
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

        let read_lock = context.graph().declarations().read().unwrap();
        let foo = read_lock.get(&DeclarationId::from("Foo")).unwrap();
        assert_members(foo, &["@@bar", "@@baz"]);
        assert_owner(foo, "Object");
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

        let declarations = context.graph().declarations().read().unwrap();
        let foo = declarations.get(&DeclarationId::from("Foo")).unwrap();
        assert_members(foo, &["bar", "@@baz"]);
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

        let declarations = context.graph().declarations().read().unwrap();
        let foo = declarations.get(&DeclarationId::from("Foo")).unwrap();
        assert_members(foo, &[]);

        let bar = declarations.get(&DeclarationId::from("Bar")).unwrap();
        assert_members(bar, &["@@cvar1", "@@cvar2"]);
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

        // TODO: this should push an error diagnostic
        let read_lock = context.graph().declarations().read().unwrap();
        assert!(read_lock.get(&DeclarationId::from("Object::@@var")).is_none());
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
        let read_lock = context.graph().declarations().read().unwrap();
        assert!(read_lock.get(&DeclarationId::from("Foo::<Foo>")).is_some());

        let foo = read_lock.get(&DeclarationId::from("Foo")).unwrap();
        assert_eq!(&DeclarationId::from("Foo::<Foo>"), singleton_class_id(foo).unwrap(),);
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

        let read_lock = context.graph().declarations().read().unwrap();
        let foo_singleton = read_lock.get(&DeclarationId::from("Foo::<Foo>")).unwrap();
        assert_members(foo_singleton, &["bar", "baz"]);
        assert_owner(foo_singleton, "Foo");

        let nested_foo_singleton = read_lock.get(&DeclarationId::from("Foo::<Foo>::<<Foo>>")).unwrap();
        assert_members(nested_foo_singleton, &["nested_bar"]);
        assert_owner(nested_foo_singleton, "Foo::<Foo>");

        let bar_singleton = read_lock.get(&DeclarationId::from("Bar::<Bar>")).unwrap();
        assert_members(bar_singleton, &["qux"]);
        assert_owner(bar_singleton, "Bar");
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

        assert!(matches!(
            ancestors_of(context.graph(), DeclarationId::from("Bar")),
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

        assert_instance_variable(&context, "Foo::<Foo>::@foo", "Foo::<Foo>");
        assert_instance_variable(&context, "Foo::@bar", "Foo");
        assert_instance_variable(&context, "Foo::<Foo>::@baz", "Foo::<Foo>");
        // @qux in `class << self; def qux` - self is Foo when called, so @qux belongs to Foo's singleton class
        assert_instance_variable(&context, "Foo::<Foo>::@qux", "Foo::<Foo>");
        assert_instance_variable(&context, "Foo::<Foo>::<<Foo>>::@nested", "Foo::<Foo>::<<Foo>>");
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

        assert_instance_variable(&context, "Foo::<Foo>::@foo", "Foo::<Foo>");
        assert_instance_variable(&context, "Bar::<Bar>::@baz", "Bar::<Bar>");
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

        // The class is `Bar::Baz`, so its singleton class is `Bar::Baz::<Baz>`
        assert_instance_variable(&context, "Bar::Baz::<Baz>::@baz", "Bar::Baz::<Baz>");
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

        assert_instance_variable(&context, "Foo::<Foo>::<<Foo>>::@bar", "Foo::<Foo>::<<Foo>>");
        assert_instance_variable(
            &context,
            "Foo::<Foo>::<<Foo>>::<<<Foo>>>::@baz",
            "Foo::<Foo>::<<Foo>>::<<<Foo>>>",
        );
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

        // Top-level instance variables belong to `<main>`, not `Object`.
        // We can't represent `<main>` yet, so no declaration is created.
        let declarations = context.graph().declarations().read().unwrap();
        let foo_decl = declarations.get(&DeclarationId::from("Object::@foo"));
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

        // Instance variable in method with unresolved receiver should not create a declaration
        let declarations = context.graph().declarations().read().unwrap();
        let baz_decl = declarations.get(&DeclarationId::from("Object::@baz"));
        assert!(baz_decl.is_none(), "@baz declaration should not exist");

        let foo_baz_decl = declarations.get(&DeclarationId::from("Foo::@baz"));
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

        let read_lock = context.graph().declarations().read().unwrap();
        let foo_class = read_lock.get(&DeclarationId::from("Foo")).unwrap();
        assert_members(foo_class, &["foo", "bar"]);
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

        let read_lock = context.graph().declarations().read().unwrap();
        let foo_class = read_lock.get(&DeclarationId::from("Object")).unwrap();
        assert_members(foo_class, &["$bar", "$foo"]);
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
        assert_ancestors_eq!(context, "B", ["B"]);
        assert_ancestors_eq!(context, "A", Vec::<&str>::new());
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

        let read_lock = context.graph().declarations().read().unwrap();
        let bar = read_lock.get(&DeclarationId::from("Foo::Bar")).unwrap();
        assert_members(bar, &["Qux"]);
        assert_owner(bar, "Foo");

        let qux = read_lock.get(&DeclarationId::from("Foo::Bar::Qux")).unwrap();
        assert_members(qux, &[]);
        assert_owner(qux, "Foo::Bar");
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
        assert_ancestors_eq!(context, "B", ["B"]);
        assert_ancestors_eq!(context, "A", Vec::<&str>::new());
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

        let read_lock = context.graph().declarations().read().unwrap();
        let bar = read_lock.get(&DeclarationId::from("Foo::Bar")).unwrap();
        assert_members(bar, &["Qux"]);
        assert_owner(bar, "Foo");

        let qux = read_lock.get(&DeclarationId::from("Foo::Bar::Qux")).unwrap();
        assert_members(qux, &[]);
        assert_owner(qux, "Foo::Bar");
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

        // Global variable aliases should still be owned by Object, regardless of where defined
        let read_lock = context.graph().declarations().read().unwrap();
        let object = read_lock.get(&DeclarationId::from("Object")).unwrap();
        assert_members(object, &["$bar"]);
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

        let read_lock = context.graph().declarations().read().unwrap();
        let foo_class = read_lock.get(&DeclarationId::from("Foo")).unwrap();
        // inner_method should be owned by Foo, not by setup
        assert_members(foo_class, &["setup", "inner_method"]);
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

        let read_lock = context.graph().declarations().read().unwrap();
        let foo_singleton_class = read_lock.get(&DeclarationId::from("Foo::<Foo>")).unwrap();
        assert_members(foo_singleton_class, &["setup"]);

        // All attr_* should be owned by Foo, not by setup
        let foo_class = read_lock.get(&DeclarationId::from("Foo")).unwrap();
        assert_members(foo_class, &["reader_attr", "writer_attr", "accessor_attr"]);
    }
}
