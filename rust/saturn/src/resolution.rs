use std::collections::VecDeque;

use crate::model::{
    declaration::Declaration,
    definitions::Definition,
    graph::{Graph, OBJECT_ID},
    identity_maps::IdentityHashMap,
    ids::{DeclarationId, DefinitionId, NameId, ReferenceId, StringId},
    name::{Name, NameRef},
};

pub enum Unit {
    Definition(DefinitionId),
    Reference(ReferenceId),
}

enum Outcome {
    /// The constant was successfully resolved to the given declaration ID
    Resolved(DeclarationId),
    /// We had everything we needed to resolved this constant, but we couldn't find it. This means it's not defined (or
    /// defined in a way that static analysis won't discover it)
    Unresolved,
    /// We couldn't resolve this constant right now because certain dependencies were missing. For example, a constant
    /// reference involved in computing ancestors (like an include) was found, but wasn't resolved yet. We need to place
    /// this back in the queue to retry once we have progressed further
    Retry,
}

impl Outcome {
    fn is_resolved_or_retry(&self) -> bool {
        matches!(self, Outcome::Resolved(_) | Outcome::Retry)
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
    graph
        .declarations_mut()
        .insert(*OBJECT_ID, Declaration::new("Object".to_string(), *OBJECT_ID));

    let (mut constant_queue, other_ids) = sorted_units(graph);
    // Flag to indicate whether we successfully resolved something in the last resolution iteration, thus making
    // progress. If nothing got resolved, then we stop because trying again would likely not successfully resolve
    // anything new
    let mut making_progress = true;

    // First, handle all constants, ensuring that we create declarations, members, owners and resolve any name
    // references that are necessary along the way
    while making_progress && let Some(unit_id) = constant_queue.pop_front() {
        match unit_id {
            Unit::Definition(id) => {
                let name_id = match graph.definitions().get(&id).unwrap() {
                    Definition::Class(class) => *class.name_id(),
                    Definition::Module(module) => *module.name_id(),
                    Definition::Constant(constant) => *constant.name_id(),
                    _ => panic!("Expected constant definitions"),
                };

                match handle_constant_declaration(graph, name_id, id) {
                    Outcome::Retry => {
                        // There might be dependencies we haven't figured out yet, so we need to retry
                        constant_queue.push_back(unit_id);
                        making_progress = false;
                    }
                    Outcome::Unresolved => {
                        // We couldn't resolve this name. Emit a diagnostic
                        making_progress = false;
                    }
                    Outcome::Resolved(_) => making_progress = true,
                }
            }
            Unit::Reference(id) => {
                let constant_ref = graph.constant_references().get(&id).unwrap();

                match resolve_constant(graph, *constant_ref.name_id()) {
                    Outcome::Retry => {
                        // There might be dependencies we haven't figured out yet, so we need to retry
                        constant_queue.push_back(unit_id);
                        making_progress = false;
                    }
                    Outcome::Unresolved => {
                        // We couldn't resolve this name. Emit a diagnostic
                        making_progress = false;
                    }
                    Outcome::Resolved(declaration_id) => {
                        graph.record_resolved_reference(id, declaration_id);
                        making_progress = true;
                    }
                }
            }
        }
    }

    // Handle other definitions that don't require resolution, but need to have their declarations and membership
    // created
    for id in other_ids {
        let definition = graph.definitions().get(&id).unwrap();
        let owner_id = *definition
            .lexical_nesting_id()
            .and_then(|lexical_nesting_id| graph.definitions_to_declarations().get(&lexical_nesting_id))
            .unwrap_or(&OBJECT_ID);

        let owner = graph.declarations().get(&owner_id).unwrap();

        let str_id = *match definition {
            Definition::Method(it) => it.str_id(),
            Definition::AttrAccessor(it) => it.str_id(),
            Definition::AttrReader(it) => it.str_id(),
            Definition::AttrWriter(it) => it.str_id(),
            Definition::GlobalVariable(it) => it.str_id(),
            Definition::InstanceVariable(it) => it.str_id(),
            Definition::ClassVariable(it) => it.str_id(),
            Definition::Class(_) | Definition::Module(_) | Definition::Constant(_) => {
                panic!("Unexpected definition type. This shouldn't happen")
            }
        };

        let name_str = graph.strings().get(&str_id).unwrap();
        let declaration_id = graph.add_declaration(format!("{}::{name_str}", owner.name()), id, owner_id);
        graph.add_member(&owner_id, declaration_id, str_id);
    }
}

// Handles the resolution of the namespace name, the creation of the declaration and membership
fn handle_constant_declaration(graph: &mut Graph, name_id: NameId, definition_id: DefinitionId) -> Outcome {
    let name_ref = graph.names().get(&name_id).unwrap();
    let str_id = *name_ref.str();

    // The name of the declaration is determined by the name of its owner, which may or may not require resolution
    // depending on whether the name has a parent scope
    match name_owner_id(graph, name_id) {
        Outcome::Resolved(owner_id) => {
            let owner = graph.declarations().get(&owner_id).unwrap();

            let mut fully_qualified_name = graph.strings().get(&str_id).unwrap().clone();

            // We don't prefix declarations with `Object::`
            if owner_id != *OBJECT_ID {
                fully_qualified_name.insert_str(0, "::");
                fully_qualified_name.insert_str(0, owner.name());
            }

            let declaration_id = graph.add_declaration(fully_qualified_name, definition_id, owner_id);
            graph.add_member(&owner_id, declaration_id, str_id);
            graph.record_resolved_name(name_id, declaration_id);
            Outcome::Resolved(declaration_id)
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
            NameRef::Resolved(resolved) => Outcome::Resolved(*resolved.declaration_id()),
            NameRef::Unresolved(_) => {
                // The only case where we wouldn't have the nesting resolved at this point is if it's available through
                // inheritance or if it doesn't exist, so we need to retry later
                Outcome::Retry
            }
        }
    } else {
        // Any constants at the top level are owned by Object
        Outcome::Resolved(*OBJECT_ID)
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
                    let declaration = graph.declarations().get(parent_scope.declaration_id()).unwrap();

                    // TODO: we can't just look inside members here, we need to search inheritance too
                    if let Some(member_id) = declaration.members().get(name.str()).copied() {
                        graph.record_resolved_name(name_id, member_id);
                        return Outcome::Resolved(member_id);
                    }
                }

                return Outcome::Retry;
            }

            // Otherwise, it's a simple constant read and we can resolve it directly
            let result = run_resolution(graph, name);

            if let Outcome::Resolved(declaration_id) = result {
                graph.record_resolved_name(name_id, declaration_id);
            }

            result
        }
        NameRef::Resolved(resolved) => Outcome::Resolved(*resolved.declaration_id()),
    }
}

fn run_resolution(graph: &Graph, name: &Name) -> Outcome {
    let str_id = *name.str();

    if name.nesting().is_some() {
        let scope_outcome = search_lexical_scopes(graph, name, str_id);

        // If we already resolved or need to retry, return early
        if scope_outcome.is_resolved_or_retry() {
            return scope_outcome;
        }

        // Search inheritance chain
    }

    // If it's a top level reference starting with `::` or if we didn't find the constant anywhere else, the
    // fallback is the top level
    search_top_level(graph, str_id)
}

/// Look for the constant in the lexical scopes that are a part of its nesting
fn search_lexical_scopes(graph: &Graph, name: &Name, str_id: StringId) -> Outcome {
    let mut current_name = name;

    while let Some(nesting_id) = current_name.nesting() {
        if let NameRef::Resolved(nesting_name_ref) = graph.names().get(nesting_id).unwrap() {
            if let Some(declaration) = graph.declarations().get(nesting_name_ref.declaration_id())
                && let Some(member) = declaration.members().get(&str_id)
            {
                return Outcome::Resolved(*member);
            }

            current_name = nesting_name_ref.name();
        } else {
            return Outcome::Retry;
        }
    }

    Outcome::Unresolved
}

/// Look for the constant at the top level (member of Object)
fn search_top_level(graph: &Graph, str_id: StringId) -> Outcome {
    let object = graph.declarations().get(&OBJECT_ID).unwrap();

    match object.members().get(&str_id) {
        Some(member_id) => Outcome::Resolved(*member_id),
        None => Outcome::Unresolved,
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
    (
        VecDeque::from(constants.into_iter().map(|(id, _)| id).collect::<Vec<_>>()),
        others,
    )
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

            let declaration = $context
                .graph
                .declarations()
                .get(&DeclarationId::from($declaration_name))
                .expect(&format!("Declaration '{}' not found in graph", $declaration_name));

            let constant = declaration
                .references()
                .iter()
                .filter_map(|r| {
                    let reference = $context
                        .graph
                        .constant_references()
                        .get(r)
                        .expect("Reference should exist");
                    if let NameRef::Resolved(_) = $context
                        .graph
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

    fn assert_members(decl: &Declaration, members: &[&str]) {
        for member in members {
            assert!(
                decl.members().contains_key(&StringId::from(*member)),
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

        let reference = context.graph.constant_references().values().next().unwrap();

        match context.graph.names().get(reference.name_id()) {
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

        let foo = context.graph.declarations().get(&DeclarationId::from("Foo")).unwrap();
        assert_members(foo, &["Bar", "Baz"]);
        assert_owner(foo, "Object");

        let bar = context
            .graph
            .declarations()
            .get(&DeclarationId::from("Foo::Bar"))
            .unwrap();
        assert_members(bar, &[]);
        assert_owner(bar, "Foo");

        let baz = context
            .graph
            .declarations()
            .get(&DeclarationId::from("Foo::Baz"))
            .unwrap();
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

        let foo = context.graph.declarations().get(&DeclarationId::from("Foo")).unwrap();
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

        let foo = context.graph.declarations().get(&DeclarationId::from("Foo")).unwrap();
        assert_members(foo, &[]);
        assert_owner(foo, "Object");

        let bar = context.graph.declarations().get(&DeclarationId::from("Bar")).unwrap();
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

        let foo = context.graph.declarations().get(&DeclarationId::from("Foo")).unwrap();
        assert_members(foo, &[]);
        assert_owner(foo, "Object");

        let bar = context.graph.declarations().get(&DeclarationId::from("Bar")).unwrap();
        assert_members(bar, &["Baz"]);
        assert_owner(bar, "Object");

        let baz = context
            .graph
            .declarations()
            .get(&DeclarationId::from("Bar::Baz"))
            .unwrap();
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
        assert_eq!(1, context.graph.declarations().len());
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

        let mut names = context.graph.names().values().collect::<Vec<_>>();
        assert_eq!(10, names.len());

        names.sort_by_key(|a| name_depth(a, context.graph.names()));

        assert_eq!(
            vec![
                "Top", "Foo", "Qux", "AfterTop", "Bar", "Baz", "Zip", "Zap", "Zop", "Boop"
            ],
            names
                .iter()
                .map(|n| context.graph.strings().get(n.str()).unwrap().as_str())
                .collect::<Vec<_>>()
        );
    }
}
