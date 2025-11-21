use crate::model::{
    declaration::Declaration,
    definitions::Definition,
    graph::{Graph, OBJECT_ID},
    ids::{DeclarationId, DefinitionId, NameId, ReferenceId, StringId},
    name::{Name, NameRef},
};

pub enum Unit {
    Definition(DefinitionId),
    Reference(ReferenceId),
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

    let (constant_ids, other_ids) = graph.sorted_resolution_units();

    // First, handle all constants, ensuring that we create declarations, members, owners and resolve any name
    // references that are necessary along the way
    for unit_id in constant_ids {
        match unit_id {
            Unit::Definition(id) => match graph.definitions().get(&id).unwrap() {
                Definition::Class(class) => {
                    handle_namespace_declaration(graph, *class.name_id(), id);
                }
                Definition::Module(module) => {
                    handle_namespace_declaration(graph, *module.name_id(), id);
                }
                _ => panic!("Expected module or class definition"),
            },
            Unit::Reference(id) => {
                let constant_ref = graph.constant_references().get(&id).unwrap();
                let name_id = *constant_ref.name_id();

                if let Some(declaration_id) = resolve_constant(graph, name_id) {
                    graph.record_resolved_name(name_id, declaration_id);
                    graph.record_resolved_reference(id, declaration_id);
                }
            }
        }
    }

    // Handle other definitions that don't require resolution, but need to have their declarations and membership
    // created
    for id in other_ids {
        let definition = graph.definitions().get(&id).unwrap();
        let owner_id = *definition
            .owner_id()
            .and_then(|oid| graph.definitions_to_declarations().get(&oid))
            .unwrap_or(&OBJECT_ID);

        let owner = graph.declarations().get(&owner_id).unwrap();

        let str_id = *match definition {
            Definition::Constant(it) => it.str_id(),
            Definition::Method(it) => it.str_id(),
            Definition::AttrAccessor(it) => it.str_id(),
            Definition::AttrReader(it) => it.str_id(),
            Definition::AttrWriter(it) => it.str_id(),
            Definition::GlobalVariable(it) => it.str_id(),
            Definition::InstanceVariable(it) => it.str_id(),
            Definition::ClassVariable(it) => it.str_id(),
            _ => panic!("Unexpected definition type. This shouldn't happen"),
        };

        let name_str = graph.strings().get(&str_id).unwrap();
        let declaration_id = graph.add_declaration(format!("{}::{name_str}", owner.name()), id, owner_id);
        graph.add_member(&owner_id, declaration_id, str_id);
    }
}

// Handles the resolution of the namespace name, the creation of the declaration and membership
fn handle_namespace_declaration(graph: &mut Graph, name_id: NameId, definition_id: DefinitionId) {
    let name_ref = graph.names().get(&name_id).unwrap();
    let str_id = *name_ref.str();

    // The name of the declaration is determined by the name of its owner, which may or may not require resolution
    // depending on whether the name has a parent scope
    if let Some(owner_id) = name_owner_id(graph, name_id) {
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
    }
}

// Returns the owner declaration ID for a given name. If the name is simple and has no parent scope, then the owner is
// either the nesting or Object. If the name has a parent scope, we attempt to resolve the reference and that should be
// the name's owner
fn name_owner_id(graph: &mut Graph, name_id: NameId) -> Option<DeclarationId> {
    let name_ref = graph.names().get(&name_id)?;

    if let Some(parent_scope) = name_ref.parent_scope() {
        // If we have `A::B`, the owner of `B` is whatever `A` resolves to
        resolve_constant(graph, *parent_scope)
    } else if let Some(nesting_id) = name_ref.nesting() {
        let nesting_name_ref = graph.names().get(nesting_id).unwrap();

        match nesting_name_ref {
            NameRef::Resolved(resolved) => Some(*resolved.declaration_id()),
            NameRef::Unresolved(_) => {
                // This case should only happen if we cannot resolve the nesting name. For example, if the nesting
                // name is `A::B` and we have not been able to resolve `A`. In every other case, we should have
                // already processed the nesting `NameRef` and it should already be associated to a `DeclarationId`
                None
            }
        }
    } else {
        // Any constants at the top level are owned by Object
        Some(*OBJECT_ID)
    }
}

/// Attempts to resolve a constant reference against the graph. Returns the fully qualified declaration ID that the
/// reference is related to or `None`. This method mutates the graph to remember which constants have already been
/// resolved
fn resolve_constant(graph: &mut Graph, name_id: NameId) -> Option<DeclarationId> {
    let name_ref = graph.names().get(&name_id)?;

    match name_ref {
        NameRef::Unresolved(name) => {
            // If there's a parent scope for this constant, it means it's a constant path. We must first resolve the
            // outer most parent, so that we can then continue resolution from there, recording the results along the
            // way
            if let Some(parent_scope_id) = name.parent_scope() {
                let parent_scope = graph.names().get(parent_scope_id)?;

                let declaration_id = match parent_scope {
                    NameRef::Resolved(name) => *name.declaration_id(),
                    NameRef::Unresolved(name) => run_resolution(graph, name)?,
                };

                let declaration = graph.declarations().get(&declaration_id)?;

                if let Some(member_id) = declaration.members().get(name.str()).copied() {
                    graph.record_resolved_name(name_id, member_id);
                    return Some(member_id);
                }

                return None;
            }

            // Otherwise, it's a simple constant read and we can resolve it directly
            let declaration_id = run_resolution(graph, name)?;
            graph.record_resolved_name(name_id, declaration_id);
            Some(declaration_id)
        }
        NameRef::Resolved(resolved) => Some(*resolved.declaration_id()),
    }
}

fn run_resolution(graph: &Graph, name: &Name) -> Option<DeclarationId> {
    let str_id = *name.str();

    // NOTE: these if statements are collapsed because of clippy, but they need to be separated only inheritance
    // resolution is added
    if name.nesting().is_some()
        && let Some(id) = search_lexical_scopes(graph, name, str_id)
    {
        return Some(id);

        // Search inheritance chain
    }

    // If it's a top level reference starting with `::` or if we didn't find the constant anywhere else, the
    // fallback is the top level
    search_top_level(graph, str_id)
}

/// Look for the constant in the lexical scopes that are a part of its nesting
fn search_lexical_scopes(graph: &Graph, name: &Name, str_id: StringId) -> Option<DeclarationId> {
    let mut current_name = name;

    while let Some(nesting_id) = current_name.nesting() {
        let nesting_name_ref = graph.names().get(nesting_id).unwrap();
        let (nesting_name, declaration_id) = match nesting_name_ref {
            NameRef::Resolved(resolved) => (resolved.name(), *resolved.declaration_id()),
            NameRef::Unresolved(name) => (&**name, run_resolution(graph, name)?),
        };

        if let Some(declaration) = graph.declarations().get(&declaration_id)
            && let Some(member) = declaration.members().get(&str_id)
        {
            return Some(*member);
        }

        current_name = nesting_name;
    }

    None
}

/// Look for the constant at the top level (member of Object)
fn search_top_level(graph: &Graph, str_id: StringId) -> Option<DeclarationId> {
    graph
        .declarations()
        .get(&OBJECT_ID)
        .and_then(|decl| decl.members().get(&str_id).copied())
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

        // TODO: temporarily broken until we make constant definitions hold a name!!
        // assert_constant_reference_to!(context, "Foo::CONST", "file:///bar.rb:4:4-4:9");
        // assert_constant_reference_to!(context, "Foo::CONST", "file:///bar.rb:5:9-5:14");
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
}
