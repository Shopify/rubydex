use crate::model::{
    graph::Graph,
    ids::{DeclarationId, NameId, ReferenceId, StringId},
    name::{Name, NameRef},
};

/// Attempts to resolve all constant references stored in the graph
///
/// # Panics
///
/// This function will panic if the reference for the given ID does not exist in the graph
pub fn resolve_all_constants(graph: &mut Graph) {
    let filter_map: Vec<ReferenceId> = graph.constant_references().keys().copied().collect();

    for id in filter_map {
        let name_id = *graph
            .constant_references()
            .get(&id)
            .expect("Reference should exist")
            .name_id();

        if let Some(declaration_id) = resolve_constant(graph, name_id) {
            graph.record_resolved_reference(id, declaration_id);
        }
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
    // Fall back to top level (member of Object). Note: temporary while we're missing RBS indexing
    let name = graph.strings().get(&str_id)?;
    let id = DeclarationId::from(name);
    graph.declarations().get(&id).map(|_| id)
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

        resolve_all_constants(&mut context.graph);
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

        resolve_all_constants(&mut context.graph);
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
        resolve_all_constants(&mut context.graph);
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
        resolve_all_constants(&mut context.graph);
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

        resolve_all_constants(&mut context.graph);
        let reference = context.graph.constant_references().values().next().unwrap();

        match context.graph.names().get(reference.name_id()) {
            Some(NameRef::Unresolved(_)) => {}
            _ => panic!("expected unresolved constant reference"),
        }
    }
}
