use std::sync::Arc;

use crate::{
    indexing::nesting_stack::Nesting,
    model::{
        graph::Graph,
        ids::{DeclarationId, NameId, ReferenceId},
        references::ConstantReference,
    },
};

/// Attempts to resolve all constant references stored in the graph
pub fn resolve_all_constants(graph: &mut Graph) {
    let filter_map: Vec<ReferenceId> = graph
        .constant_references()
        .iter()
        .filter_map(|(id, reference)| match reference {
            ConstantReference::Unresolved(_) => Some(*id),
            ConstantReference::Resolved(_) => None,
        })
        .collect();

    for id in filter_map {
        resolve_constant(graph, id);
    }
}

/// Attempts to resolve a constant reference against the graph. Returns the fully qualified declaration ID that the
/// reference is related to or `None`. This method mutates the graph to remember which constants have already been
/// resolved
fn resolve_constant(graph: &mut Graph, id: ReferenceId) -> Option<DeclarationId> {
    let reference = graph.constant_references().get(&id)?;

    match reference {
        ConstantReference::Unresolved(constant) => {
            // If there's a parent scope for this constant, it means it's a constant path. We must first resolve the
            // outer most parent, so that we can then continue resolution from there, recording the results along the
            // way
            if let Some(parent_scope_id) = constant.parent_scope_id() {
                let parent_scope = graph.constant_references().get(parent_scope_id)?;
                let declaration_id = run_resolution(graph, parent_scope)?;
                let declaration = graph.declarations().get(&declaration_id)?;

                if let Some(member_id) = declaration.members().get(constant.name_id()).copied() {
                    graph.record_resolved_constant(id, member_id);
                    return Some(member_id);
                }

                return None;
            }

            // Otherwise, it's a simple constant read and we can resolve it directly
            let declaration_id = run_resolution(graph, reference)?;
            graph.record_resolved_constant(id, declaration_id);
            Some(declaration_id)
        }
        ConstantReference::Resolved(resolved) => Some(resolved.declaration_id()),
    }
}

fn run_resolution(graph: &Graph, reference: &ConstantReference) -> Option<DeclarationId> {
    match reference {
        ConstantReference::Unresolved(constant) => {
            let name_id = constant.name_id();

            // NOTE: these if statements are collapsed because of clippy, but they need to be separated only inheritance
            // resolution is added
            if let Some(nesting) = constant.nesting()
                && let Some(id) = search_lexical_scopes(graph, nesting, *name_id)
            {
                return Some(id);

                // Search inheritance chain
            }

            // If it's a top level reference starting with `::` or if we didn't find the constant anywhere else, the
            // fallback is the top level
            search_top_level(graph, *name_id)
        }
        ConstantReference::Resolved(resolved) => Some(resolved.declaration_id()),
    }
}

/// Look for the constant in the lexical scopes that are a part of its nesting
fn search_lexical_scopes(graph: &Graph, nesting: &Arc<Nesting>, name_id: NameId) -> Option<DeclarationId> {
    let mut current_nesting = Some(nesting.as_ref());

    while let Some(nesting) = current_nesting {
        let declaration_id = nesting.declaration_id();

        if let Some(declaration) = graph.declarations().get(declaration_id)
            && let Some(member) = declaration.members().get(&name_id)
        {
            return Some(*member);
        }
        current_nesting = nesting.parent().as_ref().map(std::convert::AsRef::as_ref);
    }

    None
}

/// Look for the constant at the top level (member of Object)
fn search_top_level(graph: &Graph, name_id: NameId) -> Option<DeclarationId> {
    // Note: this code is temporary. Once we have RBS indexing, we can simply enter the graph by looking
    // up `Object` and then we search its members for the top level constant
    let name = graph.names().get(&name_id)?;
    Some(DeclarationId::from(name))
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
                    if let ConstantReference::Resolved(c) = $context.graph.constant_references().get(r).unwrap() {
                        Some(c)
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

    /// Asserts that an unresolved constant reference has specific properties
    ///
    /// The `$nesting` parameter should be:
    /// - `None` for top-level references like `::Foo`
    /// - `Some("Foo::Bar")` for references within a specific lexical scope
    ///
    /// Location format: "uri:start_line:start_column-end_line:end_column"
    /// Example: `<file:///foo.rb:3:0-3:5>`
    macro_rules! assert_unresolved_constant {
        ($context:expr, $name:expr, $nesting:expr, $location:expr) => {
            let (uri, start, end) = $context.parse_location($location);
            let expected_nesting: Option<&str> = $nesting;

            let constant = $context
                .graph
                .constant_references()
                .values()
                .filter_map(|r| {
                    if let ConstantReference::Unresolved(c) = r {
                        Some(c)
                    } else {
                        None
                    }
                })
                .find(|c| {
                    let name_matches = $context.graph.names().get(c.name_id()) == Some(&String::from($name));
                    let nesting_matches = match (c.nesting(), expected_nesting) {
                        (None, None) => true,
                        (Some(nesting), Some(expected_decl_id)) => {
                            nesting.declaration_id() == &DeclarationId::from(expected_decl_id)
                        }
                        _ => false,
                    };
                    name_matches && nesting_matches
                })
                .expect(&format!(
                    "{} with nesting {:?} is not found in unresolved references",
                    $name, expected_nesting
                ));

            if constant.uri_id() != UriId::from(&uri) {
                panic!(
                    "URI mismatch for unresolved constant '{}'\n  actual:   {:?}\n  expected: {}",
                    $name,
                    constant.uri_id(),
                    uri
                );
            }

            $context.assert_offset_matches(
                &uri,
                constant.offset(),
                start,
                end,
                &format!("unresolved constant '{}'", $name),
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

        resolve_all_constants(&mut context.graph);
        assert_unresolved_constant!(context, "Foo", None, "file:///foo.rb:0:0-0:3");
    }
}
