use super::normalize_indentation;
#[cfg(test)]
use crate::diagnostic::Rule;
use crate::indexing::local_graph::LocalGraph;
use crate::indexing::ruby_indexer::RubyIndexer;
use crate::model::graph::{Graph, IndexResult};
use crate::resolution::Resolver;

#[derive(Default)]
pub struct GraphTest {
    graph: Graph,
}

impl GraphTest {
    #[must_use]
    pub fn new() -> Self {
        Self { graph: Graph::new() }
    }

    #[must_use]
    pub fn graph(&self) -> &Graph {
        &self.graph
    }

    #[must_use]
    fn index_source(uri: &str, source: &str) -> LocalGraph {
        let mut indexer = RubyIndexer::new(uri.to_string(), source);
        indexer.index();
        indexer.local_graph()
    }

    pub fn index_uri(&mut self, uri: &str, source: &str) -> Option<IndexResult> {
        let source = normalize_indentation(source);
        let local_index = Self::index_source(uri, &source);
        self.graph.update(local_index)
    }

    pub fn delete_uri(&mut self, uri: &str) -> Option<IndexResult> {
        self.graph.delete_uri(uri)
    }

    pub fn resolve(&mut self) {
        let mut resolver = Resolver::new(&mut self.graph);
        resolver.resolve_all();
    }

    pub fn resolve_incremental(&mut self, result: &IndexResult) {
        let mut resolver = Resolver::new(&mut self.graph);
        resolver.resolve_incremental(result);
    }

    /// # Panics
    ///
    /// Panics if a diagnostic points to an invalid document
    #[cfg(test)]
    #[must_use]
    pub fn format_diagnostics(&self, ignore_rules: &[Rule]) -> Vec<String> {
        let mut diagnostics: Vec<_> = self
            .graph()
            .all_diagnostics()
            .into_iter()
            .filter(|d| !ignore_rules.contains(d.rule()))
            .collect();

        diagnostics.sort_by_key(|d| {
            let uri = self.graph().documents().get(d.uri_id()).unwrap().uri();
            (uri, d.offset())
        });

        diagnostics
            .iter()
            .map(|d| {
                let document = self.graph().documents().get(d.uri_id()).unwrap();
                d.formatted(document)
            })
            .collect()
    }
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_constant_alias_target_eq {
    ($context:expr, $alias_name:expr, $target_name:expr) => {{
        let decl_id = $crate::model::ids::DeclarationId::from($alias_name);
        let target = $context
            .graph()
            .alias_targets(&decl_id)
            .and_then(|t| t.first().copied());
        assert_eq!(
            target,
            Some($crate::model::ids::DeclarationId::from($target_name)),
            "Expected alias '{}' to have primary target '{}'",
            $alias_name,
            $target_name
        );
    }};
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_no_constant_alias_target {
    ($context:expr, $alias_name:expr) => {{
        let decl_id = $crate::model::ids::DeclarationId::from($alias_name);
        let targets = $context.graph().alias_targets(&decl_id).unwrap_or_default();
        assert!(
            targets.is_empty(),
            "Expected no alias target for '{}', but found {:?}",
            $alias_name,
            targets
        );
    }};
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_alias_targets_contain {
    ($context:expr, $alias_name:expr, $($target_name:expr),+ $(,)?) => {{
        let decl_id = $crate::model::ids::DeclarationId::from($alias_name);
        let targets = $context.graph().alias_targets(&decl_id).unwrap_or_default();
        $(
            let expected_id = $crate::model::ids::DeclarationId::from($target_name);
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
#[cfg(test)]
#[macro_export]
macro_rules! assert_constant_reference_to {
    ($context:expr, $declaration_name:expr, $location:expr) => {
        let mut all_references = $context
            .graph()
            .constant_references()
            .values()
            .map(|reference| {
                (
                    reference,
                    format!(
                        "{}:{}",
                        $context.graph().documents().get(&reference.uri_id()).unwrap().uri(),
                        reference
                            .offset()
                            .to_display_range($context.graph().documents().get(&reference.uri_id()).unwrap())
                    ),
                )
            })
            .collect::<Vec<_>>();

        all_references.sort_by_key(|(_, reference_location)| reference_location.clone());

        let reference_at_location = all_references
            .iter()
            .find(|(_, reference_location)| reference_location == $location)
            .map(|(reference, _)| reference)
            .expect(&format!(
                "No constant reference at `{}`, found references at {:?}",
                $location,
                all_references
                    .iter()
                    .map(|(_reference, reference_location)| reference_location)
                    .collect::<Vec<_>>()
            ));

        let reference_name = $context.graph().names().get(reference_at_location.name_id()).unwrap();
        let NameRef::Resolved(resolved_name) = reference_name else {
            panic!("Reference to found at `{}` is unresolved", $location);
        };

        let resolved_name_name = $context
            .graph()
            .declarations()
            .get(resolved_name.declaration_id())
            .unwrap()
            .name();
        assert_eq!(
            resolved_name_name, $declaration_name,
            "Expected reference at `{}` to be resolved to `{}`, but got `{}`",
            $location, $declaration_name, resolved_name_name
        );
    };
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_ancestors_eq {
    ($context:expr, $name:expr, $expected:expr) => {
        let declaration = $context
            .graph()
            .declarations()
            .get(&$crate::model::ids::DeclarationId::from($name))
            .unwrap();

        match declaration.as_namespace().unwrap().ancestors() {
            $crate::model::declaration::Ancestors::Cyclic(ancestors)
            | $crate::model::declaration::Ancestors::Complete(ancestors) => {
                assert_eq!(
                    $expected
                        .iter()
                        .map(|n| {
                            $crate::model::declaration::Ancestor::Complete($crate::model::ids::DeclarationId::from(*n))
                        })
                        .collect::<Vec<_>>(),
                    ancestors,
                    "Incorrect ancestors {}",
                    ancestors
                        .iter()
                        .filter_map(|id| {
                            if let $crate::model::declaration::Ancestor::Complete(id) = id {
                                let name = { $context.graph().declarations().get(id).unwrap().name().to_string() };
                                Some(name)
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                );
            }
            $crate::model::declaration::Ancestors::Partial(_) => {
                panic!("Expected ancestors to be resolved for {}", declaration.name());
            }
        }
    };
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_descendants {
    ($context:expr, $parent:expr, $descendants:expr) => {
        let parent = $context
            .graph()
            .declarations()
            .get(&$crate::model::ids::DeclarationId::from($parent))
            .unwrap();
        let actual = match parent {
            $crate::model::declaration::Declaration::Namespace($crate::model::declaration::Namespace::Class(class)) => {
                class.descendants().iter().cloned().collect::<Vec<_>>()
            }
            $crate::model::declaration::Declaration::Namespace($crate::model::declaration::Namespace::Module(
                module,
            )) => module.descendants().iter().cloned().collect::<Vec<_>>(),
            $crate::model::declaration::Declaration::Namespace(
                $crate::model::declaration::Namespace::SingletonClass(singleton),
            ) => singleton.descendants().iter().cloned().collect::<Vec<_>>(),
            _ => panic!("Tried to get descendants for a declaration that isn't a namespace"),
        };

        for descendant in &$descendants {
            let descendant_id = $crate::model::ids::DeclarationId::from(*descendant);

            assert!(
                actual.contains(&descendant_id),
                "Expected '{}' to be a descendant of '{}'",
                $context.graph().declarations().get(&descendant_id).unwrap().name(),
                parent.name()
            );
        }
    };
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_members_eq {
    ($context:expr, $declaration_id:expr, $expected_members:expr) => {
        let mut actual_members = $context
            .graph()
            .declarations()
            .get(&$crate::model::ids::DeclarationId::from($declaration_id))
            .unwrap()
            .as_namespace()
            .unwrap()
            .members()
            .iter()
            .map(|(str_id, _)| $context.graph().strings().get(str_id).unwrap().as_str())
            .collect::<Vec<_>>();

        actual_members.sort();

        assert_eq!($expected_members, actual_members.as_slice());
    };
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_no_members {
    ($context:expr, $declaration_id:expr) => {
        assert_members_eq!($context, $declaration_id, [] as [&str; 0]);
    };
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_owner_eq {
    ($context:expr, $declaration_id:expr, $expected_owner_name:expr) => {
        let actual_owner_id = $context
            .graph()
            .declarations()
            .get(&$crate::model::ids::DeclarationId::from($declaration_id))
            .unwrap()
            .owner_id();

        let actual_owner_name = $context.graph().declarations().get(actual_owner_id).unwrap().name();

        assert_eq!($expected_owner_name, actual_owner_name);
    };
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_singleton_class_eq {
    ($context:expr, $declaration_id:expr, $expected_singleton_class_name:expr) => {
        let declaration = $context
            .graph()
            .declarations()
            .get(&$crate::model::ids::DeclarationId::from($declaration_id))
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

#[cfg(test)]
#[macro_export]
macro_rules! assert_instance_variables_eq {
    ($context:expr, $declaration_id:expr, $expected_instance_variables:expr) => {
        let mut actual_instance_variables = $context
            .graph()
            .declarations()
            .get(&$crate::model::ids::DeclarationId::from($declaration_id))
            .unwrap()
            .as_namespace()
            .unwrap()
            .members()
            .iter()
            .filter_map(
                |(str_id, member_id)| match $context.graph().declarations().get(member_id) {
                    Some($crate::model::declaration::Declaration::InstanceVariable(_)) => {
                        Some($context.graph().strings().get(str_id).unwrap().as_str())
                    }
                    _ => None,
                },
            )
            .collect::<Vec<_>>();

        actual_instance_variables.sort();

        assert_eq!($expected_instance_variables, actual_instance_variables.as_slice());
    };
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_diagnostics_eq {
    ($context:expr, $expected_diagnostics:expr) => {{
        assert_eq!($expected_diagnostics, $context.format_diagnostics(&[]).as_slice());
    }};
    ($context:expr, $expected_diagnostics:expr, $ignore_rules:expr) => {{
        assert_eq!($expected_diagnostics, $context.format_diagnostics($ignore_rules));
    }};
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_no_diagnostics {
    ($context:expr) => {{
        let diagnostics = $context.format_diagnostics(&[]);
        assert!(diagnostics.is_empty(), "expected no diagnostics, got {:?}", diagnostics);
    }};
    ($context:expr, $ignore_rules:expr) => {{
        let diagnostics = $context.format_diagnostics($ignore_rules);
        assert!(diagnostics.is_empty(), "expected no diagnostics, got {:?}", diagnostics);
    }};
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_index_uri_with_single_line() {
        let mut context = GraphTest::new();

        context.index_uri("file://method.rb", "class Foo; end");
        context.resolve();

        let foo_defs = context.graph.get("Foo").unwrap();
        assert_eq!(foo_defs.len(), 1);
        assert_eq!(foo_defs[0].offset().start(), 0);
        assert_eq!(foo_defs[0].offset().end(), 14);
    }

    #[test]
    fn test_index_uri_with_multiple_lines() {
        let mut context = GraphTest::new();

        context.index_uri("file://method.rb", {
            "
            class Foo
              class Bar; end
            end
            "
        });

        context.resolve();

        let foo_defs = context.graph.get("Foo").unwrap();
        assert_eq!(foo_defs.len(), 1);
        assert_eq!(foo_defs[0].offset().start(), 0);
        assert_eq!(foo_defs[0].offset().end(), 30);

        let bar_defs = context.graph.get("Foo::Bar").unwrap();
        assert_eq!(bar_defs.len(), 1);
        assert_eq!(bar_defs[0].offset().start(), 12);
        assert_eq!(bar_defs[0].offset().end(), 26);
    }

    #[test]
    fn test_index_uri_with_new_lines() {
        let mut context = GraphTest::new();

        context.index_uri("file://method.rb", "\n\nclass Foo; end");
        context.resolve();

        let foo_defs = context.graph.get("Foo").unwrap();
        assert_eq!(foo_defs.len(), 1);
        assert_eq!(foo_defs[0].offset().start(), 2);
        assert_eq!(foo_defs[0].offset().end(), 16);
    }
}
