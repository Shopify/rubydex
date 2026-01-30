use line_index::LineIndex;

use super::normalize_indentation;
#[cfg(test)]
use crate::diagnostic::Rule;
use crate::indexing::local_graph::LocalGraph;
use crate::indexing::ruby_indexer::RubyIndexer;
use crate::model::graph::Graph;
use crate::model::ids::UriId;
use crate::offset::Offset;
use crate::position::Position;
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

    pub fn graph_mut(&mut self) -> &mut Graph {
        &mut self.graph
    }

    #[must_use]
    fn index_source(uri: &str, source: &str) -> LocalGraph {
        let mut indexer = RubyIndexer::new(uri.to_string(), source);
        indexer.index();
        indexer.local_graph()
    }

    pub fn index_uri(&mut self, uri: &str, source: &str) {
        let source = normalize_indentation(source);
        let local_index = Self::index_source(uri, &source);
        self.graph.update(local_index);
    }

    pub fn delete_uri(&mut self, uri: &str) {
        self.graph.delete_uri(uri);
    }

    pub fn resolve(&mut self) {
        let mut resolver = Resolver::new(&mut self.graph);
        resolver.resolve_all();
    }

    /// Parses a location string like `<file:///foo.rb:3:0-3:5>` into `(uri, start_offset, end_offset)`
    ///
    /// Format: uri:start_line:start_column-end_line:end_column
    /// Line and column numbers are 0-indexed
    ///
    /// # Panics
    ///
    /// Panics if the location format is invalid, the URI has no source, or the positions are invalid.
    #[must_use]
    pub fn parse_location(&self, location: &str) -> (String, u32, u32) {
        let (uri, start_position, end_position) = Self::parse_location_positions(location);
        let line_index = self.line_index_for(uri.as_str());

        (
            uri,
            line_index
                .offset(start_position)
                .unwrap_or_else(|| panic!("Invalid start position {}:{}", start_position.line, start_position.col))
                .into(),
            line_index
                .offset(end_position)
                .unwrap_or_else(|| panic!("Invalid end position {}:{}", end_position.line, end_position.col))
                .into(),
        )
    }

    /// Asserts that the given offset matches the expected offset, providing clear error messages
    /// with line:column positions when they don't match
    ///
    /// # Panics
    ///
    /// Panics if the source is not found for the URI, byte offsets are invalid, or if the actual
    /// offset doesn't match the expected offset.
    pub fn assert_offset_matches(
        &self,
        uri: &str,
        actual_offset: &Offset,
        expected_start: u32,
        expected_end: u32,
        context_message: &str,
        location: &str,
    ) {
        let line_index = self.line_index_for(uri);

        if actual_offset.start() == expected_start && actual_offset.end() == expected_end {
            return;
        }

        let actual_start_pos = line_index.line_col(actual_offset.start().into());
        let actual_end_pos = line_index.line_col(actual_offset.end().into());
        let expected_start_pos = line_index.line_col(expected_start.into());
        let expected_end_pos = line_index.line_col(expected_end.into());

        assert!(
            actual_offset.start() == expected_start,
            "Start position mismatch for {} at {}\n  actual:   {}\n  expected: {}",
            context_message,
            location,
            Self::format_position(actual_start_pos),
            Self::format_position(expected_start_pos)
        );

        assert!(
            actual_offset.end() == expected_end,
            "End position mismatch for {} at {}\n  actual:   {}\n  expected: {}",
            context_message,
            location,
            Self::format_position(actual_end_pos),
            Self::format_position(expected_end_pos)
        );
    }

    fn line_index_for(&self, uri: &str) -> &LineIndex {
        let uri_id = UriId::from(uri);
        let document = self.graph.documents().get(&uri_id).unwrap();
        document.line_index()
    }

    fn parse_location_positions(location: &str) -> (String, Position, Position) {
        let trimmed = location.trim().trim_start_matches('<').trim_end_matches('>');

        let (start_part, end_part) = trimmed.rsplit_once('-').unwrap_or_else(|| {
            panic!("Invalid location format: {location} (expected uri:start_line:start_column-end_line:end_column)")
        });

        let (start_prefix, start_column_str) = start_part
            .rsplit_once(':')
            .unwrap_or_else(|| panic!("Invalid location format: missing start column in {location}"));
        let (uri, start_line_str) = start_prefix
            .rsplit_once(':')
            .unwrap_or_else(|| panic!("Invalid location format: missing start line in {location}"));

        let (end_line_str, end_column_str) = end_part
            .split_once(':')
            .unwrap_or_else(|| panic!("Invalid location format: missing end line or column in {location}"));

        let start_line = Self::parse_number(start_line_str, "start line", location);
        let start_column = Self::parse_number(start_column_str, "start column", location);
        let end_line = Self::parse_number(end_line_str, "end line", location);
        let end_column = Self::parse_number(end_column_str, "end column", location);

        (
            uri.to_string(),
            Position {
                line: start_line,
                col: start_column,
            },
            Position {
                line: end_line,
                col: end_column,
            },
        )
    }

    fn parse_number(value: &str, field: &str, location: &str) -> u32 {
        value
            .parse()
            .unwrap_or_else(|_| panic!("Invalid {field} '{value}' in location {location}"))
    }

    fn format_position(position: Position) -> String {
        format!("line {}, column {}", position.line, position.col)
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
        let (uri, start, end) = $context.parse_location($location);

        let declaration = $context
            .graph()
            .declarations()
            .get(&$crate::model::ids::DeclarationId::from($declaration_name))
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
                if $context.graph().resolved_names().contains_key(reference.name_id()) {
                    Some(reference)
                } else {
                    None
                }
            })
            .find(|c| c.uri_id() == $crate::model::ids::UriId::from(&uri) && c.offset().start() == start)
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
#[macro_export]
macro_rules! assert_constant_reference_unresolved {
    ($context:expr, $location:expr) => {
        let (uri, start, end) = $context.parse_location($location);

        let reference = $context
            .graph()
            .constant_references()
            .values()
            .find(|r| {
                r.uri_id() == $crate::model::ids::UriId::from(&uri) && r.offset().start() == start
            })
            .expect(&format!("No constant reference found at {}", $location));

        assert!(
            !$context
                .graph()
                .resolved_names()
                .contains_key(reference.name_id()),
            "Expected reference at {} to be unresolved",
            $location
        );

        $context.assert_offset_matches(&uri, reference.offset(), start, end, "unresolved reference", $location);
    };
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
