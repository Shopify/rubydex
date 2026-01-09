use line_index::LineIndex;

use super::normalize_indentation;
use crate::indexing::local_graph::LocalGraph;
use crate::indexing::ruby_indexer::RubyIndexer;
use crate::model::graph::Graph;
use crate::offset::Offset;
use crate::position::Position;
use crate::resolution;
use std::collections::HashMap;

#[derive(Default)]
pub struct GraphTest {
    graph: Graph,
    sources: HashMap<String, String>,
}

impl GraphTest {
    #[must_use]
    pub fn new() -> Self {
        Self {
            graph: Graph::new(),
            sources: HashMap::new(),
        }
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

    pub fn index_uri(&mut self, uri: &str, source: &str) {
        let source = normalize_indentation(source);
        let local_index = Self::index_source(uri, &source);
        self.sources.insert(uri.to_string(), source);
        self.graph.update(local_index);
    }

    pub fn delete_uri(&mut self, uri: &str) {
        self.sources.remove(uri);
        self.graph.delete_uri(uri);
    }

    /// Gets the source code for a URI
    #[must_use]
    pub fn get_source(&self, uri: &str) -> Option<&str> {
        self.sources.get(uri).map(String::as_str)
    }

    pub fn resolve(&mut self) {
        resolution::resolve_all(&mut self.graph);
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

    fn line_index_for(&self, uri: &str) -> LineIndex {
        let source = self
            .get_source(uri)
            .unwrap_or_else(|| panic!("Source not found for URI: {uri}"));
        LineIndex::new(source)
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
