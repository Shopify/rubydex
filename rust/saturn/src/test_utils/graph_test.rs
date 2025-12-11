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
    pub graph: Graph,
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
