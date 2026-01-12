use line_index::LineIndex;

use super::normalize_indentation;
use crate::indexing::local_graph::LocalGraph;
use crate::indexing::ruby_indexer::RubyIndexer;
use crate::model::definitions::Definition;
use crate::model::ids::UriId;
use crate::offset::Offset;
use crate::position::Position;

#[cfg(any(test, feature = "test_utils"))]
pub struct LocalGraphTest {
    uri: String,
    source: String,
    graph: LocalGraph,
}

#[cfg(any(test, feature = "test_utils"))]
impl LocalGraphTest {
    #[must_use]
    pub fn new(uri: &str, source: &str) -> Self {
        let uri = uri.to_string();
        let source = normalize_indentation(source);

        let mut indexer = RubyIndexer::new(uri.clone(), &source);
        indexer.index();
        let graph = indexer.local_graph();

        Self { uri, source, graph }
    }

    #[must_use]
    pub fn uri(&self) -> &str {
        &self.uri
    }

    #[must_use]
    pub fn graph(&self) -> &LocalGraph {
        &self.graph
    }

    /// # Panics
    ///
    /// Panics if a definition cannot be found at the given location.
    #[must_use]
    pub fn all_definitions_at<'a>(&'a self, location: &str) -> Vec<&'a Definition> {
        let (uri, offset) = self.parse_location(&format!("{}:{}", self.uri(), location));
        let uri_id = UriId::from(&uri);

        let definitions = self
            .graph()
            .definitions()
            .values()
            .filter(|def| def.uri_id() == &uri_id && def.offset() == &offset)
            .collect::<Vec<_>>();

        assert!(
            !definitions.is_empty(),
            "could not find a definition matching {location}, did you mean one of the following: {:?}",
            {
                let mut offsets = self
                    .graph()
                    .definitions()
                    .values()
                    .map(crate::model::definitions::Definition::offset)
                    .collect::<Vec<_>>();

                offsets.sort_by_key(|a| a.start());

                offsets
                    .iter()
                    .map(|offset| self.offset_to_display_range(offset))
                    .collect::<Vec<_>>()
            }
        );

        definitions
    }

    /// # Panics
    ///
    /// Panics if no definition or multiple definitions are found at the given location.
    #[must_use]
    pub fn definition_at<'a>(&'a self, location: &str) -> &'a Definition {
        let definitions = self.all_definitions_at(location);
        assert!(
            definitions.len() < 2,
            "found more than one definition matching {location}"
        );

        definitions[0]
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
    pub fn parse_location(&self, location: &str) -> (String, Offset) {
        let (uri, start_position, end_position) = Self::parse_location_positions(location);
        let line_index = LineIndex::new(&self.source);

        let start_offset = line_index.offset(start_position).unwrap_or(0.into());
        let end_offset = line_index.offset(end_position).unwrap_or(0.into());

        (uri, Offset::new(start_offset.into(), end_offset.into()))
    }

    #[must_use]
    pub fn offset_to_display_range(&self, offset: &Offset) -> String {
        let line_index = LineIndex::new(&self.source);
        let start = line_index.line_col(offset.start().into());
        let end = line_index.line_col(offset.end().into());
        format!("{}:{}-{}:{}", start.line + 1, start.col + 1, end.line + 1, end.col + 1)
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
                line: start_line - 1,
                col: start_column - 1,
            },
            Position {
                line: end_line - 1,
                col: end_column - 1,
            },
        )
    }

    fn parse_number(value: &str, field: &str, location: &str) -> u32 {
        value
            .parse()
            .unwrap_or_else(|_| panic!("Invalid {field} '{value}' in location {location}"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_locations() {
        let context = LocalGraphTest::new("file://foo.rb", "class Foo; end");

        let (uri, offset) = context.parse_location("file://foo.rb:1:1-1:14");

        assert_eq!(uri, "file://foo.rb");
        assert_eq!(offset, Offset::new(0, 13));
    }
}
