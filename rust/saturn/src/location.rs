use std::{fmt::Display, fs};

use line_index::LineIndex;
use url::Url;

use crate::{offset::Offset, position::Position};

/// Represents a location in a source file that is meant to be displayed to the end user.
///
/// Format: `schema://path:start_line:start_column-end_line:end_column`
/// Line and column numbers are 1-indexed so clicking on the location will open the file at the correct line and column.
pub struct Location {
    uri: String,
    line_start: u32,
    line_end: u32,
    column_start: u32,
    column_end: u32,
}

impl Location {
    #[must_use]
    pub fn new(uri: String, line_start: u32, line_end: u32, column_start: u32, column_end: u32) -> Self {
        Self {
            uri,
            line_start,
            line_end,
            column_start,
            column_end,
        }
    }

    // Parsing

    /// Parses a location string like `file://foo.rb:3:1-3:6` into a `Location` instance.
    ///
    /// # Panics
    ///
    /// Panics if the location string format is invalid.
    #[must_use]
    pub fn from_string(location_string: &str) -> Self {
        let (scheme, rest) = location_string
            .split_once("://")
            .unwrap_or_else(|| panic!("Invalid location: missing `://` in {location_string}"));

        let (path, rest) = rest
            .split_once(':')
            .unwrap_or_else(|| panic!("Invalid location: missing `:` in {location_string}"));

        let (start_position, end_position) = rest
            .rsplit_once('-')
            .unwrap_or_else(|| panic!("Invalid location: missing `-` in {location_string}"));

        let (start_line_str, start_column_str) = start_position
            .rsplit_once(':')
            .unwrap_or_else(|| panic!("Invalid location: missing `:` in {location_string}"));

        let (end_line_str, end_column_str) = end_position
            .rsplit_once(':')
            .unwrap_or_else(|| panic!("Invalid location: missing `:` in {location_string}"));

        let start_line = Self::parse_number(start_line_str, "start line", location_string);
        let start_column = Self::parse_number(start_column_str, "start column", location_string);
        let end_line = Self::parse_number(end_line_str, "end line", location_string);
        let end_column = Self::parse_number(end_column_str, "end column", location_string);

        Self::new(
            format!("{scheme}://{path}"),
            start_line,
            end_line,
            start_column,
            end_column,
        )
    }

    fn parse_number(value: &str, field: &str, location_string: &str) -> u32 {
        value
            .parse()
            .unwrap_or_else(|_| panic!("Invalid location: missing {field} in {location_string}"))
    }

    // Offset conversion

    #[must_use]
    pub fn from_source_and_offset(source: &str, uri: String, offset: &Offset) -> Self {
        let line_index = LineIndex::new(source);
        let start_pos = line_index.line_col(offset.start().into());
        let end_pos = line_index.line_col(offset.end().into());

        Self::new(
            uri,
            start_pos.line + 1,
            end_pos.line + 1,
            start_pos.col + 1,
            end_pos.col + 1,
        )
    }

    /// Creates a location from a URI and an offset.
    ///
    /// # Panics
    ///
    /// - If the URI cannot be converted to a file path.
    /// - If the file cannot be read.
    #[must_use]
    pub fn from_uri_and_offset(uri: String, offset: &Offset) -> Self {
        let path = Url::parse(&uri)
            .ok()
            .and_then(|url| url.to_file_path().ok())
            .map_or_else(
                || panic!("Failed to convert URI to file path: {uri}"),
                |p| p.to_string_lossy().into_owned(),
            );

        let source = fs::read_to_string(&path).unwrap_or_else(|_| panic!("Failed to read file at path {path}"));

        Self::from_source_and_offset(&source, uri, offset)
    }

    #[must_use]
    pub fn to_offset(&self, source: &str) -> Offset {
        let line_index = LineIndex::new(source);

        let start_position = Position {
            line: self.line_start() - 1,
            col: self.column_start() - 1,
        };
        let end_position = Position {
            line: self.line_end() - 1,
            col: self.column_end() - 1,
        };

        let start_offset = line_index.offset(start_position).unwrap_or(0.into());
        let end_offset = line_index.offset(end_position).unwrap_or(0.into());

        Offset::new(start_offset.into(), end_offset.into())
    }

    // Accessors

    #[must_use]
    pub fn uri(&self) -> &str {
        &self.uri
    }

    #[must_use]
    pub fn line_start(&self) -> u32 {
        self.line_start
    }

    #[must_use]
    pub fn line_end(&self) -> u32 {
        self.line_end
    }

    #[must_use]
    pub fn column_start(&self) -> u32 {
        self.column_start
    }

    #[must_use]
    pub fn column_end(&self) -> u32 {
        self.column_end
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}-{}:{}",
            self.uri, self.line_start, self.column_start, self.line_end, self.column_end
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn location_from_string() {
        let location = Location::from_string("file://foo.rb:1:1-1:14");

        assert_eq!(location.uri, "file://foo.rb");
        assert_eq!(location.line_start, 1);
        assert_eq!(location.column_start, 1);
        assert_eq!(location.line_end, 1);
        assert_eq!(location.column_end, 14);
    }

    #[test]
    #[should_panic(expected = "Invalid location: missing `://` in foo.rb:1:1-1:14")]
    fn location_from_string_missing_scheme() {
        let _location = Location::from_string("foo.rb:1:1-1:14");
    }

    #[test]
    #[should_panic(expected = "Invalid location: missing `:` in file://foo.rb")]
    fn location_from_string_missing_position() {
        let _location = Location::from_string("file://foo.rb");
    }

    #[test]
    #[should_panic(expected = "Invalid location: missing `-` in file://foo.rb:1:1")]
    fn location_from_string_missing_range() {
        let _location = Location::from_string("file://foo.rb:1:1");
    }

    #[test]
    #[should_panic(expected = "Invalid location: missing `:` in file://foo.rb:1-1")]
    fn location_from_string_missing_start_column() {
        let _location = Location::from_string("file://foo.rb:1-1:14");
    }

    #[test]
    #[should_panic(expected = "Invalid location: missing `:` in file://foo.rb:1:1-1")]
    fn location_from_string_missing_end_column() {
        let _location = Location::from_string("file://foo.rb:1:1-1");
    }

    #[test]
    #[should_panic(expected = "Invalid location: missing start line in file://foo.rb::1-1:1")]
    fn location_from_string_missing_start_line_number() {
        let _location = Location::from_string("file://foo.rb::1-1:1");
    }

    #[test]
    #[should_panic(expected = "Invalid location: missing start column in file://foo.rb:1:-1:1")]
    fn location_from_string_missing_start_column_number() {
        let _location = Location::from_string("file://foo.rb:1:-1:1");
    }

    #[test]
    #[should_panic(expected = "Invalid location: missing end line in file://foo.rb:1:1-::1")]
    fn location_from_string_missing_end_line_number() {
        let _location = Location::from_string("file://foo.rb:1:1-::1");
    }

    #[test]
    #[should_panic(expected = "Invalid location: missing end column in file://foo.rb:1:1-1:")]
    fn location_from_string_missing_end_column_number() {
        let _location = Location::from_string("file://foo.rb:1:1-1:");
    }

    #[test]
    fn location_to_offset() {
        let source = "class Foo\n  class Bar; end\nend";

        let location = Location::from_string("file://foo.rb:1:1-1:10");
        let offset = location.to_offset(source);
        assert_eq!(offset, Offset::new(0, 9));

        let location = Location::from_string("file://foo.rb:1:1-2:1");
        let offset = location.to_offset(source);
        assert_eq!(offset, Offset::new(0, 10));

        let location = Location::from_string("file://foo.rb:2:3-2:17");
        let offset = location.to_offset(source);
        assert_eq!(offset, Offset::new(12, 26));

        let location = Location::from_string("file://foo.rb:1:1-3:4");
        let offset = location.to_offset(source);
        assert_eq!(offset, Offset::new(0, 30));
    }

    #[test]
    fn location_from_offset_round_trip() {
        let source = "class Foo\n  def bar; end\nend\n";
        let original = Location::from_string("file://foo.rb:1:1-2:3");
        let offset = original.to_offset(source);
        let recreated = Location::from_source_and_offset(source, original.uri().to_string(), &offset);

        assert_eq!(recreated.uri(), original.uri());
        assert_eq!(recreated.line_start(), original.line_start());
        assert_eq!(recreated.column_start(), original.column_start());
        assert_eq!(recreated.line_end(), original.line_end());
        assert_eq!(recreated.column_end(), original.column_end());

        let computed_offset = recreated.to_offset(source);
        assert_eq!(computed_offset, offset);
    }
}
