use super::normalize_indentation;
use crate::indexing::local_graph::LocalGraph;
use crate::indexing::rbs_indexer::RBSIndexer;
use crate::indexing::ruby_indexer::RubyIndexer;
use crate::model::definitions::Definition;
use crate::model::ids::UriId;
use crate::offset::Offset;
use crate::position::Position;

#[cfg(any(test, feature = "test_utils"))]
pub struct LocalGraphTest {
    uri: String,
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

        Self { uri, graph }
    }

    #[must_use]
    pub fn new_rbs(uri: &str, source: &str) -> Self {
        let uri = uri.to_string();
        let source = normalize_indentation(source);

        let mut indexer = RBSIndexer::new(uri.clone(), &source);
        indexer.index();
        let graph = indexer.local_graph();

        Self { uri, graph }
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
                    .map(|offset| offset.to_display_range(self.graph.document()))
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
        let line_index = self.graph.document().line_index();

        let start_offset = line_index.offset(start_position).unwrap_or(0.into());
        let end_offset = line_index.offset(end_position).unwrap_or(0.into());

        (uri, Offset::new(start_offset.into(), end_offset.into()))
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

// Primitive assertions

/// Asserts that a `NameId` resolves to the expected full path string.
///
/// Usage:
/// - `assert_name_path_eq!(ctx, "Foo::Bar::Baz", name_id)` - asserts the full path `Foo::Bar::Baz`
/// - `assert_name_path_eq!(ctx, "Baz", name_id)` - asserts just `Baz` with no parent scope
#[cfg(test)]
#[macro_export]
macro_rules! assert_name_path_eq {
    ($context:expr, $expect_path:expr, $name_id:expr) => {{
        let mut name_parts = Vec::new();
        let mut current_name_id = Some($name_id);

        while let Some(name_id) = current_name_id {
            let name = $context.graph().names().get(&name_id).unwrap();
            name_parts.push($context.graph().strings().get(name.str()).unwrap().as_str());
            current_name_id = name.parent_scope().as_ref().copied();
        }

        name_parts.reverse();

        let actual_path = name_parts.join("::");
        assert_eq!(
            $expect_path, actual_path,
            "name path mismatch: expected `{}`, got `{}`",
            $expect_path, actual_path
        );
    }};
}

/// Asserts that a `StringId` resolves to the expected string.
///
/// Usage:
/// - `assert_string_eq!(ctx, str_id, "Foo::Bar::Baz")`
#[cfg(test)]
#[macro_export]
macro_rules! assert_string_eq {
    ($context:expr, $str_id:expr, $expected_str:expr) => {{
        let string_name = $context.graph().strings().get($str_id).unwrap().as_str();
        assert_eq!(
            string_name, $expected_str,
            "string mismatch: expected `{}`, got `{}`",
            $expected_str, string_name
        );
    }};
}

// Definition assertions

#[cfg(test)]
#[macro_export]
macro_rules! assert_definition_at {
    ($context:expr, $location:expr, $variant:ident, |$var:ident| $body:block) => {{
        let __def = $context.definition_at($location);
        let __kind = __def.kind();
        match __def {
            $crate::model::definitions::Definition::$variant(boxed) => {
                let $var = &*boxed.as_ref();
                $body
            }
            _ => panic!("expected {} definition, got {:?}", stringify!($variant), __kind),
        }
    }};

    ($context:expr, $location:expr, $variant:ident) => {{
        let __def = $context.definition_at($location);
        let __kind = __def.kind();
        match __def {
            $crate::model::definitions::Definition::$variant(_) => {}
            _ => panic!("expected {} definition, got {:?}", stringify!($variant), __kind),
        }
    }};
}

/// Asserts the full path of a definition's `name_id` matches the expected string.
///
/// Usage:
/// - `assert_def_name_eq!(ctx, def, "Foo::Bar::Baz")` - asserts the full path `Foo::Bar::Baz`
/// - `assert_def_name_eq!(ctx, def, "Baz")` - asserts just `Baz` with no parent scope
#[cfg(test)]
#[macro_export]
macro_rules! assert_def_name_eq {
    ($context:expr, $def:expr, $expect_path:expr) => {{
        $crate::assert_name_path_eq!($context, $expect_path, *$def.name_id());
    }};
}

/// Asserts that a definition's superclass reference matches the expected name.
///
/// Usage:
/// - `assert_def_superclass_ref_eq!(ctx, def, "Bar::Baz")` - asserts the full path `Bar::Baz`
#[cfg(test)]
#[macro_export]
macro_rules! assert_def_superclass_ref_eq {
    ($context:expr, $def:expr, $expected_name:expr) => {{
        let name_id = *$context
            .graph()
            .constant_references()
            .get($def.superclass_ref().unwrap())
            .unwrap()
            .name_id();
        $crate::assert_name_path_eq!($context, $expected_name, name_id);
    }};
}

/// Asserts that a definition's name offset matches the expected location.
///
/// Usage:
/// - `assert_def_name_offset_eq!(ctx, def, "1:7-1:10")`
#[cfg(test)]
#[macro_export]
macro_rules! assert_def_name_offset_eq {
    ($context:expr, $def:expr, $expected_location:expr) => {{
        let (_, expected_offset) = $context.parse_location(&format!("{}:{}", $context.uri(), $expected_location));
        assert_eq!(
            &expected_offset,
            $def.name_offset(),
            "name_offset mismatch: expected `{}`, got `{}`",
            expected_offset.to_display_range($context.graph().document()),
            $def.name_offset().to_display_range($context.graph().document())
        );
    }};
}

/// Asserts that a definition's string matches the expected string.
///
/// Usage:
/// - `assert_def_str_eq!(ctx, def, "baz()")`
#[cfg(test)]
#[macro_export]
macro_rules! assert_def_str_eq {
    ($context:expr, $def:expr, $expect_name_string:expr) => {{
        $crate::assert_string_eq!($context, $def.str_id(), $expect_name_string);
    }};
}

// Comment assertions

#[cfg(test)]
#[macro_export]
/// Asserts that a definition's comments matches the expected comments.
///
/// Usage:
/// - `assert_def_comments_eq!(ctx, def, ["# Comment 1", "# Comment 2"])`
macro_rules! assert_def_comments_eq {
    ($context:expr, $def:expr, $expected_comments:expr) => {{
        let actual_comments: Vec<String> = $def.comments().iter().map(|c| c.string().to_string()).collect();
        assert_eq!(
            $expected_comments,
            actual_comments.as_slice(),
            "comments mismatch: expected `{:?}`, got `{:?}`",
            $expected_comments,
            actual_comments
        );
    }};
}

// Diagnostic assertions

#[cfg(test)]
#[macro_export]
macro_rules! assert_local_diagnostics_eq {
    ($context:expr, $expected_diagnostics:expr) => {{
        let mut diagnostics = $context.graph().diagnostics().iter().collect::<Vec<_>>();
        diagnostics.sort_by_key(|d| d.offset());
        let formatted: Vec<String> = diagnostics
            .iter()
            .map(|d| d.formatted($context.graph().document()))
            .collect();
        assert_eq!(
            $expected_diagnostics,
            formatted.as_slice(),
            "diagnostics mismatch: expected `{:?}`, got `{:?}`",
            $expected_diagnostics,
            formatted
        );
    }};
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_no_local_diagnostics {
    ($context:expr) => {{
        let diagnostics = $context.graph().diagnostics().iter().collect::<Vec<_>>();
        let formatted: Vec<String> = diagnostics
            .iter()
            .map(|d| d.formatted($context.graph().document()))
            .collect();
        assert!(diagnostics.is_empty(), "expected no diagnostics, got {:?}", formatted);
    }};
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
