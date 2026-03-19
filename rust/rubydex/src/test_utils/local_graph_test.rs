use super::normalize_indentation;
use crate::indexing::local_graph::LocalGraph;
use crate::indexing::rbs_indexer::RBSIndexer;
use crate::indexing::ruby_indexer::RubyIndexer;
use crate::model::definitions::Definition;
use crate::model::graph::NameDependent;
use crate::model::ids::{NameId, StringId, UriId};
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
    pub fn new_rbs(uri: &str, source: &str) -> Self {
        let uri = uri.to_string();
        let source = normalize_indentation(source);

        let mut indexer = RBSIndexer::new(uri.clone(), &source);
        indexer.index();
        let graph = indexer.local_graph();

        Self { uri, source, graph }
    }

    #[must_use]
    pub fn from_local_graph(uri: &str, source: &str, graph: LocalGraph) -> Self {
        Self {
            uri: uri.to_string(),
            source: source.to_string(),
            graph,
        }
    }

    #[must_use]
    pub fn uri(&self) -> &str {
        &self.uri
    }

    #[must_use]
    pub fn source(&self) -> &str {
        &self.source
    }

    #[must_use]
    pub fn source_at(&self, offset: &Offset) -> &str {
        &self.source[offset.start() as usize..offset.end() as usize]
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

    // Name dependents helpers

    /// Finds all `NameId`s matching a path. `"Foo"` matches names with str="Foo" and no
    /// `parent_scope`. `"Bar::Baz"` matches names with str="Baz" and `parent_scope` str="Bar".
    /// Multiple matches are possible when the same constant appears at different nestings.
    ///
    /// # Panics
    ///
    /// Panics if no names match the given path.
    #[must_use]
    pub fn find_name_ids(&self, path: &str) -> Vec<NameId> {
        let (parent, name) = match path.rsplit_once("::") {
            Some((p, n)) => (Some(p), n),
            None => (None, path),
        };
        let target_str_id = StringId::from(name);
        let ids: Vec<NameId> = self
            .graph()
            .names()
            .iter()
            .filter(|(_, name_ref)| {
                if *name_ref.str() != target_str_id {
                    return false;
                }
                match parent {
                    None => name_ref.parent_scope().as_ref().is_none(),
                    Some(p) => name_ref.parent_scope().as_ref().is_some_and(|ps_id| {
                        let ps = self.graph().names().get(ps_id).unwrap();
                        *ps.str() == StringId::from(p)
                    }),
                }
            })
            .map(|(id, _)| *id)
            .collect();
        assert!(!ids.is_empty(), "could not find name `{path}`");
        ids
    }

    #[must_use]
    pub fn name_dependents_for(&self, name_id: NameId) -> Vec<NameDependent> {
        self.graph()
            .name_dependents()
            .get(&name_id)
            .cloned()
            .unwrap_or_default()
    }

    /// # Panics
    ///
    /// Panics if the name's string is not in the strings map.
    #[must_use]
    pub fn name_str(&self, name_id: &NameId) -> Option<&str> {
        self.graph()
            .names()
            .get(name_id)
            .map(|n| self.graph().strings().get(n.str()).unwrap().as_str())
    }

    /// Returns the unqualified name string for a `NameDependent`, if available.
    #[must_use]
    pub fn dependent_name_str(&self, dep: &NameDependent) -> Option<&str> {
        match dep {
            NameDependent::ChildName(id) | NameDependent::NestedName(id) => self.name_str(id),
            NameDependent::Definition(id) => self
                .graph()
                .definitions()
                .get(id)
                .and_then(|d| d.name_id())
                .and_then(|name_id| self.name_str(name_id)),
            NameDependent::Reference(id) => self
                .graph()
                .constant_references()
                .get(id)
                .and_then(|r| self.name_str(r.name_id())),
        }
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

/// Asserts that the source text at a given `Offset` matches the expected string.
///
/// Usage:
/// - `assert_offset_string!(ctx, param.offset(), "String")`
#[cfg(test)]
#[macro_export]
macro_rules! assert_offset_string {
    ($context:expr, $offset:expr, $expected:expr) => {{
        let actual = $context.source_at($offset);
        assert_eq!(
            actual, $expected,
            "offset text mismatch: expected `{}`, got `{}`",
            $expected, actual
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

// Mixin assertions

/// Asserts that a definition's mixins match the expected names for a given mixin type.
///
/// Usage:
/// - `assert_def_mixins_eq!(ctx, def, Include, ["Foo", "Bar"])`
#[cfg(test)]
#[macro_export]
macro_rules! assert_def_mixins_eq {
    ($context:expr, $def:expr, $mixin_type:ident, $expected_names:expr) => {{
        use $crate::model::definitions::Mixin;

        let actual_names = $def
            .mixins()
            .iter()
            .filter_map(|mixin| {
                if let Mixin::$mixin_type(def) = mixin {
                    let name = $context
                        .graph()
                        .names()
                        .get(
                            $context
                                .graph()
                                .constant_references()
                                .get(def.constant_reference_id())
                                .unwrap()
                                .name_id(),
                        )
                        .unwrap();
                    Some($context.graph().strings().get(name.str()).unwrap().as_str())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        assert_eq!(
            $expected_names,
            actual_names.as_slice(),
            "mixins mismatch: expected `{:?}`, got `{:?}`",
            $expected_names,
            actual_names
        );
    }};
}

// Name dependent assertions

/// Asserts that `owner` has dependents matching the given list.
/// Each entry uses `Variant("name")` syntax. When multiple names match the owner path
/// (different nestings), any match suffices for each expected dependent.
///
/// Usage:
/// ```ignore
/// assert_dependents!(ctx, "Bar", [ChildName("Baz"), Definition("Bar")]);
/// assert_dependents!(ctx, "Bar::Baz", [NestedName("CONST"), Definition("Baz")]);
/// ```
#[cfg(test)]
#[macro_export]
macro_rules! assert_dependents {
    ($ctx:expr, $owner:expr, [$($variant:ident($dep:expr)),* $(,)?]) => {{
        let owner_ids = $ctx.find_name_ids($owner);
        $(
            let found = owner_ids.iter().any(|owner_id| {
                $ctx.name_dependents_for(*owner_id).iter().any(|d| {
                    matches!(d, $crate::model::graph::NameDependent::$variant(_))
                        && $ctx.dependent_name_str(d) == Some($dep)
                })
            });
            assert!(
                found,
                "expected {}({}) in {}'s dependents",
                stringify!($variant),
                $dep,
                $owner
            );
        )*
    }};
}

// Receiver assertions

/// Asserts that a method has the expected receiver.
///
/// Usage:
/// - `assert_method_has_receiver!(ctx, method, "Foo")`
/// - `assert_method_has_receiver!(ctx, method, "<Bar>")`
#[cfg(test)]
#[macro_export]
macro_rules! assert_method_has_receiver {
    ($context:expr, $method:expr, $expected_receiver:expr) => {{
        let name_id = match $method.receiver() {
            Some($crate::model::definitions::Receiver::SelfReceiver(def_id)) => {
                let def = $context.graph().definitions().get(def_id).unwrap();
                *def.name_id().expect("SelfReceiver definition should have a name_id")
            }
            Some($crate::model::definitions::Receiver::ConstantReceiver(name_id)) => *name_id,
            None => {
                panic!(
                    "Method receiver mismatch: expected `{}`, got `None`",
                    $expected_receiver
                );
            }
        };

        let name = $context.graph().names().get(&name_id).unwrap();
        let actual_name = $context.graph().strings().get(name.str()).unwrap().as_str();
        assert_eq!(
            $expected_receiver, actual_name,
            "method receiver mismatch: expected `{}`, got `{}`",
            $expected_receiver, actual_name
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
