use line_index::LineIndex;

use super::normalize_indentation;
use crate::indexing::local_graph::LocalGraph;
use crate::indexing::ruby_indexer::RubyIndexer;
use crate::location::Location;
use crate::model::definitions::Definition;
use crate::model::ids::UriId;
use crate::offset::Offset;

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
    /// - If the location string format is invalid.
    /// - If the URI has no source.
    /// - If the positions are invalid.
    /// - If a definition cannot be found at the given location.
    #[must_use]
    pub fn definition_at<'a>(&'a self, location_string: &str) -> &'a Definition {
        let location = Location::from_string(&format!("{}:{}", self.uri(), location_string));
        let offset = location.to_offset(&self.source);
        let uri_id = UriId::from(location.uri());

        let definitions = self
            .graph()
            .definitions()
            .values()
            .filter(|def| def.uri_id() == &uri_id && def.offset() == &offset)
            .collect::<Vec<_>>();

        assert!(
            !definitions.is_empty(),
            "could not find a definition matching `{location_string}`, did you mean one of the following: {:?}",
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
                    .map(|offset| self.offset_to_location_string(offset))
                    .collect::<Vec<_>>()
            }
        );
        assert!(
            definitions.len() < 2,
            "found more than one definition matching {location}"
        );

        definitions[0]
    }

    #[must_use]
    pub fn offset_to_location_string(&self, offset: &Offset) -> String {
        let line_index = LineIndex::new(&self.source);
        let start = line_index.line_col(offset.start().into());
        let end = line_index.line_col(offset.end().into());
        format!("{}:{}-{}:{}", start.line + 1, start.col + 1, end.line + 1, end.col + 1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn definition_at_valid_location() {
        let context = LocalGraphTest::new("file://foo.rb", "class Foo; end");
        let definition = context.definition_at("1:1-1:15");

        match definition {
            Definition::Class(class) => {
                assert_eq!(class.offset(), &Offset::new(0, 14));
            }
            _ => panic!("expected class definition, got {:?}", definition.kind()),
        }
    }

    #[test]
    #[should_panic(
        expected = "could not find a definition matching `1:1-1:10`, did you mean one of the following: [\"1:1-1:15\"]"
    )]
    fn definition_at_invalid_location() {
        let context = LocalGraphTest::new("file://foo.rb", "class Foo; end");
        let _definition = context.definition_at("1:1-1:10");
    }
}
