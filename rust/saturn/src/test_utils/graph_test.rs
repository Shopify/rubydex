use super::normalize_indentation;
use crate::indexing::ruby_indexer::RubyIndexer;
use crate::model::graph::Graph;
use crate::source_location::UTF8SourceLocationConverter;

#[derive(Default)]
pub struct GraphTest {
    pub graph: Graph,
}

impl GraphTest {
    #[must_use]
    pub fn new() -> Self {
        Self { graph: Graph::new() }
    }

    #[must_use]
    fn index_source(uri: &str, source: &str) -> Graph {
        let converter = UTF8SourceLocationConverter::new(source);
        let content_hash = crate::indexing::Document::calculate_content_hash(source.as_bytes());
        let mut indexer = RubyIndexer::new(uri.to_string(), &converter, source, content_hash);
        indexer.index();
        indexer.into_parts().0
    }

    pub fn index_uri(&mut self, uri: &str, source: &str) {
        let source = normalize_indentation(source);
        let local_index = Self::index_source(uri, &source);
        self.graph.update(local_index);
    }

    pub fn delete_uri(&mut self, uri: &str) {
        self.graph.unload_uri(uri);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_index_uri_with_single_line() {
        let mut context = GraphTest::new();

        context.index_uri("file://method.rb", "class Foo; end");

        let foo_defs = context.graph.get("Foo").unwrap();
        assert_eq!(foo_defs.len(), 1);
        assert_eq!(foo_defs[0].start(), 0);
        assert_eq!(foo_defs[0].end(), 14);
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

        let foo_defs = context.graph.get("Foo").unwrap();
        assert_eq!(foo_defs.len(), 1);
        assert_eq!(foo_defs[0].start(), 0);
        assert_eq!(foo_defs[0].end(), 30);

        let bar_defs = context.graph.get("Foo::Bar").unwrap();
        assert_eq!(bar_defs.len(), 1);
        assert_eq!(bar_defs[0].start(), 12);
        assert_eq!(bar_defs[0].end(), 26);
    }

    #[test]
    fn test_index_uri_with_new_lines() {
        let mut context = GraphTest::new();

        context.index_uri("file://method.rb", "\n\nclass Foo; end");

        let foo_defs = context.graph.get("Foo").unwrap();
        assert_eq!(foo_defs.len(), 1);
        assert_eq!(foo_defs[0].start(), 2);
        assert_eq!(foo_defs[0].end(), 16);
    }
}
