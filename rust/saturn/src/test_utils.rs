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
        let mut indexer = RubyIndexer::new(uri.to_string(), &converter, source);
        indexer.index();
        indexer.into_parts().0
    }

    #[must_use]
    fn normalize_indentation(input: &str) -> String {
        // If the input starts with a newline followed by indentation, drop that
        // very first newline so that the first content line can start at 0.
        // If the first newline is not followed by indentation, preserve it.
        let input = if let Some(rest) = input.strip_prefix('\n') {
            match rest.chars().next() {
                Some(' ' | '\t') => rest,
                _ => input,
            }
        } else {
            input
        };

        let lines: Vec<&str> = input.lines().collect();

        if lines.is_empty() {
            return String::new();
        }

        // Determine base indentation from the first non-empty line, but do not
        // remove or trim any leading/trailing blank lines. We only strip
        // indentation from each line while preserving all newlines.
        let first_non_empty_line = match lines.iter().find(|line| !line.trim().is_empty()) {
            Some(line) => *line,
            None => return input.to_string(),
        };

        let base_indent = first_non_empty_line.len() - first_non_empty_line.trim_start().len();

        lines
            .iter()
            .map(|line| {
                if line.trim().is_empty() {
                    ""
                } else if line.len() >= base_indent && line.chars().take(base_indent).all(char::is_whitespace) {
                    &line[base_indent..]
                } else {
                    line
                }
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    pub fn index_uri(&mut self, uri: &str, source: &str) {
        let source = Self::normalize_indentation(source);
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
