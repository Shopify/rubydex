//! Extract embedded Ruby from ERB templates and index it with a Ruby backend.

use crate::{
    diagnostic::Rule,
    indexing::{IndexerBackend, RubySourceContext, local_graph::LocalGraph},
    offset::Offset,
};

use super::build_ruby_local_graph;

pub struct ERBIndexer<'a> {
    uri: Box<str>,
    source: &'a str,
    backend: IndexerBackend,
}

impl<'a> ERBIndexer<'a> {
    #[must_use]
    pub fn new(uri: Box<str>, source: &'a str, backend: IndexerBackend) -> Self {
        Self { uri, source, backend }
    }

    #[must_use]
    pub fn index(self) -> LocalGraph {
        match herb::extract_ruby(self.source) {
            Ok(ruby_source) => self.index_ruby(&ruby_source),
            Err(error) => {
                let mut graph = self.index_ruby("");
                graph.add_diagnostic(
                    Rule::ParseError,
                    Offset::new(0, 0),
                    format!("Failed to extract embedded Ruby: {error}"),
                );
                graph
            }
        }
    }

    fn index_ruby(&self, ruby_source: &str) -> LocalGraph {
        build_ruby_local_graph(
            self.uri.clone(),
            ruby_source,
            self.source,
            self.backend,
            RubySourceContext::ERBTemplate,
        )
    }
}
