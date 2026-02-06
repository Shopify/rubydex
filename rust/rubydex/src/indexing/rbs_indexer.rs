//! Visit the RBS AST and create type definitions.

use crate::{
    indexing::local_graph::LocalGraph,
    model::{document::Document, ids::UriId},
};

pub struct RBSIndexer {
    #[allow(dead_code)]
    uri_id: UriId,
    local_graph: LocalGraph,
}

impl RBSIndexer {
    #[must_use]
    pub fn new(uri: String, source: &str) -> Self {
        let uri_id = UriId::from(&uri);
        let local_graph = LocalGraph::new(uri_id, Document::new(uri, source));

        Self { uri_id, local_graph }
    }

    #[must_use]
    pub fn local_graph(self) -> LocalGraph {
        self.local_graph
    }

    pub fn index(&mut self) {}
}
