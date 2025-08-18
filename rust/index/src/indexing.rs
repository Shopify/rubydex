use std::{cmp, fs, sync::Arc};

use crate::{
    indexing::{
        errors::{IndexingError, MultipleErrors},
        ruby_indexer::RubyIndexer,
    },
    model::graph::Graph,
};
use rayon::prelude::*;
use url::Url;
mod errors;
pub mod ruby_indexer;

// Represents a document to be indexed. If `source` is `Some(String)`, we're indexing a file state that's not committed
// to disk yet and should then use the given `source` instead of reading from disk
pub struct Document {
    uri: Url,
    source: Option<String>,
}

impl Document {
    /// # Errors
    ///
    /// Creating a new document fails on invalid URIs
    pub fn new(uri: &str, source: Option<String>) -> Result<Self, url::ParseError> {
        Ok(Self {
            uri: Url::parse(uri)?,
            source,
        })
    }

    #[must_use]
    pub fn path(&self) -> &str {
        self.uri.path()
    }
}

/// Indexes the given items, reading the content from disk and populating the given `Graph` instance.
///
/// # Errors
///
/// Returns Ok if indexing succeeded for all given documents or a vector of errors for all failures
pub fn index_in_parallel(graph_arc: &Arc<Graph>, documents: &[Document]) -> Result<(), MultipleErrors> {
    let chunk_size = cmp::max(10, documents.len() / rayon::current_num_threads());

    let errors: Vec<IndexingError> = documents
        .par_chunks(chunk_size)
        .flat_map(|chunk| {
            chunk
                .iter()
                .flat_map(|document| {
                    let mut ruby_indexer = RubyIndexer::new(Arc::clone(graph_arc), document.uri.to_string());

                    // If the document includes the source, use it directly to index (especially since the content may not
                    // be committed to disk yet). Otherwise, read it from disk
                    if let Some(source) = &document.source {
                        ruby_indexer.index(source);
                    } else {
                        match fs::read_to_string(document.path()) {
                            Ok(source) => {
                                ruby_indexer.index(&source);
                            }
                            Err(e) => {
                                ruby_indexer.add_error(IndexingError::FileReadError(format!(
                                    "Failed to read {}: {}",
                                    document.path(),
                                    e
                                )));
                            }
                        }
                    }

                    ruby_indexer.into_errors()
                })
                .collect::<Vec<_>>()
        })
        .collect();

    if errors.is_empty() {
        Ok(())
    } else {
        Err(MultipleErrors(errors))
    }
}
