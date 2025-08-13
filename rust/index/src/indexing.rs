use std::{
    cmp, fs,
    sync::{Arc, Mutex},
};

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
    pub fn new(uri: &str, source: Option<String>) -> Result<Self, IndexingError> {
        Ok(Self {
            uri: Url::parse(uri).map_err(|e| IndexingError::InvalidUri(format!("Failed to parse URI '{uri}': {e}")))?,
            source,
        })
    }

    /// # Errors
    ///
    /// Errors if the file path cannot be turned into a URI
    pub fn from_file_path(path: &str) -> Result<Self, IndexingError> {
        Ok(Self {
            uri: Url::from_file_path(path)
                .map_err(|_e| IndexingError::InvalidUri(format!("Couldn't build URI from path '{path}'")))?,
            source: None,
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
///
/// # Panics
/// This function will panic in the event of a thread dead lock, which indicates a bug in our implementation. There
/// should not be any code that tries to lock the same mutex multiple times in the same thread
pub fn index_in_parallel(graph_arc: &Arc<Mutex<Graph>>, documents: &[Document]) -> Result<(), MultipleErrors> {
    let chunk_size = cmp::max(10, documents.len() / rayon::current_num_threads());

    let indexers: Vec<RubyIndexer> = documents
        .par_chunks(chunk_size)
        .flat_map(|chunk| {
            chunk
                .iter()
                .map(|document| {
                    let mut ruby_indexer = RubyIndexer::new(document.uri.to_string());
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

                    ruby_indexer
                })
                .collect::<Vec<_>>()
        })
        .collect();

    // Insert all discovered definitions into the global graph
    let mut all_errors = Vec::new();
    let mut graph_guard = graph_arc.lock().unwrap();

    for indexer in indexers {
        let (local_graph, errors) = indexer.into_parts();
        graph_guard.update(local_graph);
        all_errors.extend(errors);
    }

    if all_errors.is_empty() {
        Ok(())
    } else {
        Err(MultipleErrors(all_errors))
    }
}
