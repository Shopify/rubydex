use std::{
    cmp, fs,
    path::PathBuf,
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
pub mod errors;
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

/// Recursively collects all Ruby files for the given workspace and dependencies, returning a vector of document instances
///
/// # Panics
///
/// Panics if there's a bug in how we're handling the arc mutex, like trying to acquire locks twice
#[must_use]
pub fn collect_documents_in_parallel(paths: Vec<String>) -> (Vec<Document>, Vec<IndexingError>) {
    let chunk_size = cmp::max(10, paths.len() / rayon::current_num_threads());
    let errors = Arc::new(Mutex::new(Vec::new()));

    let documents = paths
        .into_par_iter()
        .chunks(chunk_size)
        .flat_map(|chunk| {
            let mut documents = Vec::new();

            for path in chunk {
                collect_documents_recursively(&PathBuf::from(path), &mut documents, &errors);
            }

            documents
        })
        .collect();

    (documents, Arc::try_unwrap(errors).unwrap().into_inner().unwrap())
}

fn collect_documents_recursively(
    path: &PathBuf,
    documents: &mut Vec<Document>,
    errors: &Arc<Mutex<Vec<IndexingError>>>,
) {
    // If the file is a Ruby one, then include it. Otherwise, if it's a file of another type, we still want to return
    // early
    if path.is_file() {
        if path.extension().filter(|ext| *ext == "rb").is_some()
            && let Ok(absolute_path) = path.canonicalize()
        {
            match Document::from_file_path(&absolute_path.to_string_lossy()) {
                Ok(document) => documents.push(document),
                Err(e) => errors.lock().unwrap().push(IndexingError::InvalidUri(format!(
                    "Invalid URI for document '{}': {}",
                    absolute_path.display(),
                    e
                ))),
            }
        }

        return;
    }

    // If it's a directory, go through all entries and recursively continue collecting documents
    match fs::read_dir(path) {
        Ok(entries) => {
            for entry in entries.flatten() {
                let path = entry.path();
                collect_documents_recursively(&path, documents, errors);
            }
        }
        Err(e) => errors.lock().unwrap().push(IndexingError::FileReadError(format!(
            "Error reading path '{}': {}",
            path.display(),
            e
        ))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn collecting_documents_in_parallel() {
        let cargo_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let root = cargo_path.parent().unwrap().parent().unwrap();

        // Paths including a specific file and a directory
        let paths = vec![
            root.join("lib/index.rb").to_str().unwrap().to_string(),
            root.join("lib/index").to_str().unwrap().to_string(),
        ];

        let (documents, errors) = collect_documents_in_parallel(paths);
        assert!(errors.is_empty(), "Errors: {errors:#?}");
        assert!(documents.len() > 2);
    }

    #[test]
    fn collecting_a_non_existing_path() {
        let paths = vec!["/non_existing_file.rb".to_string(), "/non_existing_dir".to_string()];

        let (documents, errors) = collect_documents_in_parallel(paths);
        assert!(documents.is_empty());
        assert_eq!(errors.len(), 2);
    }
}
