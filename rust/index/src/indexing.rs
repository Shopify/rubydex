use crate::{
    indexing::{
        errors::{IndexingError, MultipleErrors},
        ruby_indexer::{RubyIndexer, IndexerParts},
    },
    model::graph::Graph, source_location::{UTF8SourceLocationConverter},
};
use glob::glob;
use std::sync::{
    Arc, Mutex,
    mpsc::{Receiver, Sender},
};
use std::{fs, path::Path};
use std::{sync::mpsc, thread};
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
pub fn index_in_parallel(graph: &mut Graph, documents: Vec<Document>) -> Result<(), MultipleErrors> {
    let (tx, rx): (Sender<IndexerParts>, Receiver<IndexerParts>) = mpsc::channel();
    let num_threads = thread::available_parallelism().map(std::num::NonZero::get).unwrap_or(4);
    let mut threads = Vec::with_capacity(num_threads);
    let document_queue = Arc::new(Mutex::new(documents));

    for _ in 0..num_threads {
        let thread_tx = tx.clone();
        let queue = Arc::clone(&document_queue);

        let handle = thread::spawn(move || {
            while let Some(document) = { queue.lock().unwrap().pop() } {
                let (source, errors) = {
                    let mut errors = Vec::new();

                    let source: String =
                        if let Some(source) = &document.source {
                            source.clone()
                        } else {
                            fs::read_to_string(document.path()).unwrap_or_else(|e| {
                                errors.push(IndexingError::FileReadError(format!(
                                    "Failed to read {}: {}",
                                    document.path(),
                                    e
                                )));
                                String::new()
                            })
                        };
                    
                    (source, errors)
                };
                
                let converter = UTF8SourceLocationConverter::new(&source);
                let mut ruby_indexer = RubyIndexer::new(document.uri.to_string(), &converter);

                if errors.is_empty() {
                    ruby_indexer.index(&source);
                } else {
                    for error in errors {
                        ruby_indexer.add_error(error);
                    }
                }

                thread_tx
                    .send(ruby_indexer.into_parts())
                    .expect("Receiver end should not be closed until all threads are done");
            }
        });

        threads.push(handle);
    }

    drop(tx);

    // Insert all discovered definitions into the global graph
    let mut all_errors = Vec::new();

    for (local_graph, errors) in rx {
        graph.update(local_graph);
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
pub fn collect_documents(paths: Vec<String>) -> (Vec<Document>, Vec<IndexingError>) {
    let mut errors = Vec::new();
    let mut documents = Vec::new();

    for path in paths {
        let path_obj = Path::new(&path);

        if path_obj.is_dir() {
            match glob(&format!("{path}/**/*.rb")) {
                Ok(entries) => {
                    for entry in entries {
                        match entry {
                            Ok(path) => match Document::from_file_path(&path.to_string_lossy()) {
                                Ok(document) => documents.push(document),
                                Err(e) => errors.push(e),
                            },
                            Err(e) => errors.push(IndexingError::FileReadError(format!(
                                "Failed to read glob entry in '{path}': {e}"
                            ))),
                        }
                    }
                }
                Err(e) => {
                    errors.push(IndexingError::FileReadError(format!(
                        "Failed to read glob pattern '{path}/**/*.rb': {e}"
                    )));
                }
            }
            continue;
        }

        if path_obj.exists() {
            match Document::from_file_path(&path) {
                Ok(document) => documents.push(document),
                Err(e) => errors.push(e),
            }
        }
    }

    (documents, errors)
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

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

        let (documents, errors) = collect_documents(paths);
        assert!(errors.is_empty(), "Errors: {errors:#?}");
        assert!(documents.len() > 2);
    }

    #[test]
    fn collecting_a_non_existing_path() {
        let paths = vec!["/non_existing_file.rb".to_string(), "/non_existing_dir".to_string()];

        let (documents, errors) = collect_documents(paths);
        assert!(documents.is_empty());
        assert!(errors.is_empty());
    }
}
