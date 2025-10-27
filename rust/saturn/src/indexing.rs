use crate::{
    indexing::{
        errors::{IndexingError, MultipleErrors},
        ruby_indexer::{IndexerParts, RubyIndexer},
    },
    model::graph::Graph,
    source_location::UTF8SourceLocationConverter,
};
use glob::glob;
use std::sync::{
    Arc, Mutex,
    mpsc::{Receiver, Sender},
};
use std::{
    fs,
    path::{Path, PathBuf},
};
use std::{sync::mpsc, thread};
use url::Url;
use xxhash_rust::xxh3::xxh3_64;

pub mod errors;
pub mod ruby_indexer;
pub mod scope;

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

    /// # Errors
    ///
    /// Errors if the URI is not a valid file:// URI
    pub fn path(&self) -> Result<PathBuf, IndexingError> {
        self.uri
            .to_file_path()
            .map_err(|_e| IndexingError::InvalidUri(format!("Couldn't convert URI '{}' to file path", self.uri)))
    }

    #[must_use]
    pub fn calculate_content_hash(source: &[u8]) -> u16 {
        // Explicitly take only the lower 16 bits of the hash
        // This is intentional as this is only used for document staleness check
        (xxh3_64(source) & 0xFFFF) as u16
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
    let index_document = |document: Document| -> IndexerParts {
        let uri = document.uri.to_string();
        let (source, errors) = read_document_source(document);
        if !errors.is_empty() {
            return (None, errors);
        }

        let converter = UTF8SourceLocationConverter::new(&source);
        let content_hash = Document::calculate_content_hash(source.as_bytes());
        let mut ruby_indexer = RubyIndexer::new(uri, &converter, &source, content_hash);
        ruby_indexer.index();
        ruby_indexer.into_parts()
    };

    let merge_result = |local_graph| graph.update(local_graph);

    with_parallel_workers(documents, index_document, merge_result)
}

/// Reads the source content from a document, either from memory or disk
fn read_document_source(document: Document) -> (String, Vec<IndexingError>) {
    let mut errors = Vec::new();

    let source = if let Some(source) = document.source {
        source
    } else {
        match document.path() {
            Ok(path) => fs::read_to_string(&path).unwrap_or_else(|e| {
                errors.push(IndexingError::FileReadError(format!(
                    "Failed to read {}: {}",
                    path.display(),
                    e
                )));
                String::new()
            }),
            Err(e) => {
                errors.push(e);
                String::new()
            }
        }
    };

    (source, errors)
}

fn with_parallel_workers<F, G>(documents: Vec<Document>, worker_fn: F, mut result_fn: G) -> Result<(), MultipleErrors>
where
    F: Fn(Document) -> IndexerParts + Send + Clone + 'static,
    G: FnMut(Graph),
{
    let (tx, rx): (Sender<IndexerParts>, Receiver<IndexerParts>) = mpsc::channel();
    let num_threads = thread::available_parallelism().map(std::num::NonZero::get).unwrap_or(4);
    let mut threads = Vec::with_capacity(num_threads);
    let document_queue = Arc::new(Mutex::new(documents));

    for _ in 0..num_threads {
        let thread_tx = tx.clone();
        let queue = Arc::clone(&document_queue);
        let thread_fn = worker_fn.clone();

        let handle = thread::spawn(move || {
            while let Some(document) = { queue.lock().unwrap().pop() } {
                let (result, errors) = thread_fn(document);
                if result.is_some() || !errors.is_empty() {
                    thread_tx
                        .send((result, errors))
                        .expect("Receiver end should not be closed until all threads are done");
                }
            }
        });

        threads.push(handle);
    }

    drop(tx);

    let mut all_errors = Vec::new();
    for (result, errors) in rx {
        if let Some(local_graph) = result {
            result_fn(local_graph);
        }
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

            continue;
        }

        errors.push(IndexingError::FileReadError(format!("Path '{path}' does not exist")));
    }

    (documents, errors)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::Context;

    fn collect_document_paths(context: &Context, paths: &[&str]) -> (Vec<String>, Vec<IndexingError>) {
        let (documents, errors) = collect_documents(
            paths
                .iter()
                .map(|p| context.absolute_path_to(p).to_string_lossy().into_owned())
                .collect(),
        );

        let mut paths: Vec<String> = documents
            .iter()
            .map(|d| {
                context
                    .relative_path_to(d.path().ok().unwrap())
                    .to_string_lossy()
                    .into_owned()
            })
            .collect();

        paths.sort();

        (paths, errors)
    }

    #[test]
    fn collect_all_documents() {
        let context = Context::new();
        context.touch("bar/baz.rb");
        context.touch("bar/qux.rb");
        context.touch("foo/bar.rb");

        let (paths, errors) = collect_document_paths(&context, &["foo", "bar"]);

        assert_eq!(
            paths,
            vec![
                "bar/baz.rb".to_string(),
                "bar/qux.rb".to_string(),
                "foo/bar.rb".to_string()
            ]
        );
        assert!(errors.is_empty());
    }

    #[test]
    fn collect_some_documents_based_on_paths() {
        let context = Context::new();
        context.touch("bar/baz.rb");
        context.touch("bar/qux.rb");
        context.touch("foo/bar.rb");

        let (paths, errors) = collect_document_paths(&context, &["bar"]);

        assert_eq!(paths, vec!["bar/baz.rb".to_string(), "bar/qux.rb".to_string()]);
        assert!(errors.is_empty());
    }

    #[test]
    fn collect_non_existing_paths() {
        let context = Context::new();

        let (documents, errors) = collect_documents(vec![
            context
                .absolute_path_to("non_existing_path")
                .to_string_lossy()
                .into_owned(),
        ]);

        assert!(documents.is_empty());

        assert_eq!(
            errors
                .iter()
                .map(std::string::ToString::to_string)
                .collect::<Vec<String>>(),
            vec![format!(
                "File read error: Path '{}/non_existing_path' does not exist",
                context.absolute_path().display()
            )]
        );
    }

    #[test]
    fn with_parallel_workers_skips_empty_results() {
        let documents = vec![Document::new("file:///skipped.rb", Some("module Foo; end".to_string())).unwrap()];

        let mut was_called = false;
        let worker_fn = |_document: Document| -> IndexerParts { (None, vec![]) };
        let result_fn = |_graph: Graph| {
            was_called = true;
        };

        let result = with_parallel_workers(documents, worker_fn, result_fn);

        assert!(result.is_ok());

        assert!(
            !was_called,
            "result_fn should not be called when worker_fn returns (None, vec![])"
        );
    }
}
