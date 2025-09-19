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
use std::{collections::HashMap, collections::HashSet, fs, path::Path};
use std::{sync::mpsc, thread};
use url::Url;
use xxhash_rust::xxh3::xxh3_64;
pub mod errors;
pub mod ruby_indexer;

#[derive(Clone)]
pub struct Document {
    uri: Url,
    source: String,
    content_hash: i64,
}

impl Document {
    /// # Errors
    ///
    /// Creating a new document fails on invalid URIs
    pub fn new(uri: &str, source: Option<String>) -> Result<Self, IndexingError> {
        let uri = Url::parse(uri).map_err(|e| IndexingError::InvalidUri(e.to_string()))?;

        // Try to get the file content from the source first. If not try to read from the disk
        let file_content = source.map_or_else(
            || fs::read_to_string(uri.path()).map_err(|e| IndexingError::FileReadError(e.to_string())),
            Ok,
        )?;
        let content_hash = xxh3_64(file_content.as_bytes()).cast_signed();

        Ok(Self {
            uri,
            source: file_content,
            content_hash,
        })
    }

    /// # Errors
    ///
    /// Errors if the file path cannot be turned into a URI
    pub fn from_file_path(path: &str) -> Result<Self, IndexingError> {
        let uri = Url::from_file_path(path)
            .map_err(|_e| IndexingError::InvalidUri(format!("Couldn't build URI from path '{path}'")))?;
        let file_content = fs::read_to_string(path).map_err(|e| IndexingError::FileReadError(e.to_string()))?;
        let content_hash = xxh3_64(file_content.as_bytes()).cast_signed();

        Ok(Self {
            uri,
            source: file_content,
            content_hash,
        })
    }

    #[must_use]
    pub fn path(&self) -> &str {
        self.uri.path()
    }

    #[must_use]
    pub fn content_hash(&self) -> i64 {
        self.content_hash
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
                let converter = UTF8SourceLocationConverter::new(&document.source);
                let mut ruby_indexer = RubyIndexer::new(&document, &converter);
                ruby_indexer.index();

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

/// Indexes the given documents and syncs them with the database.
///
/// This function performs a complete indexing workflow:
/// 1. Syncs the documents with the database to determine what needs to be updated
/// 2. Indexes the documents that require processing in parallel
/// 3. Saves the updated graph back to the database
///
/// # Errors
///
/// Returns `MultipleErrors` if any of the following operations fail:
/// - Database synchronization operations
/// - Parallel indexing of documents
/// - Saving the graph to the database
pub fn index_and_sync(graph: &mut Graph, documents: Vec<Document>) -> Result<(), MultipleErrors> {
    let db_documents = graph.db.all_documents_hashes_by_uri()?;
    let (new_uris, stale_uris) = calculate_diff(&documents, &db_documents);
    stale_uris.iter().try_for_each(|uri| graph.delete_uri(uri))?;
    let documents_to_index = documents
        .into_iter()
        .filter(|doc| new_uris.contains(doc.uri.as_str()))
        .collect();

    index_in_parallel(graph, documents_to_index)?;
    graph.save_to_database()?;
    Ok(())
}

/// Calculates which documents need to be indexed and which need to be removed.
///
/// Returns `(new_uris, stale_uris)` where:
/// - `new_uris`: Documents to index (includes new and updated documents)
/// - `stale_uris`: Documents to delete from graph/DB (includes deleted and updated documents)
///
/// Updated documents appear in both sets to ensure clean updates: first the old data
/// is completely removed, then the document is re-indexed fresh with new data.
fn calculate_diff(
    incoming_documents: &[Document],
    db_documents: &HashMap<String, i64>,
) -> (HashSet<String>, HashSet<String>) {
    let incoming_docs_lookup: HashMap<&str, i64> = incoming_documents
        .iter()
        .map(|doc| (doc.uri.as_str(), doc.content_hash))
        .collect();

    let db_uris: HashSet<&str> = db_documents.keys().map(String::as_str).collect();
    let incoming_document_uris: HashSet<&str> = incoming_docs_lookup.keys().copied().collect();

    let new: HashSet<&str> = incoming_document_uris.difference(&db_uris).copied().collect();
    let deleted: HashSet<&str> = db_uris.difference(&incoming_document_uris).copied().collect();
    let updated: HashSet<&str> = db_uris
        .intersection(&incoming_document_uris)
        .filter_map(|&uri_str| {
            let incoming_hash = incoming_docs_lookup.get(uri_str)?;
            let db_hash = db_documents.get(uri_str)?;

            (incoming_hash != db_hash).then_some(uri_str)
        })
        .collect();

    let new_uris = new.union(&updated).map(|&s| s.to_string()).collect();
    let stale_uris = deleted.union(&updated).map(|&s| s.to_string()).collect();

    (new_uris, stale_uris)
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;
    use crate::model::ids::UriId;

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

    #[test]
    fn index_and_sync_works() {
        let mut graph = Graph::new();
        graph.db.initialize_connection(None).unwrap();

        // Set up initial state
        let initial_documents = vec![
            Document::new("file:///existing.rb", Some("module Existing; end".to_string())).unwrap(),
            Document::new("file:///updated.rb", Some("class OldVersion; end".to_string())).unwrap(),
            Document::new("file:///to_delete.rb", Some("class ToDelete; end".to_string())).unwrap(),
        ];
        index_and_sync(&mut graph, initial_documents).unwrap();

        let initial_db_docs = graph.db.all_documents_hashes_by_uri().unwrap();
        assert_eq!(initial_db_docs.len(), 3);

        // Test sync with updated document set
        let incoming_documents = vec![
            Document::new("file:///new.rb", Some("module New; end".to_string())).unwrap(),
            Document::new("file:///existing.rb", Some("module Existing; end".to_string())).unwrap(), // Same content
            Document::new("file:///updated.rb", Some("class NewVersion; end".to_string())).unwrap(), // Updated content
        ];

        index_and_sync(&mut graph, incoming_documents).unwrap();

        let result_db_docs = graph.db.all_documents_hashes_by_uri().unwrap();
        assert_eq!(result_db_docs.len(), 3);
        assert!(result_db_docs.contains_key("file:///new.rb"));
        assert!(result_db_docs.contains_key("file:///existing.rb"));
        assert!(result_db_docs.contains_key("file:///updated.rb"));

        assert!(
            graph
                .db
                .load_uri(UriId::from("file:///new.rb"))
                .unwrap()
                .iter()
                .any(|result| result.name == "New")
        );
        assert!(
            graph
                .db
                .load_uri(UriId::from("file:///existing.rb"))
                .unwrap()
                .iter()
                .any(|result| result.name == "Existing")
        );
        assert!(
            graph
                .db
                .load_uri(UriId::from("file:///updated.rb"))
                .unwrap()
                .iter()
                .any(|result| result.name == "NewVersion")
        );

        graph.assert_integrity();

        let documents: Vec<_> = graph.documents().values().collect();
        assert_eq!(documents.len(), 3);
        assert!(documents.iter().any(|doc| doc.uri() == "file:///new.rb"));
        assert!(documents.iter().any(|doc| doc.uri() == "file:///existing.rb"));
        assert!(documents.iter().any(|doc| doc.uri() == "file:///updated.rb"));
    }
}
