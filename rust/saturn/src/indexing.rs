use crate::{
    errors::{Errors, MultipleErrors},
    indexing::ruby_indexer::{IndexerParts, RubyIndexer},
    model::{graph::Graph, identity_maps::IdentityHashSet, ids::UriId},
    source_location::UTF8SourceLocationConverter,
};
use glob::glob;
use std::sync::{
    Arc, Mutex,
    mpsc::{Receiver, Sender},
};
use std::{fs, path::Path};
use std::{sync::mpsc, thread};
use url::Url;
use xxhash_rust::xxh3::xxh3_64;

pub mod ruby_indexer;
pub mod scope;

pub enum IndexResult {
    Completed(Box<IndexerParts>),
    Skipped,
    Errored(Vec<Errors>),
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
pub fn index_in_parallel(graph: &mut Graph, file_paths: Vec<String>) -> Result<(), MultipleErrors> {
    if graph.is_db_initialized() {
        return index_and_sync(graph, file_paths);
    }

    let index_document = |file_path: &String| -> IndexResult {
        let (source, errors) = read_document_source(file_path);
        if !errors.is_empty() {
            return IndexResult::Errored(errors);
        }

        let uri = match uri_from_file_path(file_path) {
            Ok(uri) => uri,
            Err(error) => return IndexResult::Errored(vec![error]),
        };

        let converter = UTF8SourceLocationConverter::new(&source);
        let content_hash = calculate_content_hash(source.as_bytes());
        let mut ruby_indexer = RubyIndexer::new(uri, &converter, &source, content_hash);
        ruby_indexer.index();
        IndexResult::Completed(Box::new(ruby_indexer.into_parts()))
    };

    let merge_result = |local_graph| graph.update(local_graph);

    with_parallel_workers(file_paths, index_document, merge_result)
}

/// Indexes the given items and update the db entries.
///
/// # Errors
///
/// Returns Ok if indexing succeeded for all given documents, and the db operations are successful.
/// Or else a vector of errors for all failures will be returned.
///
/// # Panics
/// This function will panic in the event of a thread dead lock, which indicates a bug in our implementation. There
/// should not be any code that tries to lock the same mutex multiple times in the same thread
fn index_and_sync(graph: &mut Graph, file_paths: Vec<String>) -> Result<(), MultipleErrors> {
    let cached_hashes = graph.get_all_cached_content_hashes().map_err(MultipleErrors::from)?;
    let cached_hashes = Arc::new(cached_hashes);

    let index_document = {
        let cached_hashes = Arc::clone(&cached_hashes);
        move |file_path: &String| -> IndexResult {
            let (source, errors) = read_document_source(file_path);
            if !errors.is_empty() {
                return IndexResult::Errored(errors);
            }

            let content_hash = calculate_content_hash(source.as_bytes());
            let uri = match uri_from_file_path(file_path) {
                Ok(uri) => uri,
                Err(error) => return IndexResult::Errored(vec![error]),
            };
            let uri_id = UriId::from(uri.as_str());
            if cached_hashes
                .get(&uri_id)
                .is_some_and(|db_content_hash| db_content_hash == &content_hash)
            {
                return IndexResult::Skipped;
            }

            let converter = UTF8SourceLocationConverter::new(&source);
            let mut ruby_indexer = RubyIndexer::new(uri, &converter, &source, content_hash);
            ruby_indexer.index();
            IndexResult::Completed(Box::new(ruby_indexer.into_parts()))
        }
    };

    let mut deleted_uris: IdentityHashSet<UriId> = cached_hashes.keys().copied().collect();
    for file_path in &file_paths {
        if let Ok(uri) = uri_from_file_path(file_path) {
            let uri_id = UriId::from(uri.as_str());
            deleted_uris.remove(&uri_id);
        }
    }

    let merge_result = |local_graph: Graph| graph.update(local_graph);
    with_parallel_workers(file_paths, index_document, merge_result)?;

    graph.sync_with_database(deleted_uris.into_iter().collect())?;

    Ok(())
}

/// Reads the source content from a document, either from memory or disk
fn read_document_source(file_path: &String) -> (String, Vec<Errors>) {
    let mut errors = Vec::new();

    let source = fs::read_to_string(file_path).unwrap_or_else(|e| {
        errors.push(Errors::FileReadError(format!("Failed to read {file_path}: {e}")));
        String::new()
    });

    (source, errors)
}

#[must_use]
pub fn calculate_content_hash(source: &[u8]) -> u16 {
    // Explicitly take only the lower 16 bits of the hash
    // This is intentional as this is only used for document staleness check
    (xxh3_64(source) & 0xFFFF) as u16
}

fn uri_from_file_path(path: &str) -> Result<String, Errors> {
    Ok(Url::from_file_path(path)
        .map_err(|_e| Errors::InvalidUri(format!("Couldn't build URI from path '{path}'")))?
        .to_string())
}

fn with_parallel_workers<F, G>(file_paths: Vec<String>, worker_fn: F, mut result_fn: G) -> Result<(), MultipleErrors>
where
    F: Fn(&String) -> IndexResult + Send + Clone + 'static,
    G: FnMut(Graph),
{
    let (tx, rx): (Sender<IndexResult>, Receiver<IndexResult>) = mpsc::channel();
    let num_threads = thread::available_parallelism().map(std::num::NonZero::get).unwrap_or(4);
    let mut threads = Vec::with_capacity(num_threads);
    let document_queue = Arc::new(Mutex::new(file_paths));

    for _ in 0..num_threads {
        let thread_tx = tx.clone();
        let queue = Arc::clone(&document_queue);
        let thread_fn = worker_fn.clone();

        let handle = thread::spawn(move || {
            while let Some(document) = { queue.lock().unwrap().pop() } {
                match thread_fn(&document) {
                    IndexResult::Skipped => {}
                    result => {
                        thread_tx
                            .send(result)
                            .expect("Receiver end should not be closed until all threads are done");
                    }
                }
            }
        });

        threads.push(handle);
    }

    drop(tx);

    let mut all_errors = Vec::new();
    for result in rx {
        match result {
            IndexResult::Completed(parts) => {
                let (local_graph, errors) = *parts;
                result_fn(local_graph);
                all_errors.extend(errors);
            }
            IndexResult::Errored(errors) => {
                all_errors.extend(errors);
            }
            IndexResult::Skipped => {
                unreachable!("We should not return information about skipped files");
            }
        }
    }

    if all_errors.is_empty() {
        Ok(())
    } else {
        Err(MultipleErrors(all_errors))
    }
}

/// Recursively collects all Ruby files for the given workspace and dependencies, returning a vector of file paths
///
/// # Panics
///
/// Panics if there's a bug in how we're handling the arc mutex, like trying to acquire locks twice
#[must_use]
pub fn collect_file_paths(paths: Vec<String>) -> (Vec<String>, Vec<Errors>) {
    let mut errors = Vec::new();
    let mut file_paths = Vec::new();

    for path in paths {
        let path_obj = Path::new(&path);

        if path_obj.is_dir() {
            match glob(&format!("{path}/**/*.rb")) {
                Ok(entries) => {
                    for entry in entries {
                        match entry {
                            Ok(path) => file_paths.push(path.to_string_lossy().into_owned()),
                            Err(e) => errors.push(Errors::FileReadError(format!(
                                "Failed to read glob entry in '{path}': {e}"
                            ))),
                        }
                    }
                }
                Err(e) => {
                    errors.push(Errors::FileReadError(format!(
                        "Failed to read glob pattern '{path}/**/*.rb': {e}"
                    )));
                }
            }

            continue;
        }

        if path_obj.exists() {
            file_paths.push(path);

            continue;
        }

        errors.push(Errors::FileReadError(format!("Path '{path}' does not exist")));
    }

    (file_paths, errors)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::Context;
    use std::fs;

    fn collect_document_paths(context: &Context, paths: &[&str]) -> (Vec<String>, Vec<Errors>) {
        let (file_paths, errors) = collect_file_paths(
            paths
                .iter()
                .map(|p| context.absolute_path_to(p).to_string_lossy().into_owned())
                .collect(),
        );

        let mut paths: Vec<String> = file_paths
            .iter()
            .map(|path| context.relative_path_to(path).to_string_lossy().into_owned())
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

        let (documents, errors) = collect_file_paths(vec![
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
    fn index_in_parallel_persists_and_skips_unchanged_documents() {
        use rusqlite::Connection;

        let db_path =
            "file:index_in_parallel_persists_and_skips_unchanged_documents?mode=memory&cache=shared".to_string();
        let source = "module Foo\n  def bar; end\nend";

        let temp_dir = tempfile::tempdir().unwrap();
        let file_path = temp_dir.path().join("persist_and_skip.rb");
        fs::write(&file_path, source).unwrap();
        let file_path = file_path.to_string_lossy().into_owned();

        let mut first_graph = Graph::new();
        first_graph.set_configuration(db_path.clone()).unwrap();
        index_in_parallel(&mut first_graph, vec![file_path.clone()]).unwrap();

        let conn = Connection::open(&db_path).unwrap();
        let initial_document_count: i64 = conn
            .prepare("SELECT COUNT(*) FROM documents")
            .unwrap()
            .query_row([], |row| row.get(0))
            .unwrap();
        assert_eq!(initial_document_count, 1,);

        let initial_definition_count: i64 = conn
            .prepare("SELECT COUNT(*) FROM definitions")
            .unwrap()
            .query_row([], |row| row.get(0))
            .unwrap();
        assert_eq!(initial_definition_count, 2,);
        drop(conn);

        let mut second_graph = Graph::new();
        second_graph.set_configuration(db_path.clone()).unwrap();
        index_in_parallel(&mut second_graph, vec![file_path.clone()]).unwrap();

        assert!(
            second_graph.documents().is_empty(),
            "unchanged files should be skipped during indexing"
        );
        assert!(second_graph.definitions().is_empty());
        assert_eq!(
            second_graph.declarations().len(),
            1,
            "only the <main> declaration should remain after skipping"
        );

        let conn = Connection::open(&db_path).unwrap();
        let final_document_count: i64 = conn
            .prepare("SELECT COUNT(*) FROM documents")
            .unwrap()
            .query_row([], |row| row.get(0))
            .unwrap();
        assert_eq!(final_document_count, 1, "database should retain the original document");

        let final_definition_count: i64 = conn
            .prepare("SELECT COUNT(*) FROM definitions")
            .unwrap()
            .query_row([], |row| row.get(0))
            .unwrap();
        assert_eq!(
            final_definition_count, 2,
            "definitions should remain available after skipping unchanged content"
        );
    }

    #[test]
    fn index_in_parallel_removes_cached_uris_from_db() {
        use rusqlite::Connection;

        let temp_db_path = "file:index_in_parallel_removes_cached_uris_from_db?mode=memory&cache=shared".to_string();
        let ruby_source = "module TestModule\nend";

        let temp_dir = tempfile::tempdir().unwrap();
        let file_path = temp_dir.path().join("test.rb");
        fs::write(&file_path, ruby_source).unwrap();
        let file_path = file_path.to_string_lossy().into_owned();

        let mut first_graph = Graph::new();
        first_graph.set_configuration(temp_db_path.clone()).unwrap();

        index_in_parallel(&mut first_graph, vec![file_path.clone()]).unwrap();

        let mut second_graph = Graph::new();
        second_graph.set_configuration(temp_db_path.clone()).unwrap();

        index_in_parallel(&mut second_graph, vec![]).unwrap();

        let conn = Connection::open(&temp_db_path).unwrap();
        let doc_count: i64 = conn
            .prepare("SELECT COUNT(*) FROM documents")
            .unwrap()
            .query_row([], |row| row.get(0))
            .unwrap();
        assert_eq!(doc_count, 0, "documents should be removed from the database");

        let def_count: i64 = conn
            .prepare("SELECT COUNT(*) FROM definitions")
            .unwrap()
            .query_row([], |row| row.get(0))
            .unwrap();
        assert_eq!(def_count, 0);
    }

    #[test]
    fn index_in_parallel_persists_updated_entries() {
        use rusqlite::Connection;

        let db_path = "file:index_in_parallel_persists_updated_entries?mode=memory&cache=shared".to_string();
        let initial_source = "module Foo; end";
        let updated_source = "module Foo\n  def bar; end\nend";

        let temp_dir = tempfile::tempdir().unwrap();
        let file_path = temp_dir.path().join("foo.rb");
        fs::write(&file_path, initial_source).unwrap();
        let file_path = file_path.to_string_lossy().into_owned();

        let mut graph = Graph::new();
        graph.set_configuration(db_path.clone()).unwrap();
        index_in_parallel(&mut graph, vec![file_path.clone()]).unwrap();

        let uri = uri_from_file_path(&file_path).unwrap();
        let uri_id = UriId::from(uri.as_str());
        let conn = Connection::open(&db_path).unwrap();
        let initial_hash: i64 = conn
            .prepare("SELECT content_hash FROM documents WHERE id = ?")
            .unwrap()
            .query_row([*uri_id], |row| row.get(0))
            .unwrap();
        assert_eq!(
            initial_hash,
            i64::from(calculate_content_hash(initial_source.as_bytes()))
        );

        drop(conn);

        fs::write(&file_path, updated_source).unwrap();

        let mut graph = Graph::new();
        graph.set_configuration(db_path.clone()).unwrap();
        index_in_parallel(&mut graph, vec![file_path.clone()]).unwrap();

        let conn = Connection::open(&db_path).unwrap();
        let updated_hash: i64 = conn
            .prepare("SELECT content_hash FROM documents WHERE id = ?")
            .unwrap()
            .query_row([*uri_id], |row| row.get(0))
            .unwrap();
        assert_eq!(
            updated_hash,
            i64::from(calculate_content_hash(updated_source.as_bytes()))
        );

        let updated_row_exists: i64 = conn
            .prepare("SELECT COUNT(*) FROM documents WHERE id = ? AND content_hash = ?")
            .unwrap()
            .query_row(
                [*uri_id, i64::from(calculate_content_hash(updated_source.as_bytes()))],
                |row| row.get(0),
            )
            .unwrap();
        assert_eq!(updated_row_exists, 1);

        let definition_count: i64 = conn
            .prepare("SELECT COUNT(*) FROM definitions")
            .unwrap()
            .query_row([], |row| row.get(0))
            .unwrap();
        assert!(definition_count >= 1);

        let declaration_count: i64 = conn
            .prepare("SELECT COUNT(*) FROM declarations")
            .unwrap()
            .query_row([], |row| row.get(0))
            .unwrap();
        assert!(declaration_count >= 1);
    }

    #[test]
    fn index_in_parallel_handles_duplicate_declarations() {
        use rusqlite::Connection;

        let db_path = "file:index_in_parallel_handles_duplicate_declarations?mode=memory&cache=shared".to_string();

        let temp_dir = tempfile::tempdir().unwrap();
        let first_path = temp_dir.path().join("existing.rb");
        fs::write(&first_path, "module Foo; end").unwrap();
        let second_path = temp_dir.path().join("second.rb");
        fs::write(&second_path, "module Foo; end").unwrap();
        let first_path = first_path.to_string_lossy().into_owned();
        let second_path = second_path.to_string_lossy().into_owned();

        let mut initial_graph = Graph::new();
        initial_graph.set_configuration(db_path.clone()).unwrap();
        index_in_parallel(&mut initial_graph, vec![first_path.clone()]).unwrap();

        let conn = Connection::open(&db_path).unwrap();
        let existing_declarations: i64 = conn
            .prepare("SELECT COUNT(*) FROM declarations")
            .unwrap()
            .query_row([], |row| row.get(0))
            .unwrap();
        // two declarations here because we also have <main>
        assert_eq!(existing_declarations, 2);
        drop(conn);

        let mut graph = Graph::new();
        graph.set_configuration(db_path.clone()).unwrap();
        index_in_parallel(&mut graph, vec![first_path, second_path]).unwrap();

        let conn = Connection::open(&db_path).unwrap();

        let doc_count: i64 = conn
            .prepare("SELECT COUNT(*) FROM documents")
            .unwrap()
            .query_row([], |row| row.get(0))
            .unwrap();
        assert_eq!(doc_count, 2, "both documents should be persisted");

        let declaration_count: i64 = conn
            .prepare("SELECT COUNT(*) FROM declarations")
            .unwrap()
            .query_row([], |row| row.get(0))
            .unwrap();
        assert_eq!(declaration_count, 2, "duplicate declarations should be ignored");

        let definition_count: i64 = conn
            .prepare("SELECT COUNT(*) FROM definitions")
            .unwrap()
            .query_row([], |row| row.get(0))
            .unwrap();
        assert_eq!(definition_count, 2, "each document should have a definition row");
    }
}
