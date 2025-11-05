use crate::{
    errors::{Errors, MultipleErrors},
    indexing::ruby_indexer::{IndexerParts, RubyIndexer},
    model::graph::Graph,
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
    let index_document = |file_path: &String| -> IndexerParts {
        let (source, errors) = read_document_source(file_path);
        if !errors.is_empty() {
            return (Graph::new(), errors);
        }

        let mut ruby_indexer = RubyIndexer::new(uri_from_file_path(file_path).unwrap(), &source);
        ruby_indexer.index();
        ruby_indexer.into_parts()
    };

    let merge_result = |local_graph| graph.update(local_graph);

    with_parallel_workers(file_paths, index_document, merge_result)
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
    // Resolve the path to an absolute path
    let path =
        fs::canonicalize(path).map_err(|_e| Errors::FileReadError(format!("Failed to canonicalize path '{path}'")))?;
    Ok(Url::from_file_path(&path)
        .map_err(|_e| Errors::InvalidUri(format!("Couldn't build URI from path '{:?}'", &path.display())))?
        .to_string())
}

fn with_parallel_workers<F, G>(file_paths: Vec<String>, worker_fn: F, mut result_fn: G) -> Result<(), MultipleErrors>
where
    F: Fn(&String) -> IndexerParts + Send + Clone + 'static,
    G: FnMut(Graph),
{
    let (tx, rx): (Sender<IndexerParts>, Receiver<IndexerParts>) = mpsc::channel();
    let num_threads = thread::available_parallelism().map(std::num::NonZero::get).unwrap_or(4);
    let mut threads = Vec::with_capacity(num_threads);
    let document_queue = Arc::new(Mutex::new(file_paths));

    for _ in 0..num_threads {
        let thread_tx = tx.clone();
        let queue = Arc::clone(&document_queue);
        let thread_fn = worker_fn.clone();

        let handle = thread::spawn(move || {
            while let Some(document) = { queue.lock().unwrap().pop() } {
                let result = thread_fn(&document);
                thread_tx
                    .send(result)
                    .expect("Receiver end should not be closed until all threads are done");
            }
        });

        threads.push(handle);
    }

    drop(tx);

    let mut all_errors = Vec::new();
    for result in rx {
        let (local_graph, errors) = result;
        result_fn(local_graph);
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
    fn index_relative_paths() {
        let context = Context::new();
        context.touch("foo/bar.rb");

        let working_directory = std::env::current_dir().unwrap();

        let dotdots = "../".repeat(working_directory.components().count());

        let relative_path = dotdots
            + context
                .absolute_path_to("foo/bar.rb")
                .to_string_lossy()
                .into_owned()
                .as_str();

        let mut graph = Graph::new();
        let errors = index_in_parallel(&mut graph, vec![relative_path]);

        assert!(errors.is_ok());
        assert_eq!(graph.documents().len(), 1);
    }
}
