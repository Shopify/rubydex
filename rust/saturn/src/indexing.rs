use crate::{
    diagnostic::Code,
    indexing::{local_graph::LocalGraph, ruby_indexer::RubyIndexer},
    model::{document::Document, graph::Graph, ids::UriId},
    offset::Offset,
};
use glob::glob;
use std::sync::{
    Arc, Mutex,
    mpsc::{Receiver, Sender},
};
use std::{fs, path::Path};
use std::{sync::mpsc, thread};
use url::Url;

pub mod local_graph;
pub mod nesting_stack;
pub mod ruby_indexer;

/// Indexes the given items, reading the content from disk and populating the given `Graph` instance.
///
/// # Errors
///
/// Returns Ok if indexing succeeded for all given documents or a vector of errors for all failures
///
/// # Panics
/// This function will panic in the event of a thread dead lock, which indicates a bug in our implementation. There
/// should not be any code that tries to lock the same mutex multiple times in the same thread
pub fn index_in_parallel(graph: &mut Graph, file_paths: Vec<String>) {
    let index_document = |file_path: &String| -> LocalGraph {
        let uri = uri_from_file_path(file_path).unwrap();
        let uri_id = UriId::from(&uri);

        let source = fs::read_to_string(file_path);

        if let Ok(source) = source {
            let mut ruby_indexer = RubyIndexer::new(uri, &source);
            ruby_indexer.index();
            return ruby_indexer.local_graph();
        }

        let error = source.unwrap_err();
        let mut empty_graph = LocalGraph::new(uri_id, Document::new(uri));
        empty_graph.add_diagnostic(
            &Code::FileReadError,
            Offset::new(0, 0),
            format!("Failed to read {file_path}: {error}"),
        );
        empty_graph
    };

    with_parallel_workers(graph, file_paths, index_document);
}

fn uri_from_file_path(path: &str) -> Result<String, String> {
    // Resolve the path to an absolute path
    let path = fs::canonicalize(path).map_err(|_e| format!("Failed to canonicalize path '{path}'"))?;
    Ok(Url::from_file_path(&path)
        .map_err(|_e| format!("Couldn't build URI from path '{:?}'", &path.display()))?
        .to_string())
}

fn with_parallel_workers<F>(graph: &mut Graph, file_paths: Vec<String>, worker_fn: F)
where
    F: Fn(&String) -> LocalGraph + Send + Clone + 'static,
{
    let (tx, rx): (Sender<LocalGraph>, Receiver<LocalGraph>) = mpsc::channel();
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

    for local_graph in rx {
        graph.update(local_graph);
    }
}

/// Recursively collects all Ruby files for the given workspace and dependencies, returning a vector of document instances
///
/// # Panics
///
/// Panics if there's a bug in how we're handling the arc mutex, like trying to acquire locks twice
#[must_use]
pub fn collect_file_paths(graph: &mut Graph, paths: Vec<String>) -> Vec<String> {
    let mut file_paths = Vec::new();

    for path in paths {
        let path_obj = Path::new(&path);

        if path_obj.is_dir() {
            match glob(&format!("{path}/**/*.rb")) {
                Ok(entries) => {
                    for entry in entries {
                        match entry {
                            Ok(path) => file_paths.push(path.to_string_lossy().into_owned()),
                            Err(e) => {
                                let uri_id = graph.add_document(format!("file:///{path}"));
                                graph.add_diagnostic(
                                    &Code::PathError,
                                    uri_id,
                                    Offset::none(),
                                    format!("Failed to read glob entry in '{path}': {e}"),
                                );
                            }
                        }
                    }
                }
                Err(e) => {
                    let uri_id = graph.add_document(format!("file:///{path}"));
                    graph.add_diagnostic(
                        &Code::PathError,
                        uri_id,
                        Offset::none(),
                        format!("Failed to read glob pattern '{path}/**/*.rb': {e}"),
                    );
                }
            }

            continue;
        }

        if path_obj.exists() {
            file_paths.push(path);

            continue;
        }

        let uri_id = graph.add_document(format!("file:///{path}"));
        graph.add_diagnostic(
            &Code::PathError,
            uri_id,
            Offset::none(),
            format!("Path '{path}' does not exist"),
        );
    }

    file_paths
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;
    use crate::test_utils::Context;

    fn collect_document_paths(graph: &mut Graph, context: &Context, paths: &[&str]) -> Vec<String> {
        let file_paths = collect_file_paths(
            graph,
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

        paths
    }

    #[test]
    fn collect_all_documents() {
        let context = Context::new();

        let baz = Path::new("bar").join("baz.rb");
        let qux = Path::new("bar").join("qux.rb");
        let bar = Path::new("foo").join("bar.rb");

        context.touch(&baz);
        context.touch(&qux);
        context.touch(&bar);

        let mut graph = Graph::new();
        let paths = collect_document_paths(&mut graph, &context, &["foo", "bar"]);

        assert_eq!(
            paths,
            vec![
                baz.to_str().unwrap().to_string(),
                qux.to_str().unwrap().to_string(),
                bar.to_str().unwrap().to_string()
            ]
        );
        assert!(graph.diagnostics().is_empty());
    }

    #[test]
    fn collect_some_documents_based_on_paths() {
        let context = Context::new();
        let baz = Path::new("bar").join("baz.rb");
        let qux = Path::new("bar").join("qux.rb");
        let bar = Path::new("foo").join("bar.rb");

        context.touch(&baz);
        context.touch(&qux);
        context.touch(&bar);

        let mut graph = Graph::new();
        let paths = collect_document_paths(&mut graph, &context, &["bar"]);

        assert_eq!(
            paths,
            vec![baz.to_str().unwrap().to_string(), qux.to_str().unwrap().to_string()]
        );
        assert!(graph.diagnostics().is_empty());
    }

    #[test]
    fn collect_non_existing_paths() {
        let context = Context::new();

        let mut graph = Graph::new();
        let _paths = collect_file_paths(
            &mut graph,
            vec![
                context
                    .absolute_path_to("non_existing_path")
                    .to_string_lossy()
                    .into_owned(),
            ],
        );

        assert_eq!(
            graph
                .diagnostics()
                .iter()
                .map(|d| d.message().to_string())
                .collect::<Vec<String>>(),
            vec![format!(
                "Path '{}' does not exist",
                context.absolute_path_to("non_existing_path").display()
            )]
        );
    }

    #[test]
    fn index_relative_paths() {
        let relative_path = Path::new("foo").join("bar.rb");
        let context = Context::new();
        context.touch(&relative_path);

        let working_directory = std::env::current_dir().unwrap();
        let absolute_path = context.absolute_path_to("foo/bar.rb");

        let mut dots = PathBuf::from("..");

        for _ in 0..working_directory.components().count() - 1 {
            dots = dots.join("..");
        }

        let relative_to_pwd = &dots.join(absolute_path);

        let mut graph = Graph::new();
        index_in_parallel(&mut graph, vec![relative_to_pwd.to_str().unwrap().to_string()]);

        assert!(graph.diagnostics().is_empty());
        assert_eq!(graph.documents().len(), 1);
    }
}
