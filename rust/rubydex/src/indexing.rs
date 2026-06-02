use crate::{
    errors::Errors,
    indexing::{local_graph::LocalGraph, rbs_indexer::RBSIndexer, ruby_indexer::RubyIndexer},
    job_queue::{Job, JobQueue},
    model::graph::Graph,
    operation::ruby_builder::RubyOperationBuilder,
};
use crossbeam_channel::{Sender, unbounded};
use std::{
    ffi::OsStr,
    fs,
    path::{Path, PathBuf},
    sync::Arc,
};
use url::Url;

pub mod local_graph;
pub mod rbs_indexer;
pub mod ruby_indexer;

/// Which backend to use for indexing Ruby files.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IndexerBackend {
    /// The original tree-walking indexer.
    RubyIndexer,
    /// The two-phase operation builder + applier pipeline.
    OperationBuilder,
}

/// The language of a source document, used to dispatch to the appropriate indexer
pub enum LanguageId {
    Ruby,
    Rbs,
}

impl From<&OsStr> for LanguageId {
    fn from(ext: &OsStr) -> Self {
        if ext == "rbs" { Self::Rbs } else { Self::Ruby }
    }
}

impl LanguageId {
    pub fn from_path(path: impl AsRef<Path>) -> Self {
        path.as_ref().extension().map_or(Self::Ruby, Self::from)
    }

    /// Determines the language from an LSP language ID string.
    ///
    /// # Errors
    ///
    /// Returns an error if the language ID is not recognized.
    pub fn from_language_id(language_id: &str) -> Result<Self, Errors> {
        match language_id {
            "ruby" => Ok(Self::Ruby),
            "rbs" => Ok(Self::Rbs),
            _ => Err(Errors::FileError(format!("Unsupported language_id `{language_id}`"))),
        }
    }
}

/// Job that indexes a single file
pub struct IndexingJob {
    path: PathBuf,
    backend: IndexerBackend,
    local_graph_tx: Sender<LocalGraph>,
    errors_tx: Sender<Errors>,
}

impl IndexingJob {
    #[must_use]
    pub fn new(
        path: PathBuf,
        backend: IndexerBackend,
        local_graph_tx: Sender<LocalGraph>,
        errors_tx: Sender<Errors>,
    ) -> Self {
        Self {
            path,
            backend,
            local_graph_tx,
            errors_tx,
        }
    }

    fn send_error(&self, error: Errors) {
        self.errors_tx
            .send(error)
            .expect("errors receiver dropped before run completion");
    }
}

impl Job for IndexingJob {
    fn run(&self) {
        let Ok(source) = fs::read_to_string(&self.path) else {
            self.send_error(Errors::FileError(format!(
                "Failed to read file `{}`",
                self.path.display()
            )));

            return;
        };

        let Ok(url) = Url::from_file_path(&self.path) else {
            self.send_error(Errors::FileError(format!(
                "Couldn't build URI from path `{}`",
                self.path.display()
            )));

            return;
        };

        let language = LanguageId::from_path(&self.path);
        let local_graph = build_local_graph(url.to_string(), &source, &language, self.backend);

        self.local_graph_tx
            .send(local_graph)
            .expect("graph receiver dropped before merge");
    }
}

/// Indexes a single source string in memory, dispatching to the appropriate indexer based on `language_id`.
pub fn index_source(graph: &mut Graph, uri: &str, source: &str, language_id: &LanguageId) {
    let local_graph = build_local_graph(uri.to_string(), source, language_id, IndexerBackend::RubyIndexer);
    graph.consume_document_changes(local_graph);
}

/// Indexes the given paths, reading the content from disk and populating the given `Graph` instance.
///
/// # Panics
///
/// Will panic if the graph cannot be wrapped in an Arc<Mutex<>>
pub fn index_files(graph: &mut Graph, paths: Vec<PathBuf>, backend: IndexerBackend) -> Vec<Errors> {
    let queue = Arc::new(JobQueue::new());
    let (local_graphs_tx, local_graphs_rx) = unbounded();
    let (errors_tx, errors_rx) = unbounded();

    for path in paths {
        queue.push(Box::new(IndexingJob::new(
            path,
            backend,
            local_graphs_tx.clone(),
            errors_tx.clone(),
        )));
    }

    drop(local_graphs_tx);
    drop(errors_tx);

    let handles = JobQueue::run_without_waiting(&queue);

    // Merge graphs as they arrive, overlapping with indexing work on other threads.
    while let Ok(local_graph) = local_graphs_rx.recv() {
        graph.consume_document_changes(local_graph);
    }

    for handle in handles {
        handle.join().expect("Worker thread panicked");
    }

    errors_rx.iter().collect()
}

/// Indexes a source string using the appropriate indexer for the given language.
#[must_use]
pub fn build_local_graph(uri: String, source: &str, language: &LanguageId, backend: IndexerBackend) -> LocalGraph {
    match language {
        LanguageId::Ruby => match backend {
            IndexerBackend::RubyIndexer => {
                let mut indexer = RubyIndexer::new(uri, source);
                indexer.index();
                indexer.local_graph()
            }
            IndexerBackend::OperationBuilder => {
                let builder = RubyOperationBuilder::new(uri, source);
                let result = builder.build();
                crate::operation::applier::apply_operations(result)
            }
        },
        LanguageId::Rbs => {
            let mut indexer = RBSIndexer::new(uri, source);
            indexer.index();
            indexer.local_graph()
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;
    use crate::test_utils::Context;
    use std::path::Path;

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
        let errors = index_files(&mut graph, vec![relative_to_pwd.clone()], IndexerBackend::RubyIndexer);

        assert!(errors.is_empty());
        assert_eq!(graph.documents().len(), 2);
    }

    #[test]
    fn from_language_id_unknown() {
        let result = LanguageId::from_language_id("python");
        assert!(result.is_err());
    }

    #[test]
    fn updating_document_from_in_memory_source() {
        let context = Context::new();
        let path = context.absolute_path_to("foo/bar.rb");
        context.write(&path, "class Foo; end");

        let uri = Url::from_file_path(&path).unwrap().to_string();

        let mut graph = Graph::new();
        let errors = index_files(&mut graph, vec![path], IndexerBackend::RubyIndexer);

        assert!(errors.is_empty(), "Expected no errors, got: {errors:#?}");
        assert_eq!(6, graph.definitions().len());
        assert_eq!(2, graph.documents().len());

        index_source(&mut graph, &uri, "", &LanguageId::Ruby);

        assert_eq!(5, graph.definitions().len());
        assert_eq!(2, graph.documents().len());
    }
}
