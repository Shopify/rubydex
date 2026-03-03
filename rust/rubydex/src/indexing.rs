use crate::{
    errors::Errors,
    indexing::{local_graph::LocalGraph, rbs_indexer::RBSIndexer, ruby_indexer::RubyIndexer},
    job_queue::{Job, JobQueue},
    model::graph::Graph,
};
use crossbeam_channel::{Sender, unbounded};
use std::{ffi::OsStr, fs, path::PathBuf, sync::Arc};
use url::Url;

pub mod local_graph;
pub mod rbs_indexer;
pub mod ruby_indexer;

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
    local_graph_tx: Sender<LocalGraph>,
    errors_tx: Sender<Errors>,
}

impl IndexingJob {
    #[must_use]
    pub fn new(path: PathBuf, local_graph_tx: Sender<LocalGraph>, errors_tx: Sender<Errors>) -> Self {
        Self {
            path,
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

        let language = self.path.extension().map_or(LanguageId::Ruby, LanguageId::from);
        let local_graph = build_local_graph(url.to_string(), &source, &language);

        self.local_graph_tx
            .send(local_graph)
            .expect("graph receiver dropped before merge");
    }
}

/// Indexes a single source string in memory, dispatching to the appropriate indexer based on `language_id`.
pub fn index_source(graph: &mut Graph, uri: &str, source: &str, language_id: &LanguageId) {
    let local_graph = build_local_graph(uri.to_string(), source, language_id);
    graph.update(local_graph);
}

/// Indexes the given paths, reading the content from disk and populating the given `Graph` instance.
/// Pending work is accumulated on the graph; drained by the resolver.
///
/// # Panics
///
/// Will panic if the graph cannot be wrapped in an Arc<Mutex<>>
pub fn index_files(graph: &mut Graph, paths: Vec<PathBuf>) -> Vec<Errors> {
    let queue = Arc::new(JobQueue::new());
    let (local_graphs_tx, local_graphs_rx) = unbounded();
    let (errors_tx, errors_rx) = unbounded();

    for path in paths {
        queue.push(Box::new(IndexingJob::new(
            path,
            local_graphs_tx.clone(),
            errors_tx.clone(),
        )));
    }

    drop(local_graphs_tx);
    drop(errors_tx);

    JobQueue::run(&queue);

    while let Ok(local_graph) = local_graphs_rx.recv() {
        graph.update(local_graph);
    }

    errors_rx.iter().collect()
}

/// Indexes a source string using the appropriate indexer for the given language.
fn build_local_graph(uri: String, source: &str, language: &LanguageId) -> LocalGraph {
    match language {
        LanguageId::Ruby => {
            let mut indexer = RubyIndexer::new(uri, source);
            indexer.index();
            indexer.local_graph()
        }
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
        let errors = index_files(&mut graph, vec![relative_to_pwd.clone()]);

        assert!(errors.is_empty());
        assert_eq!(graph.documents().len(), 1);
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
        let errors = index_files(&mut graph, vec![path]);

        assert!(errors.is_empty(), "Expected no errors, got: {errors:#?}");
        assert_eq!(1, graph.definitions().len());
        assert_eq!(1, graph.documents().len());

        index_source(&mut graph, &uri, "", &LanguageId::Ruby);

        assert_eq!(0, graph.definitions().len());
        assert_eq!(1, graph.documents().len());
    }
}
