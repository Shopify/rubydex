use crate::{
    config::Config,
    errors::Errors,
    indexing::{local_graph::LocalGraph, rbs_indexer::RBSIndexer, ruby_indexer::RubyIndexer},
    job_queue::{Job, JobQueue},
    model::graph::Graph,
    operation::ruby_builder::RubyOperationBuilder,
};
use crossbeam_channel::{Sender, unbounded};
use std::{ffi::OsStr, fs, path::PathBuf, sync::Arc};
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
    config: Arc<Config>,
    local_graph_tx: Sender<LocalGraph>,
    errors_tx: Sender<Errors>,
}

impl IndexingJob {
    #[must_use]
    pub fn new(
        path: PathBuf,
        backend: IndexerBackend,
        config: Arc<Config>,
        local_graph_tx: Sender<LocalGraph>,
        errors_tx: Sender<Errors>,
    ) -> Self {
        Self {
            path,
            backend,
            config,
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
        let local_graph = build_local_graph(
            url.to_string(),
            &source,
            &language,
            self.backend,
            Arc::clone(&self.config),
        );

        self.local_graph_tx
            .send(local_graph)
            .expect("graph receiver dropped before merge");
    }
}

/// Indexes a single source string in memory, dispatching to the appropriate indexer based on `language_id`.
pub fn index_source(graph: &mut Graph, uri: &str, source: &str, language_id: &LanguageId) {
    let local_graph = build_local_graph(
        uri.to_string(),
        source,
        language_id,
        IndexerBackend::RubyIndexer,
        graph.config(),
    );
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
    let config = graph.config();

    for path in paths {
        queue.push(Box::new(IndexingJob::new(
            path,
            backend,
            Arc::clone(&config),
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
pub fn build_local_graph(
    uri: String,
    source: &str,
    language: &LanguageId,
    backend: IndexerBackend,
    config: Arc<Config>,
) -> LocalGraph {
    match language {
        LanguageId::Ruby => match backend {
            IndexerBackend::RubyIndexer => {
                let mut indexer = RubyIndexer::new_with_config(uri, source, config);
                indexer.index();
                indexer.local_graph()
            }
            IndexerBackend::OperationBuilder => {
                let builder = RubyOperationBuilder::new_with_config(uri, source, config);
                let result = builder.build();
                crate::operation::applier::apply_operations(result)
            }
        },
        LanguageId::Rbs => {
            let mut indexer = RBSIndexer::new_with_config(uri, source, config);
            indexer.index();
            indexer.local_graph()
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::{Path, PathBuf};

    use super::*;
    use crate::config::Config;
    use crate::diagnostic::{Rule, Severity};
    use crate::model::ids::UriId;
    use crate::resolution::Resolver;
    use crate::test_utils::Context;

    fn graph_with_config(workspace: &Path, content: &str) -> Graph {
        fs::write(workspace.join("rubydex.toml"), content).unwrap();
        let config = Config::load(workspace).unwrap();
        let mut graph = Graph::new();
        graph.load_config(&config);
        graph
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

    #[test]
    fn single_source_indexing_does_not_store_disabled_graph_diagnostics() {
        let workspace = tempfile::tempdir().unwrap();
        let mut graph = graph_with_config(workspace.path(), "[linter.rules.parse-warning]\nenabled = false\n");
        let path = workspace.path().join("warning.rb");
        let uri = Url::from_file_path(path).unwrap().to_string();

        index_source(&mut graph, &uri, "foo = 42", &LanguageId::Ruby);

        assert!(
            graph
                .all_diagnostics()
                .iter()
                .all(|diagnostic| diagnostic.rule() != &Rule::ParseWarning)
        );
    }

    #[test]
    fn parallel_indexing_applies_workspace_excludes_and_severity_to_graph_diagnostics() {
        let workspace = tempfile::tempdir().unwrap();
        let workspace_path = crate::path_helpers::resolved(workspace.path()).unwrap();
        let excluded_path = workspace_path.join("components/legacy/warning.rb");
        let included_path = workspace_path.join("components/current/warning.rb");
        fs::create_dir_all(excluded_path.parent().unwrap()).unwrap();
        fs::create_dir_all(included_path.parent().unwrap()).unwrap();
        fs::write(&excluded_path, "foo = 42").unwrap();
        fs::write(&included_path, "foo = 42").unwrap();
        let included_uri = Url::from_file_path(&included_path).unwrap().to_string();

        for backend in [IndexerBackend::RubyIndexer, IndexerBackend::OperationBuilder] {
            let mut graph = graph_with_config(
                workspace.path(),
                "[linter.rules.parse-warning]\nexclude = [\"components/{legacy,generated}/**\"]\nseverity = \"hint\"\n",
            );
            let errors = index_files(&mut graph, vec![excluded_path.clone(), included_path.clone()], backend);
            assert!(errors.is_empty(), "unexpected indexing errors: {errors:?}");

            let diagnostics: Vec<_> = graph
                .all_diagnostics()
                .into_iter()
                .filter(|diagnostic| diagnostic.rule() == &Rule::ParseWarning)
                .collect();
            assert_eq!(1, diagnostics.len(), "unexpected diagnostics for {backend:?}");
            assert_eq!(&UriId::from(included_uri.as_str()), diagnostics[0].uri_id());
            assert_eq!(&Severity::Hint, diagnostics[0].severity());
        }
    }

    #[test]
    fn resolution_diagnostics_use_the_same_graph_configuration() {
        let source = "class Foo\n  private :nonexistent\nend";

        for (settings, expected_severity) in [
            ("enabled = false", None),
            ("severity = \"information\"", Some(Severity::Information)),
        ] {
            let workspace = tempfile::tempdir().unwrap();
            let path = workspace.path().join("foo.rb");
            let uri = Url::from_file_path(path).unwrap().to_string();
            let config = format!("[linter.rules.undefined-method-visibility-target]\n{settings}\n");
            fs::write(workspace.path().join("rubydex.toml"), config).unwrap();
            let config = Config::load(workspace.path()).unwrap();

            for backend in [IndexerBackend::RubyIndexer, IndexerBackend::OperationBuilder] {
                let mut graph = Graph::new();
                let local_graph = build_local_graph(uri.clone(), source, &LanguageId::Ruby, backend, graph.config());
                graph.consume_document_changes(local_graph);
                graph.load_config(&config);
                Resolver::new(&mut graph).resolve();

                let diagnostics: Vec<_> = graph
                    .all_diagnostics()
                    .into_iter()
                    .filter(|diagnostic| diagnostic.rule() == &Rule::UndefinedMethodVisibilityTarget)
                    .collect();
                match expected_severity {
                    Some(severity) => {
                        assert_eq!(1, diagnostics.len(), "unexpected diagnostics for {backend:?}");
                        assert_eq!(&severity, diagnostics[0].severity());
                    }
                    None => assert!(diagnostics.is_empty(), "unexpected diagnostics for {backend:?}"),
                }
            }
        }
    }
}
