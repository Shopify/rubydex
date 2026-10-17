use crate::{
    errors::Errors,
    indexing::{erb_indexer::ERBIndexer, local_graph::LocalGraph, rbs_indexer::RBSIndexer, ruby_indexer::RubyIndexer},
    job_queue::{Job, JobQueue},
    model::graph::Graph,
    operation::ruby_builder::RubyOperationBuilder,
};
use crossbeam_channel::{Sender, unbounded};
use std::{ffi::OsStr, fs, path::PathBuf, sync::Arc};
use url::Url;

pub mod erb_indexer;
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

/// The source context in which Prism diagnostics should be interpreted.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum RubySourceContext {
    File,
    ERBTemplate,
}

impl RubySourceContext {
    /// ERB templates are partial scripts, so top-level `yield`, `break`, `next` and `redo` are valid there. Prism's
    /// `partial_script` option skips exactly these checks, but the published `ruby-prism` crate doesn't expose parse
    /// options yet, so we filter the resulting errors instead.
    // TODO: Replace this with `ruby_prism::Options::default().partial_script(self == Self::ERBTemplate)` once a
    // `ruby-prism` release includes parse options.
    pub(crate) fn should_report_parse_error(self, message: &str) -> bool {
        self != Self::ERBTemplate
            || !matches!(
                message,
                "Invalid yield" | "Invalid break" | "Invalid next" | "Invalid redo"
            )
    }
}

/// The language of a source document, used to dispatch to the appropriate indexer
pub enum LanguageId {
    Ruby,
    ERB,
    Rbs,
}

impl From<&OsStr> for LanguageId {
    fn from(ext: &OsStr) -> Self {
        if ext == "rbs" {
            Self::Rbs
        } else if ext == "erb" {
            Self::ERB
        } else {
            Self::Ruby
        }
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
            "erb" => Ok(Self::ERB),
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

        let language = self.path.extension().map_or(LanguageId::Ruby, LanguageId::from);
        let local_graph = build_local_graph(url.to_string().into(), &source, &language, self.backend);

        self.local_graph_tx
            .send(local_graph)
            .expect("graph receiver dropped before merge");
    }
}

/// Indexes a single source string in memory, dispatching to the appropriate indexer based on `language_id`.
pub fn index_source(graph: &mut Graph, uri: Box<str>, source: &str, language_id: &LanguageId) {
    let local_graph = build_local_graph(uri, source, language_id, IndexerBackend::RubyIndexer);
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
pub fn build_local_graph(uri: Box<str>, source: &str, language: &LanguageId, backend: IndexerBackend) -> LocalGraph {
    match language {
        LanguageId::Ruby => build_ruby_local_graph(uri, source, source, backend, RubySourceContext::File),
        LanguageId::ERB => ERBIndexer::new(uri, source, backend).index(),
        LanguageId::Rbs => {
            let mut indexer = RBSIndexer::new(uri, source);
            indexer.index();
            indexer.local_graph()
        }
    }
}

pub(super) fn build_ruby_local_graph(
    uri: Box<str>,
    ruby_source: &str,
    document_source: &str,
    backend: IndexerBackend,
    source_context: RubySourceContext,
) -> LocalGraph {
    match backend {
        IndexerBackend::RubyIndexer => {
            let mut indexer =
                RubyIndexer::new_with_document_source_and_context(uri, ruby_source, document_source, source_context);
            indexer.index();
            indexer.local_graph()
        }
        IndexerBackend::OperationBuilder => {
            let builder = RubyOperationBuilder::new_with_document_source_and_context(
                uri,
                ruby_source,
                document_source,
                source_context,
            );
            let result = builder.build();
            crate::operation::applier::apply_operations(result)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;
    use crate::diagnostic::Rule;
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

        assert_eq!(errors.as_slice(), []);
        assert_eq!(graph.documents().len(), 2);
    }

    #[test]
    fn from_language_id_unknown() {
        let result = LanguageId::from_language_id("python");
        assert!(result.is_err());
    }

    #[test]
    fn recognizes_erb_language_ids_and_extensions() {
        assert!(matches!(LanguageId::from(OsStr::new("erb")), LanguageId::ERB));
        assert!(matches!(LanguageId::from_language_id("erb"), Ok(LanguageId::ERB)));
    }

    #[test]
    fn indexes_embedded_ruby_with_both_backends() {
        let source = "<main>\n  <% class Greeting %>\n    <%= MESSAGE %>\n  <% end %>\n</main>\n";

        for backend in [IndexerBackend::RubyIndexer, IndexerBackend::OperationBuilder] {
            let graph = build_local_graph("file:///view.txt.erb".into(), source, &LanguageId::ERB, backend);

            assert_eq!(graph.definitions().len(), 1, "backend: {backend:?}");
            assert_eq!(
                graph.definitions().values().next().unwrap().offset().start(),
                u32::try_from(source.find("class Greeting").unwrap()).unwrap(),
                "backend: {backend:?}"
            );
            assert_eq!(
                graph.document().content_hash(),
                xxhash_rust::xxh3::xxh3_64(source.as_bytes())
            );
            assert!(graph.document().diagnostics().is_empty(), "backend: {backend:?}");
        }
    }

    #[test]
    fn erb_document_hash_includes_non_ruby_content() {
        let first = "<p>first</p><% class Greeting; end %>";
        let second = "<p>other</p><% class Greeting; end %>";

        let first_graph = build_local_graph(
            "file:///view.erb".into(),
            first,
            &LanguageId::ERB,
            IndexerBackend::RubyIndexer,
        );
        let second_graph = build_local_graph(
            "file:///view.erb".into(),
            second,
            &LanguageId::ERB,
            IndexerBackend::RubyIndexer,
        );

        assert_ne!(
            first_graph.document().content_hash(),
            second_graph.document().content_hash()
        );
    }

    #[test]
    fn erb_extraction_failure_becomes_a_diagnostic() {
        let source = "before\0<%= Foo %>";
        let graph = build_local_graph(
            "file:///view.erb".into(),
            source,
            &LanguageId::ERB,
            IndexerBackend::RubyIndexer,
        );

        assert_eq!(graph.document().diagnostics().len(), 1);
        assert!(
            graph.document().diagnostics()[0]
                .message()
                .starts_with("Failed to extract embedded Ruby:")
        );
        assert_eq!(
            graph.document().content_hash(),
            xxhash_rust::xxh3::xxh3_64(source.as_bytes())
        );
    }

    #[test]
    fn erb_allows_top_level_yield_with_both_backends() {
        let source = "<head><%= yield :head %></head><body class=\"<%= yield(:body_class) %>\"><%= yield %></body>";

        for backend in [IndexerBackend::RubyIndexer, IndexerBackend::OperationBuilder] {
            let graph = build_local_graph("file:///layout.html.erb".into(), source, &LanguageId::ERB, backend);

            assert!(
                graph
                    .diagnostics()
                    .iter()
                    .all(|diagnostic| diagnostic.message() != "Invalid yield"),
                "backend: {backend:?}"
            );
        }
    }

    #[test]
    fn erb_allows_top_level_block_exits_with_both_backends() {
        for source in ["<% break %>", "<% next %>", "<% redo %>", "<% next if foo %>"] {
            for backend in [IndexerBackend::RubyIndexer, IndexerBackend::OperationBuilder] {
                let graph = build_local_graph("file:///partial.html.erb".into(), source, &LanguageId::ERB, backend);

                assert!(
                    graph.diagnostics().is_empty(),
                    "source: {source:?}, backend: {backend:?}, diagnostics: {:?}",
                    graph.diagnostics()
                );
            }
        }
    }

    #[test]
    fn ruby_retains_top_level_block_exit_parse_errors_with_both_backends() {
        for (source, message) in [
            ("break", "Invalid break"),
            ("next", "Invalid next"),
            ("redo", "Invalid redo"),
        ] {
            for backend in [IndexerBackend::RubyIndexer, IndexerBackend::OperationBuilder] {
                let graph = build_local_graph("file:///script.rb".into(), source, &LanguageId::Ruby, backend);

                assert!(
                    graph
                        .diagnostics()
                        .iter()
                        .any(|diagnostic| diagnostic.rule() == &Rule::ParseError && diagnostic.message() == message),
                    "source: {source:?}, backend: {backend:?}"
                );
            }
        }
    }

    #[test]
    fn ruby_retains_top_level_yield_parse_error_with_both_backends() {
        let source = "yield";

        for backend in [IndexerBackend::RubyIndexer, IndexerBackend::OperationBuilder] {
            let graph = build_local_graph("file:///script.rb".into(), source, &LanguageId::Ruby, backend);

            assert!(
                graph
                    .diagnostics()
                    .iter()
                    .any(|diagnostic| diagnostic.rule() == &Rule::ParseError && diagnostic.message() == "Invalid yield"),
                "backend: {backend:?}"
            );
        }
    }

    #[test]
    fn erb_retains_genuine_ruby_parse_errors_with_both_backends() {
        let source = "<%= foo( %>";

        for backend in [IndexerBackend::RubyIndexer, IndexerBackend::OperationBuilder] {
            let graph = build_local_graph("file:///broken.erb".into(), source, &LanguageId::ERB, backend);

            assert!(
                graph
                    .diagnostics()
                    .iter()
                    .any(|diagnostic| diagnostic.rule() == &Rule::ParseError),
                "backend: {backend:?}"
            );
        }
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

        index_source(&mut graph, uri.into(), "", &LanguageId::Ruby);

        assert_eq!(5, graph.definitions().len());
        assert_eq!(2, graph.documents().len());
    }
}
