use crate::{
    errors::Errors,
    indexing::{local_graph::LocalGraph, ruby_indexer::RubyIndexer},
    job_queue::{Job, JobQueue},
    model::graph::Graph,
};
use crossbeam_channel::{Sender, unbounded};
use std::{fs, path::PathBuf, sync::Arc};
use url::Url;

pub mod local_graph;
pub mod ruby_indexer;

/// Job that indexes a single Ruby file
pub struct IndexingRubyFileJob {
    path: PathBuf,
    local_graph_tx: Sender<LocalGraph>,
    errors_tx: Sender<Errors>,
    dsl_method_names: Vec<&'static str>,
}

impl IndexingRubyFileJob {
    #[must_use]
    pub fn new(
        path: PathBuf,
        local_graph_tx: Sender<LocalGraph>,
        errors_tx: Sender<Errors>,
        dsl_method_names: Vec<&'static str>,
    ) -> Self {
        Self {
            path,
            local_graph_tx,
            errors_tx,
            dsl_method_names,
        }
    }

    fn send_error(&self, error: Errors) {
        self.errors_tx
            .send(error)
            .expect("errors receiver dropped before run completion");
    }
}

impl Job for IndexingRubyFileJob {
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

        let mut ruby_indexer = RubyIndexer::new(url.to_string(), &source, self.dsl_method_names.clone());
        ruby_indexer.index();

        self.local_graph_tx
            .send(ruby_indexer.local_graph())
            .expect("graph receiver dropped before merge");
    }
}

/// Indexes the given paths, reading the content from disk and populating the given `Graph` instance.
///
/// # Panics
///
/// Will panic if the graph cannot be wrapped in an Arc<Mutex<>>
pub fn index_files(graph: &mut Graph, paths: Vec<PathBuf>) -> Vec<Errors> {
    let queue = Arc::new(JobQueue::new());
    let (local_graphs_tx, local_graphs_rx) = unbounded();
    let (errors_tx, errors_rx) = unbounded();
    let dsl_method_names = graph.dsl_method_names();

    for path in paths {
        queue.push(Box::new(IndexingRubyFileJob::new(
            path,
            local_graphs_tx.clone(),
            errors_tx.clone(),
            dsl_method_names.clone(),
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
}
