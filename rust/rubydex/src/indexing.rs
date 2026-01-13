use crate::{errors::Errors, indexing::ruby_indexer::RubyIndexer, model::graph::Graph};
use rayon::prelude::*;
use std::{fs, path::PathBuf};
use url::Url;

pub mod local_graph;
pub mod ruby_indexer;

/// Indexes the given paths in parallel, reading the content from disk and populating the given `Graph` instance.
pub fn index_files(graph: &mut Graph, paths: &[PathBuf]) -> Vec<Errors> {
    let results: Vec<_> = paths.par_iter().map(index_file).collect();

    let mut errors = Vec::new();

    for result in results {
        match result {
            Ok(local_graph) => graph.update(local_graph),
            Err(error) => errors.push(error),
        }
    }

    errors
}

/// Indexes a single Ruby file, returning a `LocalGraph` or an error.
fn index_file(path: &PathBuf) -> Result<local_graph::LocalGraph, Errors> {
    let source =
        fs::read_to_string(path).map_err(|_| Errors::FileError(format!("Failed to read file `{}`", path.display())))?;

    let url = Url::from_file_path(path)
        .map_err(|()| Errors::FileError(format!("Couldn't build URI from path `{}`", path.display())))?;

    let mut ruby_indexer = RubyIndexer::new(url.to_string(), &source);
    ruby_indexer.index();

    Ok(ruby_indexer.local_graph())
}

#[cfg(test)]
mod tests {
    use std::{path::PathBuf, slice::from_ref};

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
        let errors = index_files(&mut graph, from_ref(relative_to_pwd));

        assert!(errors.is_empty());
        assert_eq!(graph.documents().len(), 1);
    }
}
