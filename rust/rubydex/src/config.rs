use crate::assert_mem_size;
use std::collections::HashSet;
use std::path::{Path, PathBuf};

/// Project configuration
#[derive(Debug)]
pub struct Config {
    /// Path to the workspace being analyzed
    workspace_path: Box<Path>,
    /// Paths to exclude from file discovery during indexing.
    excluded_paths: HashSet<PathBuf>,
}
assert_mem_size!(Config, 64);

impl Default for Config {
    fn default() -> Self {
        Self::new()
    }
}

impl Config {
    /// Creates a configuration whose workspace path defaults to the current working directory.
    #[must_use]
    pub fn new() -> Self {
        Self {
            workspace_path: std::env::current_dir()
                .unwrap_or_else(|_| PathBuf::from("."))
                .into_boxed_path(),
            excluded_paths: HashSet::new(),
        }
    }

    /// Returns the root directory of the workspace being analyzed.
    #[must_use]
    pub fn workspace_path(&self) -> &Path {
        &self.workspace_path
    }

    /// Sets the root directory of the workspace being analyzed.
    pub fn set_workspace_path(&mut self, workspace_path: PathBuf) {
        self.workspace_path = workspace_path.into_boxed_path();
    }

    /// Adds paths to exclude from file discovery during indexing. Excluded directories will be skipped entirely during
    /// directory traversal.
    pub fn exclude_paths(&mut self, paths: Vec<PathBuf>) {
        self.excluded_paths.extend(paths);
    }

    /// Returns the set of paths excluded from file discovery.
    #[must_use]
    pub fn excluded_paths(&self) -> &HashSet<PathBuf> {
        &self.excluded_paths
    }
}
