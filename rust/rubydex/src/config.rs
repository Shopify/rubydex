use std::collections::HashSet;
use std::path::PathBuf;

/// Project configuration that controls indexing behavior.
#[derive(Debug, Default)]
pub struct Config {
    /// Paths to exclude from file discovery during indexing.
    excluded_paths: HashSet<PathBuf>,
}

impl Config {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
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
