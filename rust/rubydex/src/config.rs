use crate::assert_mem_size;
use crate::errors::Errors;
use serde::Deserialize;
use std::collections::HashSet;
use std::fs;
use std::io::ErrorKind;
use std::path::{Path, PathBuf};

pub const DEFAULT_EXCLUDED_DIRECTORIES: &[&str] = &[
    ".bundle",
    ".claude",
    ".git",
    ".github",
    ".ruby-lsp",
    ".vscode",
    "log",
    "node_modules",
    "tmp",
];

/// Configuration coming from a config file
#[derive(Debug, Default, Deserialize)]
struct ConfigFile {
    /// Patterns to exclude from file discovery during indexing.
    #[serde(default)]
    exclude: Vec<Box<str>>,
}

/// Project configuration
#[derive(Debug)]
pub struct Config {
    /// Path to the workspace being analyzed
    workspace_path: Box<Path>,
    /// Patterns to exclude from file discovery during indexing.
    excluded_patterns: HashSet<Box<str>>,
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
            excluded_patterns: DEFAULT_EXCLUDED_DIRECTORIES.iter().map(|&dir| Box::from(dir)).collect(),
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

    /// Adds patterns to exclude from file discovery during indexing. Excluded directories will be skipped entirely during
    /// directory traversal.
    pub fn exclude_patterns(&mut self, patterns: impl IntoIterator<Item = Box<str>>) {
        self.excluded_patterns.extend(patterns);
    }

    /// Returns the set of exclusion patterns resolved against the workspace path.
    #[must_use]
    pub fn excluded_patterns(&self) -> HashSet<Box<str>> {
        self.excluded_patterns
            .iter()
            .map(|entry| {
                self.workspace_path
                    .join(&**entry)
                    .to_string_lossy()
                    .into_owned()
                    .into_boxed_str()
            })
            .collect()
    }

    /// Merges the default `rubydex.toml` configuration file from the workspace root into this config, if present.
    ///
    /// The default config file is optional, so a missing `rubydex.toml` is silently ignored. Any other failure (an
    /// unreadable or malformed file) is still reported.
    ///
    /// # Errors
    ///
    /// Will error if the config file exists but cannot be read or has invalid syntax.
    pub fn load_default(&mut self) -> Result<(), Errors> {
        let config_path = self.workspace_path.join("rubydex.toml");

        match self.load_file(&config_path) {
            Err(Errors::ConfigNotFound(_)) => Ok(()),
            other => other,
        }
    }

    /// Merges the configuration at `config_path` into this config
    ///
    /// # Errors
    ///
    /// Returns [`Errors::ConfigNotFound`] if the file does not exist or [`Errors::ConfigError`] if it cannot otherwise
    /// be read or has invalid syntax.
    pub fn load_file(&mut self, config_path: &Path) -> Result<(), Errors> {
        let content = match fs::read_to_string(config_path) {
            Ok(content) => content,
            Err(error) if error.kind() == ErrorKind::NotFound => {
                return Err(Errors::ConfigNotFound(format!(
                    "Config file `{}` does not exist",
                    config_path.display()
                )));
            }
            Err(error) => {
                return Err(Errors::ConfigError(format!(
                    "Failed to read config file `{}`: {error}",
                    config_path.display()
                )));
            }
        };

        let parsed = Self::parse(&content).map_err(|error| {
            Errors::ConfigError(format!("Invalid config file `{}`: {error}", config_path.display()))
        })?;

        self.excluded_patterns.extend(parsed.exclude);
        Ok(())
    }

    /// Parses the content into a [`ConfigFile`]
    fn parse(content: &str) -> Result<ConfigFile, toml::de::Error> {
        toml::from_str(content)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    fn workspace_exclusion(entry: &str) -> String {
        PathBuf::from("/workspace").join(entry).to_string_lossy().into_owned()
    }

    #[test]
    fn excluded_patterns_resolves_patterns_against_the_workspace_path() {
        let mut config = Config::new();
        config.set_workspace_path(PathBuf::from("/workspace"));
        config.exclude_patterns([
            Box::from("vendor"),
            Box::from("**/fixtures"),
            Box::from("/absolute/path"),
        ]);

        let excluded = config.excluded_patterns();

        let vendor = workspace_exclusion("vendor");
        let fixtures = workspace_exclusion("**/fixtures");
        let absolute = PathBuf::from("/absolute/path").to_string_lossy().into_owned();
        let git = workspace_exclusion(".git");

        assert!(excluded.contains(vendor.as_str()));
        assert!(excluded.contains(fixtures.as_str()));
        assert!(excluded.contains(absolute.as_str()));
        // Defaults are included and resolved as well.
        assert!(excluded.contains(git.as_str()));
    }

    #[test]
    fn new_seeds_the_default_excluded_directories() {
        let config = Config::new();

        for default in DEFAULT_EXCLUDED_DIRECTORIES {
            assert!(
                config.excluded_patterns.contains(*default),
                "expected `{default}` to be excluded by default"
            );
        }
    }

    #[test]
    fn load_file_merges_excluded_patterns_and_leaves_the_workspace_path_untouched() {
        let dir = tempfile::tempdir().expect("failed to create temp dir");
        let config_path = dir.path().join("rubydex.toml");
        fs::write(&config_path, "exclude = [\"vendor\", \"generated\"]\n").unwrap();

        let mut config = Config::new();
        config.set_workspace_path(PathBuf::from("/workspace"));

        config
            .load_file(&config_path)
            .expect("expected the config file to load");

        let excluded = config.excluded_patterns();
        // Entries from the file are merged in and resolved against the workspace path.
        assert!(excluded.contains(workspace_exclusion("vendor").as_str()));
        assert!(excluded.contains(workspace_exclusion("generated").as_str()));
        // Defaults seeded at construction survive the merge.
        assert!(excluded.contains(workspace_exclusion("node_modules").as_str()));
        // A config file cannot override the programmatically-set workspace path.
        assert_eq!(config.workspace_path(), Path::new("/workspace"));
    }

    #[test]
    fn load_file_accumulates_exclusions_across_multiple_loads() {
        let dir = tempfile::tempdir().expect("failed to create temp dir");
        fs::write(dir.path().join("a.toml"), "exclude = [\"vendor\"]\n").unwrap();
        fs::write(dir.path().join("b.toml"), "exclude = [\"generated\"]\n").unwrap();

        let mut config = Config::new();
        config.set_workspace_path(PathBuf::from("/workspace"));
        config
            .load_file(&dir.path().join("a.toml"))
            .expect("expected the first file to load");
        config
            .load_file(&dir.path().join("b.toml"))
            .expect("expected the second file to load");

        let excluded = config.excluded_patterns();
        assert!(excluded.contains(workspace_exclusion("vendor").as_str()));
        assert!(excluded.contains(workspace_exclusion("generated").as_str()));
    }

    #[test]
    fn load_file_errors_when_the_file_is_missing() {
        let dir = tempfile::tempdir().expect("failed to create temp dir");
        let mut config = Config::new();

        let error = config
            .load_file(&dir.path().join("does_not_exist.toml"))
            .expect_err("an explicitly requested missing file must be an error");

        assert!(
            matches!(error, Errors::ConfigNotFound(_)),
            "unexpected error: {error:?}"
        );
    }

    #[test]
    fn load_default_ignores_a_missing_config_file() {
        let dir = tempfile::tempdir().expect("failed to create temp dir");
        let mut config = Config::new();
        config.set_workspace_path(dir.path().to_path_buf());

        config
            .load_default()
            .expect("a missing rubydex.toml must not be an error");
    }

    #[test]
    fn load_default_loads_an_existing_config_file() {
        let dir = tempfile::tempdir().expect("failed to create temp dir");
        fs::write(dir.path().join("rubydex.toml"), "exclude = [\"vendor\"]\n").unwrap();

        let mut config = Config::new();
        config.set_workspace_path(dir.path().to_path_buf());
        config.load_default().expect("expected rubydex.toml to load");

        let expected = dir.path().join("vendor").to_string_lossy().into_owned();
        assert!(config.excluded_patterns().contains(expected.as_str()));
    }

    #[test]
    fn load_default_propagates_malformed_config_errors() {
        let dir = tempfile::tempdir().expect("failed to create temp dir");
        fs::write(dir.path().join("rubydex.toml"), "exclude = [\n").unwrap();

        let mut config = Config::new();
        config.set_workspace_path(dir.path().to_path_buf());

        let error = config
            .load_default()
            .expect_err("a malformed default config must still be an error");

        assert!(matches!(error, Errors::ConfigError(_)), "unexpected error: {error:?}");
    }

    #[test]
    fn parse_defaults_the_excluded_patterns_to_empty_when_the_key_is_absent() {
        let file = Config::parse("").expect("an empty config is valid");
        assert!(file.exclude.is_empty());
    }

    #[test]
    fn parse_rejects_an_exclude_value_of_the_wrong_type() {
        Config::parse("exclude = \"vendor\"").expect_err("exclude must be an array of strings, not a string");
    }
}
