use crate::assert_mem_size;
use crate::errors::Errors;
use crate::path_helpers;
use std::collections::HashSet;
use std::fs;
use std::io::ErrorKind;
use std::path::{MAIN_SEPARATOR, Path, PathBuf};
use toml::{Table, Value};

const DEFAULT_EXCLUDED_DIRECTORIES: &[&str] = &[
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

/// The graph's settings, read from the `[graph]` section of the configuration file
#[derive(Debug, Clone)]
pub struct GraphSettings {
    /// Patterns to exclude from file discovery during indexing, on top of the built-in defaults. Stored as written and
    /// only joined with the workspace path when read, so that a pattern cannot outlive the configuration it came from
    /// and be silently re-rooted under another workspace.
    excluded_patterns: HashSet<Box<str>>,
}

impl Default for GraphSettings {
    /// The settings of a workspace that configures nothing: the built-in exclusions and no more
    fn default() -> Self {
        Self {
            excluded_patterns: DEFAULT_EXCLUDED_DIRECTORIES.iter().map(|&dir| Box::from(dir)).collect(),
        }
    }
}

impl GraphSettings {
    /// Parses the `[graph]` section on top of the default exclusions
    fn parse(mut table: Table) -> Result<Self, String> {
        let exclude = match table.remove("exclude") {
            None => Vec::new(),
            Some(value) => value
                .try_into::<Vec<Box<str>>>()
                .map_err(|error| format!("invalid `graph.exclude` setting: {error}"))?,
        };

        if let Some(key) = table.keys().next() {
            return Err(format!("unknown setting `graph.{key}`"));
        }

        let mut settings = Self::default();
        settings.exclude_patterns(exclude);
        Ok(settings)
    }

    /// Adds patterns to exclude from file discovery during indexing
    fn exclude_patterns(&mut self, patterns: impl IntoIterator<Item = Box<str>>) {
        self.excluded_patterns.extend(patterns);
    }

    /// Returns the exclusion patterns as written, without resolving them against any workspace
    fn excluded_patterns(&self) -> impl Iterator<Item = &str> {
        self.excluded_patterns.iter().map(|pattern| &**pattern)
    }
}

/// The setting of a single linter rule, read from a `[linter.rules.RuleName]` table
#[derive(Debug, Clone)]
pub struct Rule {
    name: Box<str>,
    enabled: bool,
}

impl Rule {
    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[must_use]
    pub fn enabled(&self) -> bool {
        self.enabled
    }

    /// Parses a single `[linter.rules.{name}]` table
    fn parse(name: &str, value: Value) -> Result<Self, String> {
        let Value::Table(mut table) = value else {
            return Err(format!("invalid `linter.rules.{name}` setting: expected a table"));
        };

        let enabled = match table.remove("enabled") {
            Some(Value::Boolean(enabled)) => enabled,
            Some(_) => {
                return Err(format!(
                    "invalid `linter.rules.{name}.enabled` setting: expected a boolean"
                ));
            }
            None => return Err(format!("missing `linter.rules.{name}.enabled` setting")),
        };

        if let Some(key) = table.keys().next() {
            return Err(format!("unknown setting `linter.rules.{name}.{key}`"));
        }

        Ok(Self {
            name: Box::from(name),
            enabled,
        })
    }
}

/// The linter's settings, read from the `[linter]` section of the configuration file
#[derive(Debug, Clone, Default)]
pub struct LinterSettings {
    rules: Box<[Rule]>,
}

impl LinterSettings {
    #[must_use]
    pub fn rules(&self) -> &[Rule] {
        &self.rules
    }

    /// Parses the `[linter]` section
    fn parse(mut table: Table) -> Result<Self, String> {
        let rules = match table.remove("rules") {
            None => Box::default(),
            Some(Value::Table(rules)) => rules
                .into_iter()
                .map(|(name, value)| Rule::parse(&name, value))
                .collect::<Result<_, _>>()?,
            Some(_) => {
                return Err(String::from(
                    "invalid `linter.rules` setting: expected a table of rules",
                ));
            }
        };

        if let Some(key) = table.keys().next() {
            return Err(format!("unknown setting `linter.{key}`"));
        }

        Ok(Self { rules })
    }
}

/// The configuration of a workspace, parsed from its `rubydex.toml` and shared by all built-in tools. It carries both
/// the settings that are global to every tool, such as the workspace being analyzed, and the typed settings of each
/// tool's own section (e.g. `[graph]`). Every section is parsed eagerly, so that all validation happens at load time and
/// unknown sections or settings are rejected. Load the file once and hand the configuration to each consumer, so that
/// none of them has to read it again.
#[derive(Debug, Clone)]
pub struct Config {
    /// Root directory of the workspace being analyzed, which is where its configuration file lives. Global, and not
    /// configurable through the file itself, since it is what says where that file is.
    workspace_path: Box<Path>,
    graph: GraphSettings,
    linter: LinterSettings,
}
assert_mem_size!(Config, 80);

impl Default for Config {
    /// The configuration of the current working directory, with the default settings of every section, which is what a
    /// graph is configured by until one is loaded for it. Guessing a root here rather than leaving it empty is what
    /// keeps `Graph::new` infallible, since patterns are resolved against it; every other configuration comes from
    /// [`Config::load`]. Cannot be derived, as `Box<Path>` has no default.
    fn default() -> Self {
        Self {
            workspace_path: std::env::current_dir()
                .unwrap_or_else(|_| PathBuf::from("."))
                .into_boxed_path(),
            graph: GraphSettings::default(),
            linter: LinterSettings::default(),
        }
    }
}

impl Config {
    /// Loads the configuration of the workspace rooted at `workspace_path`, which is where its `rubydex.toml` is
    /// expected to be. The configuration file itself is optional: a workspace without one gets the default settings.
    ///
    /// # Errors
    ///
    /// Returns [`Errors::ConfigError`] if `workspace_path` is not a directory, or if the configuration file exists but
    /// cannot be read or is invalid.
    pub fn load(workspace_path: &Path) -> Result<Self, Errors> {
        let workspace_path = path_helpers::resolved(workspace_path).map_err(|error| {
            Errors::ConfigError(format!(
                "Failed to resolve workspace path `{}`: {error}",
                workspace_path.display()
            ))
        })?;

        // Since loading a configuration is how a workspace is chosen, a root that cannot be analyzed has to be rejected
        // here. Otherwise the mistake is only reported much later, by whatever first tries to walk it.
        if !workspace_path.is_dir() {
            return Err(Errors::ConfigError(format!(
                "Workspace `{}` is not a directory",
                workspace_path.display()
            )));
        }

        let config_path = workspace_path.join("rubydex.toml");

        let content = match fs::read_to_string(&config_path) {
            Ok(content) => content,
            // Configuring a workspace is optional, so a missing file is the same as an empty one
            Err(error) if error.kind() == ErrorKind::NotFound => String::new(),
            Err(error) => {
                return Err(Errors::ConfigError(format!(
                    "Failed to read config file `{}`: {error}",
                    config_path.display()
                )));
            }
        };

        Self::parse(workspace_path, &content)
            .map_err(|error| Errors::ConfigError(format!("Invalid config file `{}`: {error}", config_path.display())))
    }

    /// Returns the root directory of the workspace this configuration belongs to
    #[must_use]
    pub fn workspace_path(&self) -> &Path {
        &self.workspace_path
    }

    /// Adds patterns to exclude from file discovery during indexing. Excluded directories will be skipped entirely
    /// during directory traversal.
    pub fn exclude_patterns(&mut self, patterns: impl IntoIterator<Item = Box<str>>) {
        self.graph.exclude_patterns(patterns);
    }

    /// Returns the set of exclusion patterns resolved against the workspace path. Resolving is the one thing that needs
    /// both halves of the configuration, which is why it lives here rather than on the settings of the section the
    /// patterns come from.
    #[must_use]
    pub fn excluded_patterns(&self) -> HashSet<Box<str>> {
        self.graph
            .excluded_patterns()
            .map(|pattern| {
                // We must replace the separator on Windows for forward slash to use in glob patterns.
                self.workspace_path
                    .join(pattern)
                    .to_string_lossy()
                    .replace(MAIN_SEPARATOR, "/")
                    .into_boxed_str()
            })
            .collect()
    }

    #[must_use]
    pub fn linter(&self) -> &LinterSettings {
        &self.linter
    }

    /// Parses the content of the configuration file of the workspace rooted at `workspace_path` into the typed
    /// settings of each section
    fn parse(workspace_path: PathBuf, content: &str) -> Result<Self, String> {
        let mut sections: Table = toml::from_str(content).map_err(|error| error.to_string())?;

        // Every top-level entry must be a section table. A non-table entry is most likely a typo, and an array of
        // tables comes from `[[section]]` syntax, which is a section shape mistake rather than an unknown setting.
        if let Some((key, value)) = sections.iter().find(|(_, value)| !value.is_table()) {
            if value
                .as_array()
                .is_some_and(|array| !array.is_empty() && array.iter().all(Value::is_table))
            {
                return Err(format!(
                    "section `{key}` must be a table; use `[{key}]` instead of `[[{key}]]`"
                ));
            }

            return Err(format!("unknown setting `{key}`"));
        }

        // Non-table entries were rejected above, so every present section is always a table.
        let graph = match sections.remove("graph") {
            Some(Value::Table(table)) => GraphSettings::parse(table)?,
            _ => GraphSettings::default(),
        };

        let linter = match sections.remove("linter") {
            Some(Value::Table(table)) => LinterSettings::parse(table)?,
            _ => LinterSettings::default(),
        };

        // Every section must be backed by a typed settings struct, so any leftover section is unknown.
        if let Some(key) = sections.keys().next() {
            return Err(format!("unknown section `{key}`"));
        }

        Ok(Self {
            workspace_path: Box::from(workspace_path),
            graph,
            linter,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The pattern the exclusion `entry` resolves to under `workspace_path`, spelled the way exclusions are
    fn exclusion(workspace_path: impl AsRef<Path>, entry: &str) -> String {
        workspace_path
            .as_ref()
            .join(entry)
            .to_string_lossy()
            .replace(MAIN_SEPARATOR, "/")
    }

    fn workspace_exclusion(entry: &str) -> String {
        exclusion("/workspace", entry)
    }

    /// Parses configuration content for an arbitrary workspace, for the tests that only care about the settings
    fn parse(content: &str) -> Result<Config, String> {
        Config::parse(PathBuf::from("/workspace"), content)
    }

    #[test]
    fn excluded_patterns_resolves_patterns_against_the_workspace_path() {
        let mut config = parse("").expect("an empty config is valid");
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
    fn excluded_patterns_are_separated_by_forward_slashes() {
        // Exclusions are glob patterns, and a glob pattern is written with forward slashes whatever the platform's own
        // separator is. Joining them against the workspace path is what would otherwise introduce a backslash, so the
        // invariant is asserted on the joined result. Vacuous where the separator already is a forward slash.
        let mut config = parse("").expect("an empty config is valid");
        config.exclude_patterns([Box::from("vendor/bundle")]);

        let excluded = config.excluded_patterns();

        assert!(!excluded.is_empty(), "expected the defaults at least");
        assert!(
            excluded.iter().all(|pattern| !pattern.contains('\\')),
            "unexpected backslash in {excluded:?}"
        );
    }

    #[test]
    fn load_parses_the_settings_of_every_section() {
        let dir = tempfile::tempdir().expect("failed to create temp dir");
        fs::write(
            dir.path().join("rubydex.toml"),
            "[graph]\nexclude = [\"vendor\"]\n\n[linter.rules.Something]\nenabled = true\n",
        )
        .unwrap();

        let config = Config::load(dir.path()).expect("expected the config file to load");
        let excluded = config.excluded_patterns();

        let path = path_helpers::resolved(dir.path()).unwrap();
        assert!(excluded.contains(exclusion(&path, "vendor").as_str()));

        let rules = config.linter().rules();
        assert_eq!(rules.len(), 1);
        assert_eq!(rules[0].name(), "Something");
        assert!(rules[0].enabled());
    }

    #[test]
    fn parse_parses_every_linter_rule() {
        let config = parse("[linter.rules.Something]\nenabled = true\n\n[linter.rules.Other]\nenabled = false\n")
            .expect("expected the config to parse");

        let rules = config.linter().rules();
        assert_eq!(rules.len(), 2);

        let something = rules.iter().find(|rule| rule.name() == "Something").unwrap();
        assert!(something.enabled());

        let other = rules.iter().find(|rule| rule.name() == "Other").unwrap();
        assert!(!other.enabled());
    }

    #[test]
    fn parse_accepts_an_empty_linter_section() {
        let config = parse("[linter]\n").expect("an empty linter section is valid");
        assert!(config.linter().rules().is_empty());

        let config = parse("[linter.rules]\n").expect("an empty rules table is valid");
        assert!(config.linter().rules().is_empty());
    }

    #[test]
    fn parse_rejects_an_unknown_section() {
        let error = parse("[lintr]\n").expect_err("every section must be backed by typed settings structs");
        assert!(error.contains("unknown section `lintr`"), "unexpected error: {error}");
    }

    #[test]
    fn parse_rejects_an_unknown_linter_setting() {
        let error = parse("[linter]\nparallel = true\n").expect_err("typos inside the linter section must be rejected");

        assert!(
            error.contains("unknown setting `linter.parallel`"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn parse_rejects_a_rules_setting_that_is_not_a_table() {
        let error = parse("[linter]\nrules = true\n").expect_err("rules must be a table of rule tables");

        assert!(
            error.contains("invalid `linter.rules` setting"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn parse_rejects_a_rule_that_is_not_a_table() {
        let error = parse("[linter.rules]\nSomething = true\n").expect_err("every rule setting must be a table");

        assert!(
            error.contains("invalid `linter.rules.Something` setting"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn parse_rejects_a_rule_without_enabled() {
        let error =
            parse("[linter.rules.Something]\n").expect_err("a rule setting must state whether the rule is enabled");

        assert!(
            error.contains("missing `linter.rules.Something.enabled` setting"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn parse_rejects_a_non_boolean_enabled_setting() {
        let error = parse("[linter.rules.Something]\nenabled = \"yes\"\n").expect_err("enabled only accepts a boolean");

        assert!(
            error.contains("invalid `linter.rules.Something.enabled` setting"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn parse_rejects_an_unknown_rule_setting() {
        let error = parse("[linter.rules.Something]\nenabled = true\nseverity = \"error\"\n")
            .expect_err("rules only accept the enabled setting");

        assert!(
            error.contains("unknown setting `linter.rules.Something.severity`"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn load_returns_the_default_configuration_for_a_workspace_without_a_config_file() {
        let dir = tempfile::tempdir().expect("failed to create temp dir");
        let config = Config::load(dir.path()).expect("a missing rubydex.toml must not be an error");

        assert_eq!(config.workspace_path(), path_helpers::resolved(dir.path()).unwrap());
        assert_eq!(config.excluded_patterns().len(), DEFAULT_EXCLUDED_DIRECTORIES.len());
        assert!(config.linter().rules().is_empty());
    }

    #[test]
    fn load_reads_the_config_file_under_the_workspace_path() {
        let dir = tempfile::tempdir().expect("failed to create temp dir");
        fs::write(dir.path().join("rubydex.toml"), "[graph]\nexclude = [\"vendor\"]\n").unwrap();

        let config = Config::load(dir.path()).expect("expected rubydex.toml to load");
        let excluded = config.excluded_patterns();

        let path = path_helpers::resolved(dir.path()).unwrap();
        assert_eq!(config.workspace_path(), path);
        assert!(excluded.contains(exclusion(&path, "vendor").as_str()));
        assert!(excluded.contains(exclusion(&path, ".git").as_str()));
    }

    #[test]
    fn load_errors_when_the_workspace_is_not_a_directory() {
        let dir = tempfile::tempdir().expect("failed to create temp dir");
        let missing = dir.path().join("typo");
        let file = dir.path().join("file.rb");
        fs::write(&file, "class Foo; end").unwrap();

        for workspace_path in [missing.as_path(), file.as_path()] {
            let error = Config::load(workspace_path).expect_err("a workspace must be a directory");
            let named = path_helpers::resolved(workspace_path).unwrap_or_else(|_| workspace_path.to_path_buf());

            assert!(matches!(error, Errors::ConfigError(_)), "unexpected error: {error:?}");
            assert!(
                error.to_string().contains(&named.display().to_string()),
                "expected the error to name the workspace `{}`: {error}",
                named.display()
            );
        }
    }

    #[test]
    fn load_errors_when_the_config_file_cannot_be_read() {
        let dir = tempfile::tempdir().expect("failed to create temp dir");
        // A `rubydex.toml` that exists but cannot be read is a broken workspace, unlike one that has no configuration
        // at all. Making it a directory is the portable way of making it unreadable.
        fs::create_dir(dir.path().join("rubydex.toml")).unwrap();

        let error = Config::load(dir.path()).expect_err("an unreadable config file must be an error");

        assert!(matches!(error, Errors::ConfigError(_)), "unexpected error: {error:?}");
        assert!(
            error.to_string().contains("Failed to read config file"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn load_propagates_malformed_config_errors() {
        let dir = tempfile::tempdir().expect("failed to create temp dir");
        fs::write(dir.path().join("rubydex.toml"), "[graph]\nexclude = [\n").unwrap();

        let error = Config::load(dir.path()).expect_err("a malformed config file must be an error");

        assert!(matches!(error, Errors::ConfigError(_)), "unexpected error: {error:?}");
    }

    #[test]
    fn parse_accepts_an_empty_config() {
        let config = parse("").expect("an empty config is valid");

        // Nothing is configured, so the settings of every section are the default ones.
        assert_eq!(config.excluded_patterns().len(), DEFAULT_EXCLUDED_DIRECTORIES.len());
        assert!(config.linter().rules().is_empty());
    }

    #[test]
    fn parse_rejects_an_exclude_value_of_the_wrong_type() {
        let error =
            parse("[graph]\nexclude = \"vendor\"").expect_err("exclude must be an array of strings, not a string");

        assert!(error.contains("graph.exclude"), "unexpected error: {error}");
    }

    #[test]
    fn parse_rejects_an_unknown_top_level_setting() {
        let error = parse("excludes = [\"vendor\"]\n").expect_err("every top-level entry must be a tool section table");

        assert!(
            error.contains("unknown setting `excludes`"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn parse_rejects_an_unknown_graph_setting() {
        let error = parse("[graph]\nexcludes = [\"vendor\"]\n")
            .expect_err("the graph section is owned by Rubydex, so typos inside it must be rejected");

        assert!(
            error.contains("unknown setting `graph.excludes`"),
            "unexpected error: {error}"
        );
    }

    #[test]
    fn parse_rejects_a_graph_section_that_is_not_a_table() {
        let error = parse("graph = \"yes\"\n").expect_err("the graph section must be a table");

        assert!(error.contains("`graph`"), "unexpected error: {error}");
    }

    #[test]
    fn parse_rejects_an_array_of_tables_section() {
        let error =
            parse("[[linter]]\nparallel = true\n").expect_err("array-of-tables syntax is not a valid tool section");

        assert!(
            error.contains("use `[linter]` instead of `[[linter]]`"),
            "unexpected error: {error}"
        );
    }
}
