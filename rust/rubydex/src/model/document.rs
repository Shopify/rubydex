use std::path::PathBuf;

use line_index::LineIndex;
use url::Url;

use crate::diagnostic::Diagnostic;
use crate::model::identity_maps::IdentityHashMap;
use crate::model::ids::{DefinitionId, ReferenceId};

// Represents a document currently loaded into memory. Identified by its unique URI, it holds the edges to all
// definitions and references discovered in it
#[derive(Debug)]
pub struct Document {
    uri: String,
    line_index: LineIndex,
    definition_ids: Vec<DefinitionId>,
    method_reference_ids: Vec<ReferenceId>,
    constant_reference_ids: Vec<ReferenceId>,
    diagnostics: Vec<Diagnostic>,
    definition_diagnostics: IdentityHashMap<DefinitionId, Vec<Diagnostic>>,
    reference_diagnostics: IdentityHashMap<ReferenceId, Vec<Diagnostic>>,
}

impl Document {
    #[must_use]
    pub fn new(uri: String, source: &str) -> Self {
        Self {
            uri,
            line_index: LineIndex::new(source),
            definition_ids: Vec::new(),
            method_reference_ids: Vec::new(),
            constant_reference_ids: Vec::new(),
            diagnostics: Vec::new(),
            definition_diagnostics: IdentityHashMap::default(),
            reference_diagnostics: IdentityHashMap::default(),
        }
    }

    #[must_use]
    pub fn uri(&self) -> &str {
        &self.uri
    }

    #[must_use]
    pub fn line_index(&self) -> &LineIndex {
        &self.line_index
    }

    #[must_use]
    pub fn definitions(&self) -> &[DefinitionId] {
        &self.definition_ids
    }

    pub fn add_definition(&mut self, definition_id: DefinitionId) {
        debug_assert!(
            !self.definition_ids.contains(&definition_id),
            "Cannot add the same exact definition to a document twice. Duplicate definition IDs"
        );

        self.definition_ids.push(definition_id);
    }

    #[must_use]
    pub fn method_references(&self) -> &[ReferenceId] {
        &self.method_reference_ids
    }

    pub fn add_method_reference(&mut self, reference_id: ReferenceId) {
        self.method_reference_ids.push(reference_id);
    }

    #[must_use]
    pub fn constant_references(&self) -> &[ReferenceId] {
        &self.constant_reference_ids
    }

    pub fn add_constant_reference(&mut self, reference_id: ReferenceId) {
        self.constant_reference_ids.push(reference_id);
    }

    #[must_use]
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    pub fn add_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    #[must_use]
    pub fn diagnostics_for_definition(&self, id: DefinitionId) -> &[Diagnostic] {
        self.definition_diagnostics.get(&id).map_or(&[], Vec::as_slice)
    }

    pub fn add_diagnostic_for_definition(&mut self, id: DefinitionId, diagnostic: Diagnostic) {
        self.definition_diagnostics.entry(id).or_default().push(diagnostic);
    }

    #[must_use]
    pub fn diagnostics_for_reference(&self, id: ReferenceId) -> &[Diagnostic] {
        self.reference_diagnostics.get(&id).map_or(&[], Vec::as_slice)
    }

    pub fn add_diagnostic_for_reference(&mut self, id: ReferenceId, diagnostic: Diagnostic) {
        self.reference_diagnostics.entry(id).or_default().push(diagnostic);
    }

    pub fn all_diagnostics(&self) -> impl Iterator<Item = &Diagnostic> {
        self.diagnostics
            .iter()
            .chain(self.definition_diagnostics.values().flatten())
            .chain(self.reference_diagnostics.values().flatten())
    }

    /// Computes the require path for this document given load paths.
    ///
    /// Returns `None` if:
    /// - URI is not `file://` scheme
    /// - URI doesn't end with `.rb`
    /// - File path doesn't match any load path
    /// - `Url::to_file_path()` fails
    #[must_use]
    pub fn require_path(&self, load_paths: &[PathBuf]) -> Option<String> {
        let url = Url::parse(&self.uri).ok()?;
        if url.scheme() != "file" {
            return None;
        }
        let file_path = url.to_file_path().ok()?;

        if file_path.extension().is_none_or(|ext| ext != "rb") {
            return None;
        }

        for load_path in load_paths {
            if let Ok(relative) = file_path.strip_prefix(load_path) {
                let file_path = relative
                    .components()
                    .filter_map(|c| c.as_os_str().to_str())
                    .collect::<Vec<_>>()
                    .join("/");

                let require_path = file_path.trim_end_matches(".rb").to_string();
                return Some(require_path);
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;

    fn test_root() -> PathBuf {
        let root = if cfg!(windows) {
            PathBuf::from_str("C:\\")
        } else {
            PathBuf::from_str("/")
        }
        .unwrap();

        root.join("test")
    }

    #[test]
    #[should_panic(expected = "Cannot add the same exact definition to a document twice. Duplicate definition IDs")]
    fn inserting_duplicate_definitions() {
        let mut document = Document::new("file:///foo.rb".to_string(), "class Foo; end");
        let def_id = DefinitionId::new(123);

        document.add_definition(def_id);
        document.add_definition(def_id);

        assert_eq!(document.definitions().len(), 1);
    }

    #[test]
    fn tracking_references() {
        let mut document = Document::new("file:///foo.rb".to_string(), "class Foo; end");
        let method_ref = ReferenceId::new(1);
        let constant_ref = ReferenceId::new(2);

        document.add_method_reference(method_ref);
        document.add_constant_reference(constant_ref);

        assert_eq!(document.method_references(), &[method_ref]);
        assert_eq!(document.constant_references(), &[constant_ref]);
    }

    #[test]
    fn require_path() {
        let root = test_root();
        let path = root.join("lib").join("foo").join("bar.rb");
        let uri = Url::from_file_path(path).unwrap().to_string();
        let load_paths = [root.join("lib")];

        let document = Document::new(uri, "");
        assert_eq!(Some("foo/bar".to_string()), document.require_path(&load_paths));
    }

    #[test]
    fn require_path_first_load_path_wins() {
        let root = test_root();
        let path = root.join("a").join("b").join("foo.rb");
        let uri = Url::from_file_path(path).unwrap().to_string();
        let load_path_a = root.join("a");
        let load_path_ab = root.join("a").join("b");
        let load_paths = [load_path_a, load_path_ab];

        let document = Document::new(uri, "");
        assert_eq!(Some("b/foo".to_string()), document.require_path(&load_paths));
    }

    #[test]
    fn require_path_returns_none() {
        let root = test_root();
        let load_paths = [root.join("lib")];

        // non-file URI
        let document = Document::new("untitled:Untitled-1".to_string(), "");
        assert_eq!(None, document.require_path(&load_paths));

        // non-.rb extension
        let path = root.join("lib").join("foo.so");
        let uri = Url::from_file_path(path).unwrap().to_string();
        let document = Document::new(uri, "");
        assert_eq!(None, document.require_path(&load_paths));

        // /libfoo is not under /lib - should not match due to partial directory name
        let path = root.join("libfoo").join("bar.rb");
        let uri = Url::from_file_path(path).unwrap().to_string();
        let document = Document::new(uri, "");
        assert_eq!(None, document.require_path(&load_paths));
    }

    #[test]
    fn require_path_with_spaces() {
        let root = test_root();
        let path = root.join("lib").join("space foo").join("bar.rb");
        let uri = Url::from_file_path(path).unwrap().to_string();
        let load_paths = [root.join("lib")];

        let document = Document::new(uri, "");
        assert_eq!(Some("space foo/bar".to_string()), document.require_path(&load_paths));
    }
}
