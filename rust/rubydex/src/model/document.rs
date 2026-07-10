use std::path::PathBuf;

use line_index::LineIndex;
use url::Url;
use xxhash_rust::xxh3::xxh3_64;

use crate::assert_mem_size;
use crate::diagnostic::Diagnostic;
use crate::model::ids::{ConstantReferenceId, DefinitionId, MethodReferenceId};

// Represents a document currently loaded into memory. Identified by its unique URI, it holds the edges to all
// definitions and references discovered in it
#[derive(Debug)]
pub struct Document {
    uri: String,
    line_index: LineIndex,
    definition_ids: Vec<DefinitionId>,
    method_reference_ids: Vec<MethodReferenceId>,
    constant_reference_ids: Vec<ConstantReferenceId>,
    diagnostics: Vec<Diagnostic>,
    content_hash: u64,
}
assert_mem_size!(Document, 184);

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
            content_hash: xxh3_64(source.as_bytes()),
        }
    }

    #[must_use]
    pub fn uri(&self) -> &str {
        &self.uri
    }

    #[must_use]
    pub fn content_hash(&self) -> u64 {
        self.content_hash
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
    pub fn method_references(&self) -> &[MethodReferenceId] {
        &self.method_reference_ids
    }

    pub fn add_method_reference(&mut self, reference_id: MethodReferenceId) {
        self.method_reference_ids.push(reference_id);
    }

    #[must_use]
    pub fn constant_references(&self) -> &[ConstantReferenceId] {
        &self.constant_reference_ids
    }

    pub fn add_constant_reference(&mut self, reference_id: ConstantReferenceId) {
        self.constant_reference_ids.push(reference_id);
    }

    #[must_use]
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    pub fn add_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    /// The file-system path of this document, decoded from its URI.
    ///
    /// Returns `None` when the URI is not a `file://` URL (e.g. the synthetic built-in document) or
    /// cannot be converted to a path. Uses `Url` so percent-encoding and platform-specific paths
    /// (including Windows drive paths) are handled correctly.
    #[must_use]
    pub fn file_path(&self) -> Option<PathBuf> {
        let url = Url::parse(&self.uri).ok()?;
        if url.scheme() != "file" {
            return None;
        }
        url.to_file_path().ok()
    }

    /// The base file name of this document (the last path segment), decoded from its URI.
    ///
    /// Prefers the platform file path, but falls back to the last URL path segment so it still works
    /// for `file://` URIs that don't convert to a local path on the current platform (e.g. a
    /// drive-less path like `file:///foo.rb` on Windows). Returns `None` only when the URI has no
    /// usable path segment (e.g. the synthetic built-in document).
    #[must_use]
    pub fn file_name(&self) -> Option<String> {
        if let Some(path) = self.file_path()
            && let Some(name) = path.file_name()
        {
            return Some(name.to_string_lossy().into_owned());
        }

        let url = Url::parse(&self.uri).ok()?;
        let segment = url.path_segments()?.rfind(|segment| !segment.is_empty())?;
        Some(segment.to_string())
    }

    /// Computes the require path for this document given load paths.
    ///
    /// Returns `None` if:
    /// - URI is not `file://` scheme
    /// - URI doesn't end with `.rb`
    /// - File path doesn't match any load path
    /// - `Url::to_file_path()` fails
    ///
    /// # Panics
    ///
    /// Panics if load path entries exceed u16.
    #[must_use]
    pub fn require_path(&self, load_paths: &[PathBuf]) -> Option<(String, u16)> {
        let file_path = self.file_path()?;
        if file_path.extension().is_none_or(|ext| ext != "rb") {
            return None;
        }

        for (load_path_index, load_path) in load_paths.iter().enumerate() {
            if let Ok(relative) = file_path.strip_prefix(load_path) {
                let file_path = relative
                    .components()
                    .filter_map(|c| c.as_os_str().to_str())
                    .collect::<Vec<_>>()
                    .join("/");

                let require_path = file_path.trim_end_matches(".rb").to_string();
                return Some((
                    require_path,
                    load_path_index.try_into().expect("Load path entries exceed u16"),
                ));
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
        let root = if cfg!(windows) { "C:\\" } else { "/" };
        PathBuf::from_str(root).unwrap()
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
        let method_ref = MethodReferenceId::new(1);
        let constant_ref = ConstantReferenceId::new(2);

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
        assert_eq!(Some(("foo/bar".to_string(), 0)), document.require_path(&load_paths));
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
        assert_eq!(Some(("b/foo".to_string(), 0)), document.require_path(&load_paths));
    }

    #[test]
    fn require_path_returns_none() {
        let root = test_root();
        let load_paths = [root.join("lib")];

        // non-file URI
        let document = Document::new("untitled:Untitled-1".to_string(), "");
        assert!(document.require_path(&load_paths).is_none());

        // non-.rb extension
        let path = root.join("lib").join("foo.so");
        let uri = Url::from_file_path(path).unwrap().to_string();
        let document = Document::new(uri, "");
        assert!(document.require_path(&load_paths).is_none());

        // /libfoo is not under /lib - should not match due to partial directory name
        let path = root.join("libfoo").join("bar.rb");
        let uri = Url::from_file_path(path).unwrap().to_string();
        let document = Document::new(uri, "");
        assert!(document.require_path(&load_paths).is_none());
    }

    #[test]
    fn require_path_with_spaces() {
        let root = test_root();
        let path = root.join("lib").join("space foo").join("bar.rb");
        let uri = Url::from_file_path(path).unwrap().to_string();
        let load_paths = [root.join("lib")];

        let document = Document::new(uri, "");
        assert_eq!(
            Some(("space foo/bar".to_string(), 0)),
            document.require_path(&load_paths)
        );
    }
}
