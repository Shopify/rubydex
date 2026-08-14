//! Writes a resolved [`Graph`] to disk and re-opens it without rebuilding it.
//!
//! Indexing and resolving a hyper scale workspace costs tens of seconds and gigabytes of resident
//! memory. A snapshot removes that cost for a process that only reads the graph.
//!
//! The format is an [rkyv](https://rkyv.org) archive: the bytes on disk are the in-memory layout of
//! the graph. [`Snapshot::open`] maps the file and casts a pointer, so it allocates nothing and
//! copies nothing. The operating system pages in only the parts a query touches, and it can reclaim
//! those pages under memory pressure. [`Snapshot::to_graph`] is the opposite trade: it walks the
//! archive once and hands back an owned, mutable [`Graph`].
//!
//! Every id in the graph is a deterministic xxh3 content hash, so the ids in an archive stay valid
//! in another process. Nothing needs relocating on load.
//!
//! # Staleness
//!
//! A snapshot also carries a [`Manifest`], a Merkle tree over the files it was built from.
//! [`Snapshot::changes`] compares that tree against the file system and reports which documents to
//! re-index. See [`manifest`] for why the tree is shaped the way it is.
//!
//! A [`Document`](crate::model::document::Document) archives without its line index, and rebuilds
//! it from disk on first use.
//!
//! # Layout
//!
//! The manifest and the graph are two independent archives in one file, rather than one archive
//! holding both. A staleness check then touches only the manifest region, which is megabytes, and
//! never faults in the graph, which is gigabytes.
//!
//! ```text
//! offset 0   magic         8 bytes   "RDXSNAP\0"
//! offset 8   version       4 bytes   little-endian u32
//! offset 12  reserved      4 bytes   zero
//! offset 16  manifest_len  8 bytes   little-endian u64, padded to a multiple of 16
//! offset 24  reserved      8 bytes   zero
//! offset 32  manifest archive
//! offset 32 + manifest_len   graph archive
//! ```
//!
//! The header is 32 bytes and the manifest is padded to 16, so both archives start on a 16 byte
//! boundary. A memory map starts on a page boundary, so that is enough for any alignment the
//! archived types can ask for.

use std::fmt;
use std::fs::File;
use std::io::{self, Write};
use std::path::{Path, PathBuf};

use crate::errors::Errors;
use crate::indexing::{self, IndexerBackend};
use crate::model::graph::{ArchivedGraph, Graph};
use crate::resolution::Resolver;
use crate::snapshot::manifest::{ArchivedManifest, Changes, Manifest, Verification};
use memmap2::Mmap;
use rkyv::rancor::Error as RkyvError;

pub mod manifest;

/// Identifies a rubydex snapshot file.
const MAGIC: &[u8; 8] = b"RDXSNAP\0";

/// Bumped whenever the archived layout of the graph or the manifest changes. An archive written by
/// a different version of the model is not readable, and there is no migration: rebuild it.
pub const FORMAT_VERSION: u32 = 1;

/// Size of the fixed header, in bytes.
const HEADER_LEN: usize = 32;

/// Both archives start on a multiple of this, so neither is ever misaligned.
const ALIGNMENT: usize = 16;

#[derive(Debug)]
pub enum SnapshotError {
    Io(io::Error),
    /// The file is shorter than the header, or one of the two archives does not fit.
    Truncated {
        len: usize,
    },
    /// The first eight bytes are not [`MAGIC`], so this is not a snapshot.
    NotASnapshot,
    /// The file was written by a different model layout.
    VersionMismatch {
        found: u32,
        expected: u32,
    },
    /// The mapping does not satisfy the alignment an archive requires.
    Misaligned {
        required: usize,
    },
    /// An archive is malformed, or could not be walked back into owned values.
    Archive(RkyvError),
}

impl fmt::Display for SnapshotError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SnapshotError::Io(error) => write!(f, "snapshot io error: {error}"),
            SnapshotError::Truncated { len } => {
                write!(f, "snapshot is truncated: {len} bytes, need more than {HEADER_LEN}")
            }
            SnapshotError::NotASnapshot => write!(f, "file is not a rubydex snapshot"),
            SnapshotError::VersionMismatch { found, expected } => write!(
                f,
                "snapshot format version {found} cannot be read by this build, which writes version {expected}"
            ),
            SnapshotError::Misaligned { required } => {
                write!(f, "snapshot mapping is not aligned to {required} bytes")
            }
            SnapshotError::Archive(error) => write!(f, "snapshot archive error: {error}"),
        }
    }
}

impl std::error::Error for SnapshotError {}

impl From<io::Error> for SnapshotError {
    fn from(error: io::Error) -> Self {
        SnapshotError::Io(error)
    }
}

impl From<RkyvError> for SnapshotError {
    fn from(error: RkyvError) -> Self {
        SnapshotError::Archive(error)
    }
}

/// Writes `graph` to `path`, replacing any file already there, and returns the bytes written.
///
/// `roots` are the paths that were indexed. They bound the manifest, so a later reload knows where
/// new files could appear. Building the manifest stats every file once.
///
/// Take the snapshot after resolution. Serializing costs one pass over the graph plus one write of
/// the whole archive, so this is much more expensive than [`Snapshot::open`]; the point is to pay
/// it once and read it many times.
///
/// # Errors
///
/// Returns [`SnapshotError::Archive`] if either archive cannot be serialized, and
/// [`SnapshotError::Io`] if the file cannot be written.
pub fn write<P: AsRef<Path>>(graph: &Graph, roots: &[PathBuf], path: P) -> Result<u64, SnapshotError> {
    let manifest = Manifest::build(graph, roots);
    let manifest_archive = rkyv::to_bytes::<RkyvError>(&manifest)?;
    let graph_archive = rkyv::to_bytes::<RkyvError>(graph)?;

    // The recorded length is the archive's exact length. rkyv finds an archive's root by counting
    // back from the end of the slice it is given, so padding must never sit inside that slice; it
    // goes between the two archives instead, purely to align the second one.
    let manifest_len = manifest_archive.len();
    let padding = (ALIGNMENT - manifest_len % ALIGNMENT) % ALIGNMENT;

    let mut header = [0u8; HEADER_LEN];
    header[..8].copy_from_slice(MAGIC);
    header[8..12].copy_from_slice(&FORMAT_VERSION.to_le_bytes());
    header[16..24].copy_from_slice(&(manifest_len as u64).to_le_bytes());

    let mut file = File::create(path.as_ref())?;
    file.write_all(&header)?;
    file.write_all(&manifest_archive)?;
    file.write_all(&vec![0u8; padding])?;
    file.write_all(&graph_archive)?;
    file.flush()?;

    Ok((HEADER_LEN + manifest_len + padding + graph_archive.len()) as u64)
}

/// A memory-mapped snapshot file.
///
/// The map stays alive for as long as this value does, and every reference handed out by
/// [`Snapshot::graph`] or [`Snapshot::manifest`] borrows from it.
pub struct Snapshot {
    map: Mmap,
    manifest_len: usize,
    graph_offset: usize,
}

impl Snapshot {
    /// Maps `path` and checks its header.
    ///
    /// This reads neither archive. It is a constant-time operation regardless of how large the
    /// graph is, and it leaves the resident set almost untouched; pages arrive as queries reach
    /// them.
    ///
    /// # Errors
    ///
    /// Returns [`SnapshotError::NotASnapshot`], [`SnapshotError::VersionMismatch`],
    /// [`SnapshotError::Truncated`] or [`SnapshotError::Misaligned`] if the file cannot be used,
    /// and [`SnapshotError::Io`] if it cannot be opened or mapped.
    pub fn open<P: AsRef<Path>>(path: P) -> Result<Self, SnapshotError> {
        let file = File::open(path.as_ref())?;
        // SAFETY: mapping is only unsound if another process mutates the file underneath us. A
        // snapshot is written once and then treated as immutable.
        let map = unsafe { Mmap::map(&file)? };

        if map.len() <= HEADER_LEN {
            return Err(SnapshotError::Truncated { len: map.len() });
        }
        if &map[..8] != MAGIC {
            return Err(SnapshotError::NotASnapshot);
        }

        let mut version_bytes = [0u8; 4];
        version_bytes.copy_from_slice(&map[8..12]);
        let found = u32::from_le_bytes(version_bytes);
        if found != FORMAT_VERSION {
            return Err(SnapshotError::VersionMismatch {
                found,
                expected: FORMAT_VERSION,
            });
        }

        let mut manifest_len_bytes = [0u8; 8];
        manifest_len_bytes.copy_from_slice(&map[16..24]);
        let manifest_len = usize::try_from(u64::from_le_bytes(manifest_len_bytes))
            .map_err(|_| SnapshotError::Truncated { len: map.len() })?;

        // The graph archive begins after the manifest plus whatever padding realigned it.
        let graph_offset = (HEADER_LEN + manifest_len).next_multiple_of(ALIGNMENT);
        if graph_offset >= map.len() {
            return Err(SnapshotError::Truncated { len: map.len() });
        }
        if !(map.as_ptr() as usize).is_multiple_of(ALIGNMENT) {
            return Err(SnapshotError::Misaligned { required: ALIGNMENT });
        }

        Ok(Snapshot {
            map,
            manifest_len,
            graph_offset,
        })
    }

    /// The archived graph, borrowed straight out of the memory map.
    ///
    /// Reading through this reference never allocates. Use it for anything that only needs to read
    /// the graph; use [`Snapshot::to_graph`] when the graph has to be mutated.
    ///
    /// The archive is trusted: [`Snapshot::open`] checks the header but not the body, because
    /// walking the body would page in the whole file and give up the reason to use a memory map at
    /// all. Call [`Snapshot::validate`] first when the file may not be your own.
    #[must_use]
    pub fn graph(&self) -> &ArchivedGraph {
        // SAFETY: `open` verified the magic, the format version and the alignment. The body is
        // trusted, which is the documented contract of this method.
        unsafe { rkyv::access_unchecked::<ArchivedGraph>(self.graph_bytes()) }
    }

    /// The archived manifest, borrowed straight out of the memory map.
    ///
    /// Touching this pages in only the manifest region, never the graph.
    #[must_use]
    pub fn manifest(&self) -> &ArchivedManifest {
        // SAFETY: as for `graph`.
        unsafe { rkyv::access_unchecked::<ArchivedManifest>(self.manifest_bytes()) }
    }

    /// Reports which files changed since the snapshot was written.
    ///
    /// This is the cheap way to decide whether the archived graph can be used as is. It stats the
    /// files and directories the manifest knows about and re-reads only the directories whose mtime
    /// moved; it never walks the workspace and never touches the graph archive.
    #[must_use]
    pub fn changes(&self, verification: Verification) -> Changes {
        self.manifest().diff(verification)
    }

    /// Checks every pointer and every length in both archives.
    ///
    /// This reads the whole file, so it costs time proportional to the archive and it makes the
    /// whole archive resident. Prefer [`Snapshot::graph`] for a file you wrote yourself.
    ///
    /// # Errors
    ///
    /// Returns [`SnapshotError::Archive`] if either archive is malformed.
    pub fn validate(&self) -> Result<&ArchivedGraph, SnapshotError> {
        rkyv::access::<ArchivedManifest, RkyvError>(self.manifest_bytes())?;
        Ok(rkyv::access::<ArchivedGraph, RkyvError>(self.graph_bytes())?)
    }

    /// Walks the archive and rebuilds an owned, mutable [`Graph`].
    ///
    /// This allocates the entire graph, so it costs about as much memory as indexing does. It is
    /// still far cheaper than re-indexing, and it is the path to take when the caller needs to
    /// apply document changes afterwards.
    ///
    /// # Errors
    ///
    /// Returns [`SnapshotError::Archive`] if the archive cannot be walked.
    pub fn to_graph(&self) -> Result<Graph, SnapshotError> {
        Ok(rkyv::deserialize::<Graph, RkyvError>(self.graph())?)
    }

    /// Rebuilds an owned graph and brings it up to date with the workspace.
    ///
    /// This is the whole point of carrying a manifest. Rather than re-indexing everything, it
    /// re-indexes only what [`Snapshot::changes`] reports:
    ///
    /// - documents whose file disappeared are dropped,
    /// - modified and newly discovered files are indexed again,
    /// - resolution then runs over the invalidated subset only.
    ///
    /// Dropping and re-indexing a document feeds the same incremental invalidation path an editor
    /// uses, so the resolver's worklist already holds exactly the units that need redoing. An
    /// unchanged workspace skips indexing entirely and only pays for the deserialize.
    ///
    /// Returns the graph together with what changed, so a caller can report or log it.
    ///
    /// # Errors
    ///
    /// Returns [`SnapshotError::Archive`] if the archive cannot be walked.
    pub fn catch_up(
        &self,
        verification: Verification,
        backend: IndexerBackend,
    ) -> Result<(Graph, Changes, Vec<Errors>), SnapshotError> {
        let changes = self.changes(verification);
        let mut graph = self.to_graph()?;

        for (uri_id, _) in &changes.removed {
            graph.delete_document_by_id(*uri_id);
        }

        let mut errors = Vec::new();
        if !changes.modified.is_empty() || !changes.added.is_empty() {
            let stale: Vec<PathBuf> = changes.modified.iter().chain(&changes.added).cloned().collect();
            errors = indexing::index_files(&mut graph, stale, backend);
        }

        // Always resolve: a deletion produces pending work without producing any file to index.
        if !changes.is_empty() {
            Resolver::new(&mut graph).resolve();
        }

        Ok((graph, changes, errors))
    }

    /// Size of the mapped file in bytes, header included.
    #[must_use]
    pub fn len(&self) -> usize {
        self.map.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.map.len() <= HEADER_LEN
    }

    fn manifest_bytes(&self) -> &[u8] {
        &self.map[HEADER_LEN..HEADER_LEN + self.manifest_len]
    }

    fn graph_bytes(&self) -> &[u8] {
        &self.map[self.graph_offset..]
    }
}

impl fmt::Debug for Snapshot {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Snapshot")
            .field("len", &self.map.len())
            .field("manifest_len", &self.manifest_len)
            .field("graph_offset", &self.graph_offset)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::indexing::{LanguageId, index_source};
    use crate::model::ids::{UriId, declaration_id_from_lookup_name};
    use crate::resolution::Resolver;

    /// Spells a path as a URI the way indexing does. Building one with `format!` breaks on
    /// Windows, where a path is `C:\\...` rather than something that can follow `file://`.
    fn file_uri(path: &Path) -> String {
        url::Url::from_file_path(path)
            .expect("test path is absolute")
            .to_string()
    }

    /// Indexes every `.rb` file under `directory` and resolves the result.
    fn graph_for(directory: &Path) -> Graph {
        let mut graph = Graph::new();
        let mut stack = vec![directory.to_path_buf()];
        while let Some(current) = stack.pop() {
            for entry in std::fs::read_dir(&current).unwrap().flatten() {
                let path = entry.path();
                if path.is_dir() {
                    stack.push(path);
                } else if path.extension().is_some_and(|ext| ext == "rb") {
                    let source = std::fs::read_to_string(&path).unwrap();
                    index_source(&mut graph, &file_uri(&path), &source, &LanguageId::Ruby);
                }
            }
        }
        Resolver::new(&mut graph).resolve();
        graph
    }

    /// Builds a workspace, snapshots it, and hands back the pieces a staleness test needs.
    ///
    /// The snapshot lives beside the workspace, never inside it. Writing a file into an indexed
    /// directory bumps that directory's mtime, which the tree would correctly report as a change.
    fn workspace(files: &[(&str, &str)]) -> (tempfile::TempDir, PathBuf, PathBuf) {
        let directory = tempfile::tempdir().unwrap();
        let base = crate::path_helpers::resolved(directory.path()).unwrap();
        let root = base.join("workspace");
        std::fs::create_dir_all(&root).unwrap();

        for (name, source) in files {
            let path = root.join(name);
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(&path, source).unwrap();
        }

        let snapshot_path = base.join("graph.rdxsnap");
        let graph = graph_for(&root);
        write(&graph, std::slice::from_ref(&root), &snapshot_path).unwrap();

        (directory, root, snapshot_path)
    }

    /// mtime has finite resolution, so a rewrite in the same instant can look unchanged. Push the
    /// clock forward rather than sleeping.
    fn rewrite(path: &Path, source: &str) {
        std::fs::write(path, source).unwrap();
        let future = std::time::SystemTime::now() + std::time::Duration::from_secs(2);
        let file = std::fs::OpenOptions::new().write(true).open(path).unwrap();
        file.set_modified(future).unwrap();
    }

    fn resolved_graph() -> Graph {
        let mut graph = Graph::new();
        index_source(
            &mut graph,
            "file:///snapshot_test.rb",
            "module Outer\n  class Inner < Base\n    def call; end\n  end\nend\n",
            &LanguageId::Ruby,
        );
        Resolver::new(&mut graph).resolve();
        graph
    }

    #[test]
    fn round_trips_a_resolved_graph() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("graph.rdxsnap");

        let graph = resolved_graph();
        let expected_declarations = graph.declarations().len();
        let expected_definitions = graph.definitions().len();

        let written = write(&graph, &[], &path).unwrap();
        assert!(written > HEADER_LEN as u64);

        let snapshot = Snapshot::open(&path).unwrap();
        let restored = snapshot.to_graph().unwrap();

        assert_eq!(restored.declarations().len(), expected_declarations);
        assert_eq!(restored.definitions().len(), expected_definitions);
        assert!(restored.get("Outer::Inner").is_some());
        assert!(restored.get("Outer::Inner#call()").is_some());
    }

    #[test]
    fn reads_declarations_without_deserializing() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("graph.rdxsnap");

        let graph = resolved_graph();
        write(&graph, &[], &path).unwrap();

        let snapshot = Snapshot::open(&path).unwrap();
        let archived = snapshot.graph();

        assert_eq!(archived.declarations().len(), graph.declarations().len());

        // A point lookup against the archive uses the same content-hashed id as the live graph.
        let id = declaration_id_from_lookup_name("Outer::Inner");
        assert!(archived.declaration(id).is_some());
        assert!(
            archived
                .declaration(declaration_id_from_lookup_name("Nope::Missing"))
                .is_none()
        );
    }

    #[test]
    fn validate_accepts_a_freshly_written_archive() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("graph.rdxsnap");

        write(&resolved_graph(), &[], &path).unwrap();

        let snapshot = Snapshot::open(&path).unwrap();
        assert!(snapshot.validate().is_ok());
    }

    #[test]
    fn rejects_a_file_that_is_not_a_snapshot() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("not_a_snapshot");
        std::fs::write(
            &path,
            b"this file is long enough but it has entirely the wrong magic bytes",
        )
        .unwrap();

        assert!(matches!(Snapshot::open(&path), Err(SnapshotError::NotASnapshot)));
    }

    #[test]
    fn rejects_a_future_format_version() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("graph.rdxsnap");
        write(&resolved_graph(), &[], &path).unwrap();

        let mut bytes = std::fs::read(&path).unwrap();
        bytes[8..12].copy_from_slice(&(FORMAT_VERSION + 1).to_le_bytes());
        std::fs::write(&path, &bytes).unwrap();

        assert!(matches!(
            Snapshot::open(&path),
            Err(SnapshotError::VersionMismatch { found, expected })
                if found == FORMAT_VERSION + 1 && expected == FORMAT_VERSION
        ));
    }

    #[test]
    fn rejects_a_truncated_file() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("graph.rdxsnap");
        std::fs::write(&path, MAGIC).unwrap();

        assert!(matches!(
            Snapshot::open(&path),
            Err(SnapshotError::Truncated { len: 8 })
        ));
    }

    /// A snapshot does not carry the line index, so a restored document has to rebuild one from
    /// its own file. This is the one place where a restore reaches back to disk, and locations are
    /// wrong if it misbehaves, so pin the behaviour down.
    #[test]
    fn restored_documents_rebuild_their_line_index_from_disk() {
        let directory = tempfile::tempdir().unwrap();
        let source_path = directory.path().join("shapes.rb");
        let source = "class Circle\n  def area\n    3\n  end\nend\n";
        std::fs::write(&source_path, source).unwrap();

        let uri = file_uri(&source_path);
        let mut graph = Graph::new();
        index_source(&mut graph, &uri, source, &LanguageId::Ruby);
        Resolver::new(&mut graph).resolve();

        let uri_id = UriId::from(uri.as_str());
        let definition_id = *graph.documents().get(&uri_id).unwrap().definitions().first().unwrap();
        let expected = graph
            .definitions()
            .get(&definition_id)
            .unwrap()
            .offset()
            .to_location(graph.documents().get(&uri_id).unwrap());

        let snapshot_path = directory.path().join("graph.rdxsnap");
        write(&graph, &[], &snapshot_path).unwrap();
        drop(graph);

        let snapshot = Snapshot::open(&snapshot_path).unwrap();
        let restored = snapshot.to_graph().unwrap();
        let document = restored.documents().get(&uri_id).unwrap();
        let actual = restored
            .definitions()
            .get(&definition_id)
            .unwrap()
            .offset()
            .to_location(document);

        assert_eq!(actual.start_line(), expected.start_line());
        assert_eq!(actual.start_col(), expected.start_col());
        assert_eq!(actual.end_line(), expected.end_line());
        assert_eq!(actual.end_col(), expected.end_col());
        assert_eq!(expected.start_line(), 0, "class Circle starts on the first line");
    }

    /// The lazy rebuild reads the document's file. When that file is gone the index must degrade to
    /// an empty one rather than panic, because a snapshot never tracks whether files still exist.
    #[test]
    fn restored_documents_survive_a_missing_file() {
        let directory = tempfile::tempdir().unwrap();
        let source_path = directory.path().join("gone.rb");
        let source = "class Gone\nend\n";
        std::fs::write(&source_path, source).unwrap();

        let uri = file_uri(&source_path);
        let mut graph = Graph::new();
        index_source(&mut graph, &uri, source, &LanguageId::Ruby);
        Resolver::new(&mut graph).resolve();

        let snapshot_path = directory.path().join("graph.rdxsnap");
        write(&graph, &[], &snapshot_path).unwrap();
        drop(graph);
        std::fs::remove_file(&source_path).unwrap();

        let snapshot = Snapshot::open(&snapshot_path).unwrap();
        let restored = snapshot.to_graph().unwrap();
        let document = restored.documents().get(&UriId::from(uri.as_str())).unwrap();

        assert_eq!(document.line_index().len(), 0.into());
    }

    // ---------------------------------------------------------------- staleness

    #[test]
    fn an_untouched_workspace_reports_no_changes() {
        let (_directory, _root, snapshot_path) =
            workspace(&[("a.rb", "class A; end\n"), ("nested/b.rb", "class B; end\n")]);

        let snapshot = Snapshot::open(&snapshot_path).unwrap();
        let changes = snapshot.changes(Verification::Metadata);

        assert!(changes.is_empty(), "{changes:?}");
        assert_eq!(changes.unchanged, 2);
        assert_eq!(snapshot.manifest().file_count(), 2);
        assert!(snapshot.manifest().dir_count() >= 2, "root and nested are both tracked");
        assert_ne!(snapshot.manifest().root(), 0);
    }

    #[test]
    fn an_edited_file_is_reported_as_modified() {
        let (_directory, root, snapshot_path) =
            workspace(&[("a.rb", "class A; end\n"), ("nested/b.rb", "class B; end\n")]);

        rewrite(&root.join("nested/b.rb"), "class B\n  def extra; end\nend\n");

        let snapshot = Snapshot::open(&snapshot_path).unwrap();
        let changes = snapshot.changes(Verification::Metadata);

        assert_eq!(changes.modified, vec![root.join("nested/b.rb")]);
        assert!(changes.added.is_empty());
        assert!(changes.removed.is_empty());
        assert_eq!(changes.unchanged, 1);
    }

    /// Editing a file bumps only that file's mtime. Discovering it therefore has to come from the
    /// file sweep, not from the directory sweep.
    #[test]
    fn content_verification_catches_an_mtime_preserving_edit() {
        let (_directory, root, snapshot_path) = workspace(&[("a.rb", "class A; end\n")]);

        let path = root.join("a.rb");
        let original = std::fs::metadata(&path).unwrap().modified().unwrap();
        // Same length, so size cannot give it away either.
        std::fs::write(&path, "class Z; end\n").unwrap();
        std::fs::OpenOptions::new()
            .write(true)
            .open(&path)
            .unwrap()
            .set_modified(original)
            .unwrap();

        let snapshot = Snapshot::open(&snapshot_path).unwrap();

        let metadata_only = snapshot.changes(Verification::Metadata);
        assert!(
            metadata_only.is_empty(),
            "size and mtime both match, so this edit is invisible"
        );

        let by_content = snapshot.changes(Verification::Content);
        assert_eq!(by_content.modified, vec![path]);
    }

    #[test]
    fn a_new_file_is_found_through_its_directory() {
        let (_directory, root, snapshot_path) = workspace(&[("a.rb", "class A; end\n")]);

        rewrite(&root.join("c.rb"), "class C; end\n");
        // Adding an entry bumps the containing directory's mtime by itself, which is what the
        // directory sweep looks for.

        let snapshot = Snapshot::open(&snapshot_path).unwrap();
        let changes = snapshot.changes(Verification::Metadata);

        assert_eq!(changes.added, vec![root.join("c.rb")]);
        assert!(changes.modified.is_empty());
        assert!(changes.removed.is_empty());
    }

    /// A directory the manifest never saw is walked in full, so files nested inside a new tree are
    /// still found.
    #[test]
    fn a_new_nested_directory_is_walked() {
        let (_directory, root, snapshot_path) = workspace(&[("a.rb", "class A; end\n")]);

        std::fs::create_dir_all(root.join("fresh/deeper")).unwrap();
        std::fs::write(root.join("fresh/deeper/d.rb"), "class D; end\n").unwrap();

        let snapshot = Snapshot::open(&snapshot_path).unwrap();
        let changes = snapshot.changes(Verification::Metadata);

        assert_eq!(changes.added, vec![root.join("fresh/deeper/d.rb")]);
    }

    #[test]
    fn a_deleted_file_is_reported_with_its_document_id() {
        let (_directory, root, snapshot_path) =
            workspace(&[("a.rb", "class A; end\n"), ("nested/b.rb", "class B; end\n")]);

        let gone = root.join("nested/b.rb");
        std::fs::remove_file(&gone).unwrap();

        let snapshot = Snapshot::open(&snapshot_path).unwrap();
        let changes = snapshot.changes(Verification::Metadata);

        assert_eq!(changes.removed.len(), 1);
        let (uri_id, path) = &changes.removed[0];
        assert_eq!(path, &gone);
        assert_eq!(*uri_id, UriId::from(file_uri(&gone).as_str()));
        assert_eq!(changes.unchanged, 1);
    }

    /// The whole point of the tree: an identical workspace produces an identical root.
    #[test]
    fn the_root_hash_is_stable_across_rebuilds() {
        let (_directory, root, snapshot_path) = workspace(&[("a.rb", "class A; end\n")]);

        let first = Snapshot::open(&snapshot_path).unwrap().manifest().root();

        let second_path = root.parent().unwrap().join("again.rdxsnap");
        let graph = graph_for(&root);
        write(&graph, std::slice::from_ref(&root), &second_path).unwrap();
        let second = Snapshot::open(&second_path).unwrap().manifest().root();

        assert_eq!(first, second);
    }

    #[test]
    fn the_root_hash_changes_when_a_file_changes() {
        let (_directory, root, snapshot_path) = workspace(&[("a.rb", "class A; end\n")]);
        let before = Snapshot::open(&snapshot_path).unwrap().manifest().root();

        rewrite(&root.join("a.rb"), "class A\n  def more; end\nend\n");

        let after_path = root.parent().unwrap().join("after.rdxsnap");
        let graph = graph_for(&root);
        write(&graph, std::slice::from_ref(&root), &after_path).unwrap();
        let after = Snapshot::open(&after_path).unwrap().manifest().root();

        assert_ne!(before, after);
    }

    // ---------------------------------------------------------------- catch up

    /// Sorted fully qualified names, the cheapest way to say two graphs agree.
    fn declaration_names(graph: &Graph) -> Vec<String> {
        let mut names: Vec<String> = graph.declarations().values().map(|d| d.name().to_string()).collect();
        names.sort_unstable();
        names
    }

    /// The contract that makes partial indexing worth doing: catching a snapshot up must land on
    /// exactly the graph a full rebuild would have produced.
    #[test]
    fn catching_up_matches_a_full_rebuild() {
        let (_directory, root, snapshot_path) = workspace(&[
            ("a.rb", "class A; end\n"),
            ("nested/b.rb", "class B < A\n  def keep; end\nend\n"),
            ("nested/gone.rb", "class Gone; end\n"),
        ]);

        // One edit, one deletion, one addition, which is every case the manifest reports.
        rewrite(
            &root.join("nested/b.rb"),
            "class B < A\n  def keep; end\n  def added; end\nend\n",
        );
        std::fs::remove_file(root.join("nested/gone.rb")).unwrap();
        rewrite(&root.join("nested/fresh.rb"), "class Fresh < B; end\n");

        let snapshot = Snapshot::open(&snapshot_path).unwrap();
        let (caught_up, changes, errors) = snapshot
            .catch_up(Verification::Metadata, IndexerBackend::RubyIndexer)
            .unwrap();
        assert!(errors.is_empty(), "{errors:?}");

        assert_eq!(changes.modified, vec![root.join("nested/b.rb")]);
        assert_eq!(changes.added, vec![root.join("nested/fresh.rb")]);
        assert_eq!(changes.removed.len(), 1);
        assert_eq!(changes.to_index(), 2, "only the edited and the new file are re-indexed");

        let rebuilt = graph_for(&root);

        assert_eq!(declaration_names(&caught_up), declaration_names(&rebuilt));
        assert_eq!(caught_up.documents().len(), rebuilt.documents().len());
        assert_eq!(caught_up.definitions().len(), rebuilt.definitions().len());

        // The deleted class is gone, the new one is present, and the edit landed.
        assert!(caught_up.get("Gone").is_none());
        assert!(caught_up.get("Fresh").is_some());
        assert!(caught_up.get("B#added()").is_some());
    }

    /// Ancestors are the part most likely to go stale, because they cascade. A new subclass has to
    /// see the parent that the snapshot already knew about.
    #[test]
    fn catching_up_relinks_ancestors_across_the_snapshot_boundary() {
        let (_directory, root, snapshot_path) =
            workspace(&[("base.rb", "class Base\n  def inherited_call; end\nend\n")]);

        rewrite(&root.join("child.rb"), "class Child < Base; end\n");

        let snapshot = Snapshot::open(&snapshot_path).unwrap();
        let (caught_up, changes, _) = snapshot
            .catch_up(Verification::Metadata, IndexerBackend::RubyIndexer)
            .unwrap();

        assert_eq!(changes.to_index(), 1, "only the new file is indexed");

        let rebuilt = graph_for(&root);
        assert_eq!(declaration_names(&caught_up), declaration_names(&rebuilt));

        // The method is only reachable if Child's ancestor chain resolved back to the archived Base.
        let child = caught_up.get("Child").expect("Child exists");
        assert!(!child.is_empty());
        assert!(caught_up.get("Base#inherited_call()").is_some());
    }

    #[test]
    fn catching_up_an_untouched_workspace_indexes_nothing() {
        let (_directory, root, snapshot_path) = workspace(&[("a.rb", "class A; end\n")]);

        let snapshot = Snapshot::open(&snapshot_path).unwrap();
        let (caught_up, changes, _) = snapshot
            .catch_up(Verification::Metadata, IndexerBackend::RubyIndexer)
            .unwrap();

        assert!(changes.is_empty());
        assert_eq!(changes.to_index(), 0);
        assert_eq!(declaration_names(&caught_up), declaration_names(&graph_for(&root)));
    }
}
