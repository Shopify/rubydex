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
//! # What a snapshot does not store
//!
//! A snapshot does not record which files it was built from, so it cannot tell that the workspace
//! changed. The caller owns that decision. A [`Document`](crate::model::document::Document) also
//! archives without its line index, and rebuilds it from disk on first use.
//!
//! # Layout
//!
//! ```text
//! offset 0   magic     8 bytes   "RDXSNAP\0"
//! offset 8   version   4 bytes   little-endian u32
//! offset 12  reserved  4 bytes   zero
//! offset 16  archive   rest of the file
//! ```
//!
//! The header is 16 bytes so that the archive stays aligned: a memory map starts on a page
//! boundary, and 16 divides every alignment the archived graph can ask for.

use std::fmt;
use std::fs::File;
use std::io::{self, Write};
use std::path::Path;

use memmap2::Mmap;
use rkyv::rancor::Error as RkyvError;

use crate::model::graph::{ArchivedGraph, Graph};

/// Identifies a rubydex snapshot file.
const MAGIC: &[u8; 8] = b"RDXSNAP\0";

/// Bumped whenever the archived layout of the graph changes. An archive written by a different
/// version of the model is not readable, and there is no migration: rebuild it from source.
pub const FORMAT_VERSION: u32 = 1;

/// Size of the fixed header, in bytes. Also the alignment the archive is guaranteed.
const HEADER_LEN: usize = 16;

#[derive(Debug)]
pub enum SnapshotError {
    Io(io::Error),
    /// The file is shorter than the header, or holds no archive.
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
    /// The mapping does not satisfy the alignment the archived graph requires.
    Misaligned {
        required: usize,
    },
    /// The archive is malformed, or could not be walked back into an owned graph.
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
/// Take the snapshot after resolution. Serializing costs one pass over the graph plus one write of
/// the whole archive, so this is much more expensive than [`Snapshot::open`]; the point is to pay
/// it once and read it many times.
///
/// # Errors
///
/// Returns [`SnapshotError::Archive`] if the graph cannot be serialized, and
/// [`SnapshotError::Io`] if the file cannot be written.
pub fn write<P: AsRef<Path>>(graph: &Graph, path: P) -> Result<u64, SnapshotError> {
    let archive = rkyv::to_bytes::<RkyvError>(graph)?;

    let mut header = [0u8; HEADER_LEN];
    header[..8].copy_from_slice(MAGIC);
    header[8..12].copy_from_slice(&FORMAT_VERSION.to_le_bytes());

    let mut file = File::create(path.as_ref())?;
    file.write_all(&header)?;
    file.write_all(&archive)?;
    file.flush()?;

    Ok((HEADER_LEN + archive.len()) as u64)
}

/// A memory-mapped snapshot file.
///
/// The map stays alive for as long as this value does, and every reference handed out by
/// [`Snapshot::graph`] borrows from it.
pub struct Snapshot {
    map: Mmap,
}

impl Snapshot {
    /// Maps `path` and checks its header.
    ///
    /// This does not read the archive. It is a constant-time operation regardless of how large the
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

        let required = align_of::<ArchivedGraph>();
        if !(map[HEADER_LEN..].as_ptr() as usize).is_multiple_of(required) {
            return Err(SnapshotError::Misaligned { required });
        }

        Ok(Snapshot { map })
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
        unsafe { rkyv::access_unchecked::<ArchivedGraph>(self.archive()) }
    }

    /// Checks every pointer and every length in the archive, then returns it.
    ///
    /// This reads the whole file, so it costs time proportional to the archive and it makes the
    /// whole archive resident. Prefer [`Snapshot::graph`] for a file you wrote yourself.
    ///
    /// # Errors
    ///
    /// Returns [`SnapshotError::Archive`] if the archive is malformed.
    pub fn validate(&self) -> Result<&ArchivedGraph, SnapshotError> {
        Ok(rkyv::access::<ArchivedGraph, RkyvError>(self.archive())?)
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

    /// Size of the mapped file in bytes, header included.
    #[must_use]
    pub fn len(&self) -> usize {
        self.map.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.map.len() <= HEADER_LEN
    }

    fn archive(&self) -> &[u8] {
        &self.map[HEADER_LEN..]
    }
}

impl fmt::Debug for Snapshot {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Snapshot").field("len", &self.map.len()).finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::indexing::{LanguageId, index_source};
    use crate::model::ids::{UriId, declaration_id_from_lookup_name};
    use crate::resolution::Resolver;

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

        let written = write(&graph, &path).unwrap();
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
        write(&graph, &path).unwrap();

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

        write(&resolved_graph(), &path).unwrap();

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
        write(&resolved_graph(), &path).unwrap();

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

        let uri = format!("file://{}", source_path.display());
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
        write(&graph, &snapshot_path).unwrap();
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

        let uri = format!("file://{}", source_path.display());
        let mut graph = Graph::new();
        index_source(&mut graph, &uri, source, &LanguageId::Ruby);
        Resolver::new(&mut graph).resolve();

        let snapshot_path = directory.path().join("graph.rdxsnap");
        write(&graph, &snapshot_path).unwrap();
        drop(graph);
        std::fs::remove_file(&source_path).unwrap();

        let snapshot = Snapshot::open(&snapshot_path).unwrap();
        let restored = snapshot.to_graph().unwrap();
        let document = restored.documents().get(&UriId::from(uri.as_str())).unwrap();

        assert_eq!(document.line_index().len(), 0.into());
    }
}
