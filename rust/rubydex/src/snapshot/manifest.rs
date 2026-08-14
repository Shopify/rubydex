//! A Merkle tree over the files a snapshot was built from, used to decide what to re-index.
//!
//! # Why a tree, and where it actually pays
//!
//! The root hash answers "did anything change" in one comparison, but a root is only as cheap as
//! the leaves underneath it, and the leaves come from the file system. Measured on a 110,447 file
//! workspace, the costs are lopsided:
//!
//! | step                                   | cost    |
//! |----------------------------------------|---------|
//! | recursive walk of the tree             | 1.415 s |
//! | `stat` every known file, in parallel   | 74 ms   |
//! | `stat` every known directory, parallel | 22 ms   |
//! | re-read one changed directory          | 89 us   |
//! | fold every leaf into a root            | 3.7 ms  |
//!
//! So the folding is free and the comparison was never the problem. The walk is. The tree earns its
//! place at the **interior** nodes, not the leaves, because of one file system property:
//!
//! - editing a file bumps that file's mtime, and not its directory's,
//! - adding, removing or renaming an entry bumps the containing directory's mtime.
//!
//! Recording directories as interior nodes therefore lets a reload skip the walk entirely. It stats
//! the files it already knows to catch edits and deletions, stats the directories it already knows
//! to catch insertions, and only re-reads the handful of directories whose mtime moved. A full
//! discovery pass collapses into two flat `stat` sweeps.
//!
//! Leaves also carry the `xxh3` content hash that [`Document::content_hash`] already stores, so
//! [`Verification::Content`] can confirm a suspicion exactly instead of trusting metadata.
//!
//! # What this does not promise
//!
//! [`Verification::Metadata`] trusts size and mtime, the same bet `make`, `cargo` and Bazel make. A
//! write that restores the previous mtime and length slips past it. [`Verification::Content`] reads
//! every byte and does not guess, at roughly twenty times the cost.

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::time::UNIX_EPOCH;

use glob::Pattern;
use xxhash_rust::xxh3::Xxh3Default;

use crate::model::graph::Graph;
use crate::model::ids::UriId;
use crate::path_helpers;

/// How hard [`ArchivedManifest::diff`] works to decide a file changed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Verification {
    /// Compare size and mtime only. Two `stat` sweeps, no file contents read.
    #[default]
    Metadata,
    /// Re-hash every file and compare against the recorded content hash. Exact, and much slower.
    Content,
}

/// One file the snapshot was built from.
#[derive(Debug, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
pub struct FileEntry {
    /// Absolute path, as resolved from the document's URI.
    pub path: Box<str>,
    /// The document this file produced, so a caller can drop it when the file is gone.
    pub uri_id: UriId,
    pub size: u64,
    pub mtime_ns: u64,
    /// `xxh3` of the file's contents, mirroring `Document::content_hash`.
    pub content_hash: u64,
}

/// One directory, an interior node of the tree.
#[derive(Debug, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
pub struct DirEntry {
    pub path: Box<str>,
    pub mtime_ns: u64,
    /// Merkle hash of this directory's direct children, files and subdirectories alike.
    pub subtree: u64,
}

/// The Merkle tree recorded next to a graph in a snapshot.
#[derive(Debug, Default, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
pub struct Manifest {
    /// The paths that were indexed, needed to bound the search for new directories.
    pub roots: Vec<Box<str>>,
    /// Every indexed file, sorted by path.
    pub files: Vec<FileEntry>,
    /// Every directory containing an indexed file, sorted by path.
    pub dirs: Vec<DirEntry>,
    /// The exclusion patterns in force when the snapshot was built.
    ///
    /// Recorded for two reasons. Discovering a new file has to apply the same rule the original
    /// listing did, or a reload would resurrect files the workspace deliberately skips. And a
    /// caller can compare them against the current configuration: different patterns mean a
    /// different file set, which no amount of per-file checking would notice.
    pub excluded: Vec<Box<str>>,
    /// Hash of the whole tree. Equal roots mean equal trees.
    pub root: u64,
}

/// What changed between a manifest and the file system it describes.
#[derive(Debug, Default)]
pub struct Changes {
    /// Files whose contents differ, and files that appeared. Both need re-indexing.
    pub modified: Vec<PathBuf>,
    pub added: Vec<PathBuf>,
    /// Documents whose file is gone. Both the id and the path, so a caller can report either.
    pub removed: Vec<(UriId, PathBuf)>,
    pub unchanged: usize,
}

impl Changes {
    /// Whether the snapshot still describes the workspace exactly.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.modified.is_empty() && self.added.is_empty() && self.removed.is_empty()
    }

    /// Total number of files a caller has to index to catch up.
    #[must_use]
    pub fn to_index(&self) -> usize {
        self.modified.len() + self.added.len()
    }
}

fn mtime_ns(metadata: &std::fs::Metadata) -> u64 {
    metadata
        .modified()
        .ok()
        .and_then(|time| time.duration_since(UNIX_EPOCH).ok())
        .map_or(0, |elapsed| u64::try_from(elapsed.as_nanos()).unwrap_or(u64::MAX))
}

/// Hash of a leaf: identity plus everything that decides whether it is stale.
fn leaf_hash(path: &str, size: u64, mtime: u64, content_hash: u64) -> u64 {
    let mut hasher = Xxh3Default::new();
    hasher.update(path.as_bytes());
    hasher.update(&size.to_le_bytes());
    hasher.update(&mtime.to_le_bytes());
    hasher.update(&content_hash.to_le_bytes());
    hasher.digest()
}

/// Combines child hashes into a parent. Children arrive sorted, so the result is order independent
/// in practice and still sensitive to any child changing.
fn combine(path: &str, mtime: u64, children: &[u64]) -> u64 {
    let mut hasher = Xxh3Default::new();
    hasher.update(path.as_bytes());
    hasher.update(&mtime.to_le_bytes());
    for child in children {
        hasher.update(&child.to_le_bytes());
    }
    hasher.digest()
}

/// Splits `items` across the available cores and runs `work` on each chunk.
///
/// The graph and the manifest are borrowed out of a memory map, so the `'static` bound on
/// [`JobQueue`](crate::job_queue::JobQueue) does not fit. Scoped threads borrow instead.
fn parallel_chunks<T: Sync, R: Send>(items: &[T], work: impl Fn(&[T]) -> R + Sync) -> Vec<R> {
    if items.is_empty() {
        return Vec::new();
    }

    let workers = std::thread::available_parallelism().map_or(4, std::num::NonZeroUsize::get);
    let chunk_size = items.len().div_ceil(workers).max(1);

    std::thread::scope(|scope| {
        let handles: Vec<_> = items
            .chunks(chunk_size)
            .map(|chunk| scope.spawn(|| work(chunk)))
            .collect();
        handles
            .into_iter()
            .map(|handle| handle.join().expect("manifest worker panicked"))
            .collect()
    })
}

impl Manifest {
    /// Records every file in `graph`, plus every directory on the way down from `roots`.
    ///
    /// This stats each file once. It runs after resolution, alongside writing the snapshot, so it
    /// is on the expensive side of the trade rather than the reload side.
    ///
    /// # Panics
    ///
    /// Panics if a worker thread panics while stat-ing files.
    #[must_use]
    pub fn build(graph: &Graph, roots: &[PathBuf]) -> Manifest {
        // Documents hold URIs; the manifest needs real paths to stat.
        let documents: Vec<(UriId, PathBuf, u64)> = graph
            .documents()
            .iter()
            .filter_map(|(uri_id, document)| {
                document
                    .file_path()
                    .map(|path| (*uri_id, path, document.content_hash()))
            })
            .collect();

        let mut files: Vec<FileEntry> = parallel_chunks(&documents, |chunk| {
            chunk
                .iter()
                .filter_map(|(uri_id, path, content_hash)| {
                    let metadata = std::fs::metadata(path).ok()?;
                    Some(FileEntry {
                        path: path.to_string_lossy().into_owned().into_boxed_str(),
                        uri_id: *uri_id,
                        size: metadata.len(),
                        mtime_ns: mtime_ns(&metadata),
                        content_hash: *content_hash,
                    })
                })
                .collect::<Vec<_>>()
        })
        .into_iter()
        .flatten()
        .collect();
        files.sort_unstable_by(|left, right| left.path.cmp(&right.path));

        // Every ancestor directory of an indexed file is an interior node. Anything above the roots
        // is outside the snapshot's world and is not tracked.
        //
        // Roots must be spelled the same way a document path is, or `starts_with` never matches and
        // no directory is recorded. `fs::canonicalize` alone is not enough: on Windows it returns a
        // verbatim `\\?\C:\...` path, while a document path comes back from its URI spelled plainly.
        // `path_helpers::resolved` canonicalizes and then simplifies, exactly as listing does.
        let root_paths: Vec<PathBuf> = roots
            .iter()
            .filter_map(|root| path_helpers::resolved(root).ok())
            .collect();
        let mut directories: BTreeSet<PathBuf> = BTreeSet::new();
        for entry in &files {
            let mut current = Path::new(entry.path.as_ref()).parent();
            while let Some(directory) = current {
                let inside = root_paths.iter().any(|root| directory.starts_with(root));
                if !inside || !directories.insert(directory.to_path_buf()) {
                    break;
                }
                current = directory.parent();
            }
        }

        // Children first, so a parent can fold hashes that already exist.
        let mut children: BTreeMap<PathBuf, Vec<u64>> = BTreeMap::new();
        for entry in &files {
            let hash = leaf_hash(&entry.path, entry.size, entry.mtime_ns, entry.content_hash);
            if let Some(parent) = Path::new(entry.path.as_ref()).parent() {
                children.entry(parent.to_path_buf()).or_default().push(hash);
            }
        }

        let mut subtrees: HashMap<PathBuf, u64> = HashMap::new();
        let mut dirs: Vec<DirEntry> = Vec::with_capacity(directories.len());
        // Deepest first: a parent's children are all resolved before the parent is folded.
        for directory in directories.iter().rev() {
            let mtime = std::fs::metadata(directory).as_ref().map_or(0, mtime_ns);
            let mut child_hashes = children.remove(directory).unwrap_or_default();
            child_hashes.sort_unstable();

            let display = directory.to_string_lossy().into_owned();
            let subtree = combine(&display, mtime, &child_hashes);
            subtrees.insert(directory.clone(), subtree);

            if let Some(parent) = directory.parent() {
                children.entry(parent.to_path_buf()).or_default().push(subtree);
            }

            dirs.push(DirEntry {
                path: display.into_boxed_str(),
                mtime_ns: mtime,
                subtree,
            });
        }
        dirs.sort_unstable_by(|left, right| left.path.cmp(&right.path));

        let mut top: Vec<u64> = root_paths
            .iter()
            .filter_map(|root| subtrees.get(root).copied())
            .collect();
        top.sort_unstable();
        let root = combine("", 0, &top);

        let mut excluded: Vec<Box<str>> = graph.excluded_patterns().into_iter().collect();
        excluded.sort_unstable();

        Manifest {
            roots: root_paths
                .iter()
                .map(|path| path.to_string_lossy().into_owned().into_boxed_str())
                .collect(),
            files,
            dirs,
            excluded,
            root,
        }
    }
}

impl ArchivedManifest {
    /// The recorded Merkle root. Two snapshots with the same root describe the same tree.
    #[must_use]
    pub fn root(&self) -> u64 {
        self.root.to_native()
    }

    #[must_use]
    pub fn file_count(&self) -> usize {
        self.files.len()
    }

    #[must_use]
    pub fn dir_count(&self) -> usize {
        self.dirs.len()
    }

    /// The exclusion patterns the snapshot was built with, sorted.
    #[must_use]
    pub fn excluded_patterns(&self) -> Vec<Box<str>> {
        self.excluded
            .iter()
            .map(|pattern| Box::from(pattern.as_ref()))
            .collect()
    }

    /// Whether `current` is the same exclusion set the snapshot was built with.
    ///
    /// A different set means a different file list, which per-file checking cannot detect: a
    /// newly excluded file still exists and still matches its recorded metadata. A caller that
    /// sees `false` here must re-index from scratch.
    #[must_use]
    pub fn excludes_match<S: std::hash::BuildHasher>(&self, current: &HashSet<Box<str>, S>) -> bool {
        if self.excluded.len() != current.len() {
            return false;
        }
        self.excluded.iter().all(|pattern| current.contains(pattern.as_ref()))
    }

    /// Compares the recorded tree against the file system and reports what moved.
    ///
    /// Runs the two flat `stat` sweeps described on this module, then re-reads only the directories
    /// whose mtime changed. It never walks the workspace.
    ///
    /// # Panics
    ///
    /// Panics if a worker thread panics while stat-ing.
    #[must_use]
    pub fn diff(&self, verification: Verification) -> Changes {
        let files = self.files.as_slice();

        // Sweep one: the files we know about. Catches edits and deletions.
        let per_chunk = parallel_chunks(files, |chunk| {
            let mut modified = Vec::new();
            let mut removed = Vec::new();
            let mut unchanged = 0usize;

            for entry in chunk {
                let path = PathBuf::from(entry.path.as_ref());
                let Ok(metadata) = std::fs::metadata(&path) else {
                    removed.push((UriId::new(entry.uri_id.to_native()), path));
                    continue;
                };

                let stale = match verification {
                    Verification::Metadata => {
                        metadata.len() != entry.size.to_native() || mtime_ns(&metadata) != entry.mtime_ns.to_native()
                    }
                    Verification::Content => std::fs::read(&path).map_or(true, |bytes| {
                        xxhash_rust::xxh3::xxh3_64(&bytes) != entry.content_hash.to_native()
                    }),
                };

                if stale {
                    modified.push(path);
                } else {
                    unchanged += 1;
                }
            }

            (modified, removed, unchanged)
        });

        let mut changes = Changes::default();
        for (modified, removed, unchanged) in per_chunk {
            changes.modified.extend(modified);
            changes.removed.extend(removed);
            changes.unchanged += unchanged;
        }

        // Sweep two: the directories we know about. Catches insertions.
        let dirs = self.dirs.as_slice();
        let dirty: Vec<PathBuf> = parallel_chunks(dirs, |chunk| {
            chunk
                .iter()
                .filter_map(|entry| {
                    let path = PathBuf::from(entry.path.as_ref());
                    match std::fs::metadata(&path) {
                        Ok(metadata) if mtime_ns(&metadata) == entry.mtime_ns.to_native() => None,
                        // A directory that vanished took its files with it, and sweep one already
                        // reported those, so there is nothing new to find inside it.
                        Err(_) => None,
                        Ok(_) => Some(path),
                    }
                })
                .collect::<Vec<_>>()
        })
        .into_iter()
        .flatten()
        .collect();

        if !dirty.is_empty() {
            let known_files: HashSet<&str> = files.iter().map(|entry| entry.path.as_ref()).collect();
            let known_dirs: HashSet<&str> = dirs.iter().map(|entry| entry.path.as_ref()).collect();
            // Discovery has to obey the same exclusions the original listing did, or a reload
            // would pull in files the workspace deliberately skips.
            let excluded: Vec<Pattern> = self
                .excluded
                .iter()
                .filter_map(|entry| Pattern::new(entry).ok())
                .collect();
            changes.added = collect_additions(&dirty, &known_files, &known_dirs, &excluded);
        }

        changes
    }
}

/// Reads the directories whose mtime moved and reports indexable files the manifest never saw.
///
/// A directory that is itself unknown is brand new, so it is walked in full: everything inside it
/// is an addition.
fn collect_additions(
    dirty: &[PathBuf],
    known_files: &HashSet<&str>,
    known_dirs: &HashSet<&str>,
    excluded: &[Pattern],
) -> Vec<PathBuf> {
    let is_excluded = |path: &Path| excluded.iter().any(|pattern| pattern.matches_path(path));

    let mut added = Vec::new();
    let mut pending: Vec<PathBuf> = dirty.to_vec();

    while let Some(directory) = pending.pop() {
        let Ok(entries) = std::fs::read_dir(&directory) else {
            continue;
        };

        for entry in entries.flatten() {
            let path = entry.path();
            let Ok(file_type) = entry.file_type() else { continue };
            if is_excluded(&path) {
                continue;
            }

            if file_type.is_dir() {
                // An unknown directory cannot have been stat-ed in sweep two, so descend into it.
                if !known_dirs.contains(path.to_string_lossy().as_ref()) {
                    pending.push(path);
                }
            } else if crate::listing::is_indexable_file(&path) && !known_files.contains(path.to_string_lossy().as_ref())
            {
                added.push(path);
            }
        }
    }

    added.sort_unstable();
    added.dedup();
    added
}
