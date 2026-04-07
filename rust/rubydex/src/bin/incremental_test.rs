use std::collections::HashSet;
use std::io::Write as _;
use std::panic;
use std::time::Instant;

use clap::Parser;

use rubydex::{
    indexing::{self, LanguageId},
    integrity, listing,
    model::graph::{Graph, TraceEvent},
    resolution::Resolver,
};

#[derive(Parser, Debug)]
#[command(
    name = "incremental_test",
    about = "Test that incremental resolution matches fresh resolution"
)]
struct Args {
    /// Path to a Ruby project or directory of gems
    #[arg(value_name = "PATH")]
    path: String,

    /// RNG seed for deterministic file selection
    #[arg(long, default_value = "42")]
    seed: u64,

    /// Number of delete/re-add cycles
    #[arg(long, default_value = "5")]
    cycles: usize,

    /// Bisect a failing round to find the minimum reproduction (deleted + indexed files)
    #[arg(long)]
    bisect: bool,

    /// File listing URIs to skip during indexing (one per line, grown by --bisect)
    #[arg(long, value_name = "FILE")]
    skip_file: Option<String>,

    /// Number of retries per bisect step (for non-deterministic failures)
    #[arg(long, default_value = "1")]
    retries: usize,

    /// Trace creation/removal of declarations whose name contains PATTERN
    #[arg(long, value_name = "PATTERN")]
    trace_declaration: Option<String>,
}

/// Delete percentage (hardcoded for now)
const DELETE_PCT: usize = 10;
/// Maximum files to delete per cycle
const MAX_DELETE: usize = 30;

struct GraphCounts {
    declarations: usize,
    definitions: usize,
    names: usize,
    documents: usize,
    constant_references: usize,
    method_references: usize,
}

impl GraphCounts {
    fn from_graph(graph: &Graph) -> Self {
        Self {
            declarations: graph.declarations().len(),
            definitions: graph.definitions().len(),
            names: graph.names().len(),
            documents: graph.documents().len(),
            constant_references: graph.constant_references().len(),
            method_references: graph.method_references().len(),
        }
    }

    fn compare(&self, other: &GraphCounts) -> Vec<String> {
        let checks: &[(&str, usize, usize)] = &[
            ("declarations", self.declarations, other.declarations),
            ("definitions", self.definitions, other.definitions),
            ("names", self.names, other.names),
            ("documents", self.documents, other.documents),
            (
                "constant_references",
                self.constant_references,
                other.constant_references,
            ),
            ("method_references", self.method_references, other.method_references),
        ];

        let mut mismatches = Vec::new();
        for &(name, incremental, fresh) in checks {
            if incremental != fresh {
                let diff = incremental.cast_signed() - fresh.cast_signed();
                mismatches.push(format!(
                    "{name}: incremental={incremental}, fresh={fresh} (diff={diff:+})",
                ));
            }
        }
        mismatches
    }
}

impl std::fmt::Display for GraphCounts {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} declarations, {} definitions, {} names, {} documents, {} constant_refs, {} method_refs",
            self.declarations,
            self.definitions,
            self.names,
            self.documents,
            self.constant_references,
            self.method_references,
        )
    }
}

fn resolve(graph: &mut Graph) {
    Resolver::new(graph).resolve();
}

/// Deterministic file selection using xxhash. Each file is hashed with the seed,
/// then files are sorted by hash and the first N are selected.
fn select_files_to_delete(file_count: usize, seed: u64) -> Vec<usize> {
    let n = ((file_count * DELETE_PCT) / 100).clamp(1, MAX_DELETE);
    let mut scored: Vec<(u64, usize)> = (0..file_count)
        .map(|i| {
            let hash = xxhash_rust::xxh3::xxh3_64_with_seed(&i.to_le_bytes(), seed);
            (hash, i)
        })
        .collect();
    scored.sort_by_key(|(hash, _)| *hash);
    let mut indices: Vec<usize> = scored.into_iter().take(n).map(|(_, i)| i).collect();
    indices.sort_unstable();
    indices
}

/// Check for dangling definition IDs — definitions referenced by declarations but not in the graph.
fn check_dangling_definitions(graph: &Graph) -> Vec<String> {
    let mut dangles = Vec::new();
    for decl in graph.declarations().values() {
        for def_id in decl.definitions() {
            if !graph.definitions().contains_key(def_id) {
                dangles.push(format!(
                    "Declaration `{}` (kind={}) references missing def {def_id:?}",
                    decl.name(),
                    decl.kind(),
                ));
            }
        }
    }
    dangles
}

/// Check which definitions in a document don't map to a declaration.
fn check_unmapped_definitions(graph: &Graph, uri: &str) -> Vec<String> {
    let uri_id = rubydex::model::ids::UriId::from(uri);
    let Some(document) = graph.documents().get(&uri_id) else {
        return vec![format!("Document {uri} not found")];
    };

    let mut unmapped = Vec::new();
    for def_id in document.definitions() {
        let decl_id = graph.definition_id_to_declaration_id(*def_id);
        if decl_id.is_none() {
            let def = graph.definitions().get(def_id).unwrap();
            unmapped.push(format!("  {def_id:?} ({}) → no declaration", def.kind()));
        }
    }
    unmapped
}

fn set_trace_for_phase(graph: &mut Graph, pattern: &str, phase: &str) {
    let pat = pattern.to_string();
    let phase = phase.to_string();
    graph.set_trace(move |event| {
        let name = match &event {
            TraceEvent::Created { name, .. } | TraceEvent::Removed { name, .. } => name,
        };
        if name.contains(pat.as_str()) {
            eprintln!("[TRACE {phase}] {event}");
        }
    });
}

/// Result of comparing incremental vs fresh graphs.
struct RoundResult {
    counts: GraphCounts,
    integrity_errors: Vec<rubydex::integrity::IntegrityError>,
    extras: Vec<String>,
    missing: Vec<String>,
}

/// Run a single round: index → resolve → delete → resolve → re-add → resolve.
/// When `indexed_indices` is Some, only those files are indexed (both incremental and fresh).
/// When None, all files are indexed.
fn run_round(
    files: &[(String, String, LanguageId)],
    delete_indices: &[usize],
    indexed_indices: Option<&[usize]>,
    verbose: bool,
    trace_pattern: Option<&str>,
) -> RoundResult {
    let all_indices: Vec<usize> = (0..files.len()).collect();
    let active_indices = indexed_indices.unwrap_or(&all_indices);

    let mut incremental = Graph::new();
    for &i in active_indices {
        let (ref uri, ref source, ref lang) = files[i];
        indexing::index_source(&mut incremental, uri, source, lang);
    }
    if let Some(pat) = trace_pattern {
        set_trace_for_phase(&mut incremental, pat, "initial");
    }
    resolve(&mut incremental);

    if verbose {
        for &i in delete_indices {
            let unmapped = check_unmapped_definitions(&incremental, &files[i].0);
            if !unmapped.is_empty() {
                println!("  Unmapped definitions in {} ({}):", files[i].0, unmapped.len());
                for u in unmapped.iter().take(5) {
                    println!("    {u}");
                }
                if unmapped.len() > 5 {
                    println!("    ... and {} more", unmapped.len() - 5);
                }
            }
        }
    }

    for &i in delete_indices {
        incremental.delete_document(&files[i].0);
    }

    if verbose {
        let dangles = check_dangling_definitions(&incremental);
        if !dangles.is_empty() {
            println!("  Dangling definitions after delete ({}):", dangles.len());
            for d in dangles.iter().take(10) {
                println!("    {d}");
            }
        }
    }

    if verbose {
        println!("  Resolving after delete...");
    }
    if let Some(pat) = trace_pattern {
        set_trace_for_phase(&mut incremental, pat, "post-delete");
    }
    resolve(&mut incremental);

    for &i in delete_indices {
        indexing::index_source(&mut incremental, &files[i].0, &files[i].1, &files[i].2);
    }

    if verbose {
        println!("  Resolving after re-index...");
    }
    if let Some(pat) = trace_pattern {
        set_trace_for_phase(&mut incremental, pat, "post-re-add");
    }
    resolve(&mut incremental);

    let counts = GraphCounts::from_graph(&incremental);
    let integrity_errors = integrity::check_integrity(&incremental);

    let mut fresh = Graph::new();
    for &i in active_indices {
        let (ref uri, ref source, ref lang) = files[i];
        indexing::index_source(&mut fresh, uri, source, lang);
    }
    if let Some(pat) = trace_pattern {
        set_trace_for_phase(&mut fresh, pat, "fresh");
    }
    resolve(&mut fresh);

    let mut missing: Vec<String> = Vec::new();
    for (decl_id, decl) in fresh.declarations() {
        if !incremental.declarations().contains_key(decl_id) {
            let owner_name = fresh.declarations().get(decl.owner_id()).map(|d| d.name().to_string());
            let owner_exists_in_inc = incremental.declarations().contains_key(decl.owner_id());
            let owner_has_singleton_in_inc = if owner_exists_in_inc {
                incremental
                    .declarations()
                    .get(decl.owner_id())
                    .and_then(|d| d.as_namespace())
                    .and_then(|ns| ns.singleton_class())
                    .is_some()
            } else {
                false
            };
            missing.push(format!(
                "{} ({}) owner={} in_inc={owner_exists_in_inc} has_singleton={owner_has_singleton_in_inc}",
                decl.name(),
                decl.kind(),
                owner_name.as_deref().unwrap_or("?"),
            ));
        }
    }
    missing.sort();

    let mut extras: Vec<String> = Vec::new();
    for (decl_id, decl) in incremental.declarations() {
        if !fresh.declarations().contains_key(decl_id) {
            extras.push(format!("{} ({})", decl.name(), decl.kind()));
        }
    }
    extras.sort();

    RoundResult {
        counts,
        integrity_errors,
        extras,
        missing,
    }
}

/// Returns true if the given configuration causes a mismatch or panic.
/// Runs up to `retries` times — returns true if ANY attempt fails.
fn round_fails(
    files: &[(String, String, LanguageId)],
    delete_indices: &[usize],
    indexed_indices: Option<&[usize]>,
    retries: usize,
) -> bool {
    for _ in 0..retries {
        let result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
            run_round(files, delete_indices, indexed_indices, false, None)
        }));
        let failed = match result {
            Ok(r) => !r.extras.is_empty() || !r.missing.is_empty() || !r.integrity_errors.is_empty(),
            Err(_) => true,
        };
        if failed {
            return true;
        }
    }
    false
}

// --- Skip file I/O ---

fn read_skip_file(path: &str) -> HashSet<String> {
    let Ok(content) = std::fs::read_to_string(path) else {
        return HashSet::new();
    };
    content.lines().filter(|l| !l.is_empty()).map(String::from).collect()
}

fn write_skip_file(path: &str, skipped_uris: &HashSet<String>) {
    let mut sorted: Vec<&str> = skipped_uris.iter().map(String::as_str).collect();
    sorted.sort_unstable();
    let mut file = std::fs::File::create(path).expect("failed to write skip file");
    for uri in sorted {
        writeln!(file, "{uri}").expect("failed to write skip file");
    }
}

/// Compute indices of files NOT in the skip set.
fn compute_indexed_indices(files: &[(String, String, LanguageId)], skipped: &HashSet<String>) -> Vec<usize> {
    (0..files.len()).filter(|&i| !skipped.contains(&files[i].0)).collect()
}

// --- Delta debugging ---

/// Delta-debugging: find the minimum subset of `initial` that still triggers `fails`.
/// Elements in `protected` are never removed. Uses chunked removal (halves → quarters → ...)
/// before falling back to one-at-a-time for efficiency with large sets.
/// Calls `on_progress` after each successful removal for persistence.
fn minimize_set<F, P>(
    initial: &[usize],
    protected: &HashSet<usize>,
    label: &str,
    files: &[(String, String, LanguageId)],
    mut fails: F,
    mut on_progress: P,
) -> Vec<usize>
where
    F: FnMut(&[usize]) -> bool,
    P: FnMut(&[usize]),
{
    let mut current = initial.to_vec();
    let removable_count = current.iter().filter(|i| !protected.contains(i)).count();

    println!(
        "\nMinimizing {removable_count} {label} ({} protected)...\n",
        protected.len()
    );

    if removable_count == 0 {
        return current;
    }

    // Chunked removal: try removing halves, then quarters, etc.
    let mut chunk_size = removable_count / 2;
    while chunk_size >= 1 {
        let mut changed_this_pass = true;
        while changed_this_pass {
            changed_this_pass = false;
            let removable: Vec<usize> = current.iter().copied().filter(|i| !protected.contains(i)).collect();

            let mut offset = 0;
            while offset < removable.len() {
                let chunk_end = (offset + chunk_size).min(removable.len());
                let to_remove: HashSet<usize> = removable[offset..chunk_end].iter().copied().collect();
                let candidate: Vec<usize> = current.iter().copied().filter(|i| !to_remove.contains(i)).collect();

                let remaining = candidate.iter().filter(|i| !protected.contains(i)).count();
                print!(
                    "  Removing chunk of {} {label} ({remaining} remain)... ",
                    to_remove.len(),
                );

                if fails(&candidate) {
                    println!("still fails");
                    current = candidate;
                    changed_this_pass = true;
                    on_progress(&current);
                    break; // recompute removable with updated current
                }
                println!("passes — keeping");
                offset += chunk_size;
            }
        }
        chunk_size /= 2;
    }

    // One-at-a-time pass for stragglers
    let mut changed = true;
    while changed {
        changed = false;
        let removable: Vec<usize> = current.iter().copied().filter(|i| !protected.contains(i)).collect();

        for &idx in &removable {
            let candidate: Vec<usize> = current.iter().copied().filter(|&i| i != idx).collect();

            let remaining = candidate.iter().filter(|i| !protected.contains(i)).count();
            print!("  Without {} ({remaining} remain)... ", files[idx].0,);

            if fails(&candidate) {
                println!("still fails — removing");
                current = candidate;
                changed = true;
                on_progress(&current);
                break; // restart scan since indices shifted
            }
            println!("passes — keeping");
        }
    }

    current
}

/// Find minimum set of deleted files that still triggers the failure.
fn bisect_deleted_files(
    files: &[(String, String, LanguageId)],
    delete_indices: &[usize],
    retries: usize,
) -> Vec<usize> {
    minimize_set(
        delete_indices,
        &HashSet::new(),
        "deleted files",
        files,
        |candidate| round_fails(files, candidate, None, retries),
        |_| {},
    )
}

/// Run both bisect phases and print a complete minimal reproduction.
/// Phase 2 writes to `skip_file` after each successful removal for resumability.
fn bisect_and_report(
    files: &[(String, String, LanguageId)],
    delete_indices: &[usize],
    skip_file: &str,
    retries: usize,
    fresh_counts: &GraphCounts,
) {
    // Phase 1: minimize deleted files
    let min_deleted = bisect_deleted_files(files, delete_indices, retries);
    println!(
        "\nPhase 1 complete: {} → {} deleted files",
        delete_indices.len(),
        min_deleted.len()
    );
    for &i in &min_deleted {
        println!("  {}", files[i].0);
    }

    // Phase 2: minimize indexed files by growing the skip file
    let existing_skipped = read_skip_file(skip_file);
    let mut indexed = compute_indexed_indices(files, &existing_skipped);

    // Verify the failure still reproduces with current skip list
    if !round_fails(files, &min_deleted, Some(&indexed), retries) {
        println!("\nRound passes with current skip list — cannot bisect indexed files.");
        println!("Try clearing {skip_file} and re-running.");
        return;
    }

    let protected: HashSet<usize> = min_deleted.iter().copied().collect();

    indexed = minimize_set(
        &indexed,
        &protected,
        "indexed files",
        files,
        |candidate| round_fails(files, &min_deleted, Some(candidate), retries),
        |current_indexed| {
            // Write skip file: all files NOT in current_indexed
            let indexed_set: HashSet<usize> = current_indexed.iter().copied().collect();
            let skipped: HashSet<String> = (0..files.len())
                .filter(|i| !indexed_set.contains(i))
                .map(|i| files[i].0.clone())
                .collect();
            write_skip_file(skip_file, &skipped);
        },
    );

    let non_deleted_count = indexed.iter().filter(|i| !min_deleted.contains(i)).count();
    println!(
        "\nPhase 2 complete: {} → {} indexed files ({non_deleted_count} surviving + {} deleted)",
        files.len(),
        indexed.len(),
        min_deleted.len()
    );

    // Write final skip file
    let indexed_set: HashSet<usize> = indexed.iter().copied().collect();
    let final_skipped: HashSet<String> = (0..files.len())
        .filter(|i| !indexed_set.contains(i))
        .map(|i| files[i].0.clone())
        .collect();
    write_skip_file(skip_file, &final_skipped);
    println!("Skip file written to {skip_file}");

    // Final verification
    println!("\nFinal verification:");
    let final_result = run_round(files, &min_deleted, Some(&indexed), true, None);
    print_round_result(&final_result, fresh_counts);

    // Print the minimal reproduction
    let surviving: Vec<usize> = indexed.iter().copied().filter(|i| !min_deleted.contains(i)).collect();

    println!("\n========================================");
    println!("Minimal reproduction ({} files total):", indexed.len());
    println!("========================================\n");

    println!("Index {} file(s):", surviving.len());
    for &i in &surviving {
        println!("  {}", files[i].0);
    }
    println!("\nDelete and re-add {} file(s):", min_deleted.len());
    for &i in &min_deleted {
        println!("  {}", files[i].0);
    }

    println!("\nFile contents:");
    for &i in &indexed {
        let role = if min_deleted.contains(&i) {
            "DELETE+RE-ADD"
        } else {
            "INDEX"
        };
        println!("\n--- [{role}] {} ---", files[i].0);
        for line in files[i].1.lines().take(50) {
            println!("  {line}");
        }
        let total_lines = files[i].1.lines().count();
        if total_lines > 50 {
            println!("  ... ({total_lines} lines total)");
        }
    }
}

fn print_round_result(result: &RoundResult, fresh_counts: &GraphCounts) {
    if !result.missing.is_empty() {
        println!("  Missing declarations ({}):", result.missing.len());
        for m in result.missing.iter().take(30) {
            println!("    {m}");
        }
    }
    if !result.extras.is_empty() {
        println!("  Extra declarations ({}):", result.extras.len());
        for e in result.extras.iter().take(10) {
            println!("    {e}");
        }
    }

    let mismatches = result.counts.compare(fresh_counts);
    if !mismatches.is_empty() {
        println!("  Count mismatches:");
        for m in &mismatches {
            println!("    {m}");
        }
        println!("  Incremental: {}", result.counts);
        println!("  Fresh:       {fresh_counts}");
    }
    if !result.integrity_errors.is_empty() {
        println!("  Integrity errors ({}):", result.integrity_errors.len());
        for error in result.integrity_errors.iter().take(20) {
            println!("    - {error}");
        }
    }
}

#[allow(clippy::too_many_lines)]
fn main() {
    let args = Args::parse();

    println!("Listing files...");
    let (file_paths, errors) = listing::collect_file_paths(vec![args.path], &HashSet::new());

    for error in &errors {
        eprintln!("{error}");
    }

    if file_paths.is_empty() {
        eprintln!("No files found");
        std::process::exit(1);
    }

    let mut file_paths = file_paths;
    file_paths.sort();
    // Deterministic shuffle: use seed to permute file order so that different seeds
    // explore different IdentityHashMap insertion orders (which affect resolution ordering).
    let mut scored: Vec<(u64, _)> = file_paths
        .into_iter()
        .enumerate()
        .map(|(i, p)| {
            let hash = xxhash_rust::xxh3::xxh3_64_with_seed(&i.to_le_bytes(), args.seed);
            (hash, p)
        })
        .collect();
    scored.sort_by_key(|(hash, _)| *hash);
    let file_paths: Vec<_> = scored.into_iter().map(|(_, p)| p).collect();

    let files: Vec<(String, String, LanguageId)> = file_paths
        .into_iter()
        .filter_map(|path| {
            let content = std::fs::read_to_string(&path).ok()?;
            let uri = url::Url::from_file_path(&path).ok()?.to_string();
            let ext = path.extension().unwrap_or_default();
            let language_id = LanguageId::from(ext);
            Some((uri, content, language_id))
        })
        .collect();

    println!("Found {} files", files.len());

    // Apply skip file to determine which files are indexed
    let skipped = args.skip_file.as_deref().map_or_else(HashSet::new, read_skip_file);
    let indexed_indices = if skipped.is_empty() {
        None
    } else {
        let indices = compute_indexed_indices(&files, &skipped);
        println!("Skipping {} files ({} indexed)", skipped.len(), indices.len());
        Some(indices)
    };
    println!();

    // Build fresh baseline (respecting skip list)
    println!("Building fresh baseline...");
    let fresh_start = Instant::now();
    let mut fresh = Graph::new();
    let baseline_indices: Vec<usize> = indexed_indices.clone().unwrap_or_else(|| (0..files.len()).collect());
    for &i in &baseline_indices {
        let (ref uri, ref source, ref lang) = files[i];
        indexing::index_source(&mut fresh, uri, source, lang);
    }
    resolve(&mut fresh);
    let fresh_counts = GraphCounts::from_graph(&fresh);
    println!(
        "Fresh baseline ({:.2}s): {fresh_counts}\n",
        fresh_start.elapsed().as_secs_f64()
    );

    let fresh_integrity_errors = integrity::check_integrity(&fresh);
    if !fresh_integrity_errors.is_empty() {
        eprintln!(
            "WARNING: Fresh graph has {} integrity errors",
            fresh_integrity_errors.len()
        );
        for error in &fresh_integrity_errors {
            eprintln!("  - {error}");
        }
    }

    drop(fresh);

    // Select files to delete from the INDEXED set (not all files)
    let active_count = baseline_indices.len();

    let mut failed_rounds: Vec<usize> = Vec::new();

    for round in 0..args.cycles {
        let round_seed = args.seed.wrapping_add(round as u64);
        let selected = select_files_to_delete(active_count, round_seed);
        // Map back to global indices
        let delete_indices: Vec<usize> = selected.iter().map(|&i| baseline_indices[i]).collect();

        println!("Round {}", round + 1);
        println!("  Removing {} files:", delete_indices.len());
        for &i in &delete_indices {
            println!("    {}", files[i].0);
        }

        let round_start = Instant::now();

        let idx_ref = indexed_indices.as_deref();
        let trace_pat = args.trace_declaration.as_deref();
        let round_result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
            run_round(&files, &delete_indices, idx_ref, true, trace_pat)
        }));

        let elapsed = round_start.elapsed().as_secs_f64();

        match round_result {
            Ok(result) => {
                let mismatches = result.counts.compare(&fresh_counts);

                if mismatches.is_empty() && result.integrity_errors.is_empty() {
                    println!("  OK ({elapsed:.2}s): {}", result.counts);
                } else {
                    print_round_result(&result, &fresh_counts);
                    println!("  FAILED ({elapsed:.2}s)");
                    failed_rounds.push(round + 1);

                    if args.bisect {
                        let skip_path = args.skip_file.as_deref().unwrap_or("/tmp/incremental_test_skip");
                        if args.skip_file.is_none() {
                            println!("\nNo --skip-file specified, using {skip_path}");
                        }
                        bisect_and_report(&files, &delete_indices, skip_path, args.retries, &fresh_counts);
                        break;
                    }
                }
            }
            Err(panic_info) => {
                let msg = if let Some(s) = panic_info.downcast_ref::<String>() {
                    s.clone()
                } else if let Some(s) = panic_info.downcast_ref::<&str>() {
                    (*s).to_string()
                } else {
                    "unknown panic".to_string()
                };
                println!("  PANICKED ({elapsed:.2}s): {msg}");
                failed_rounds.push(round + 1);

                if args.bisect {
                    let skip_path = args.skip_file.as_deref().unwrap_or("/tmp/incremental_test_skip");
                    if args.skip_file.is_none() {
                        println!("\nNo --skip-file specified, using {skip_path}");
                    }
                    bisect_and_report(&files, &delete_indices, skip_path, args.retries, &fresh_counts);
                    break;
                }
            }
        }
        println!();
    }

    if failed_rounds.is_empty() {
        println!("All {} rounds passed (seed={}).", args.cycles, args.seed);
    } else {
        eprintln!(
            "FAILED: {} of {} rounds failed (rounds: {failed_rounds:?}, seed={})",
            failed_rounds.len(),
            args.cycles,
            args.seed,
        );
        std::process::exit(1);
    }
}
