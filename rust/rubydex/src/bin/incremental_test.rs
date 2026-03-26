use std::panic;
use std::time::Instant;

use clap::Parser;

use rubydex::{
    indexing::{self, LanguageId},
    integrity, listing,
    model::graph::Graph,
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
            if graph.definitions().get(def_id).is_none() {
                dangles.push(format!("Declaration `{}` references missing definition", decl.name()));
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

fn run_round(
    files: &[(String, String, LanguageId)],
    delete_indices: &[usize],
) -> (GraphCounts, Vec<rubydex::integrity::IntegrityError>) {
    // Build incremental graph: index all → resolve → delete → resolve → re-add → resolve
    let mut incremental = Graph::new();
    for (uri, source, lang) in files {
        indexing::index_source(&mut incremental, uri, source, lang);
    }
    resolve(&mut incremental);

    // Check which definitions in to-be-deleted files don't map to declarations
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

    // Delete selected files
    for &i in delete_indices {
        incremental.delete_document(&files[i].0);
    }

    // Check for dangling definitions before resolving
    let dangles = check_dangling_definitions(&incremental);
    if !dangles.is_empty() {
        println!("  Dangling definitions after delete ({}):", dangles.len());
        for d in dangles.iter().take(10) {
            println!("    {d}");
        }
        if dangles.len() > 10 {
            println!("    ... and {} more", dangles.len() - 10);
        }
    }

    println!("  Resolving after delete...");
    resolve(&mut incremental);

    // Re-add deleted files
    for &i in delete_indices {
        indexing::index_source(&mut incremental, &files[i].0, &files[i].1, &files[i].2);
    }

    println!("  Resolving after re-index...");
    resolve(&mut incremental);

    let counts = GraphCounts::from_graph(&incremental);
    let integrity_errors = integrity::check_integrity(&incremental);
    (counts, integrity_errors)
}

#[allow(clippy::too_many_lines)]
fn main() {
    let args = Args::parse();

    // Collect file paths using the existing listing infrastructure
    println!("Listing files...");
    let (file_paths, errors) = listing::collect_file_paths(vec![args.path]);

    for error in &errors {
        eprintln!("{error}");
    }

    if file_paths.is_empty() {
        eprintln!("No files found");
        std::process::exit(1);
    }

    // Read all file contents upfront so we can re-index deleted files
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

    println!("Found {} files\n", files.len());

    // Build fresh baseline
    println!("Building fresh baseline...");
    let fresh_start = Instant::now();
    let mut fresh = Graph::new();
    for (uri, source, lang) in &files {
        indexing::index_source(&mut fresh, uri, source, lang);
    }
    resolve(&mut fresh);
    let fresh_counts = GraphCounts::from_graph(&fresh);
    println!(
        "Fresh baseline ({:.2}s): {fresh_counts}\n",
        fresh_start.elapsed().as_secs_f64()
    );

    // Check integrity on fresh graph
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

    // We only need the counts from fresh, drop the graph
    drop(fresh);

    let mut failed_rounds: Vec<usize> = Vec::new();

    for round in 0..args.cycles {
        let round_seed = args.seed.wrapping_add(round as u64);
        let delete_indices = select_files_to_delete(files.len(), round_seed);

        println!("Round {}", round + 1);

        // Print files to be deleted before running (useful if we panic)
        println!("  Removing {} files:", delete_indices.len());
        for &i in &delete_indices {
            println!("    {}", files[i].0);
        }

        let round_start = Instant::now();

        // Run the round in a catch_unwind so panics don't kill the whole test
        let round_result = panic::catch_unwind(panic::AssertUnwindSafe(|| run_round(&files, &delete_indices)));

        let elapsed = round_start.elapsed().as_secs_f64();

        match round_result {
            Ok((incremental_counts, integrity_errors)) => {
                let mismatches = incremental_counts.compare(&fresh_counts);

                if mismatches.is_empty() && integrity_errors.is_empty() {
                    println!("  OK ({elapsed:.2}s): {incremental_counts}");
                } else {
                    println!("  FAILED ({elapsed:.2}s):");
                    if !mismatches.is_empty() {
                        println!("  Count mismatches:");
                        for m in &mismatches {
                            println!("    {m}");
                        }
                        println!("  Incremental: {incremental_counts}");
                        println!("  Fresh:       {fresh_counts}");
                    }
                    if !integrity_errors.is_empty() {
                        println!("  Integrity errors ({}):", integrity_errors.len());
                        for error in integrity_errors.iter().take(20) {
                            println!("    - {error}");
                        }
                        if integrity_errors.len() > 20 {
                            println!("    ... and {} more", integrity_errors.len() - 20);
                        }
                    }
                    failed_rounds.push(round + 1);
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
            }
        }
        println!();
    }

    // Summary
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
