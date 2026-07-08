use clap::{Parser, ValueEnum};
use std::{
    fs, mem,
    path::{Path, PathBuf},
    time::{Duration, Instant},
};

use rubydex::{
    dot,
    indexing::{self, IndexerBackend, LanguageId, build_local_graph},
    integrity, listing,
    model::graph::Graph,
    resolution::Resolver,
    stats::{
        memory::MemoryStats,
        timer::{Timer, time_it},
    },
};
use url::Url;

#[derive(Parser, Debug)]
#[command(name = "rubydex_cli", about = "A Static Analysis Toolkit for Ruby", version)]
#[allow(clippy::struct_excessive_bools)]
struct Args {
    #[arg(
        value_name = "PATHS",
        default_value = ".",
        help = "Path(s) to index. If the first path is a directory, it is used as the workspace root for rubydex.toml"
    )]
    paths: Vec<String>,

    #[arg(long = "stop-after", help = "Stop after the given stage")]
    stop_after: Option<StopAfter>,

    #[arg(long = "dot", help = "Output a DOT graph visualization")]
    dot: bool,

    #[arg(long = "show-builtins", help = "Include built-in declarations in DOT output")]
    show_builtins: bool,

    #[arg(long = "stats", help = "Show detailed performance statistics")]
    stats: bool,

    #[arg(long = "check-integrity", help = "Check the integrity of the graph after resolution")]
    check_integrity: bool,

    #[arg(
        long = "indexer",
        value_enum,
        default_value = "ruby-indexer",
        help = "Which indexer backend to use for Ruby files"
    )]
    indexer: Indexer,

    #[arg(
        long = "report-orphans",
        value_name = "PATH",
        num_args = 0..=1,
        require_equals = true,
        default_missing_value = "/tmp/rubydex-orphan-report.txt",
        help = "Write orphan definitions report to specified file"
    )]
    report_orphans: Option<String>,

    #[arg(
        long = "incremental_cycle",
        value_name = "N",
        help = "After the initial build, run N incremental resolution cycles and report their timings"
    )]
    incremental_cycle: Option<usize>,

    #[arg(
        long = "incremental_files",
        value_name = "N",
        default_value_t = 1,
        help = "Number of files to re-index per incremental cycle"
    )]
    incremental_files: usize,
}

#[derive(Debug, Clone, ValueEnum)]
enum StopAfter {
    Listing,
    Indexing,
    Resolution,
}

#[derive(Debug, Clone, ValueEnum)]
enum Indexer {
    RubyIndexer,
    OperationBuilder,
}

impl From<&Indexer> for IndexerBackend {
    fn from(indexer: &Indexer) -> Self {
        match indexer {
            Indexer::RubyIndexer => IndexerBackend::RubyIndexer,
            Indexer::OperationBuilder => IndexerBackend::OperationBuilder,
        }
    }
}

fn exit(print_stats: bool) {
    if print_stats {
        Timer::print_breakdown();
        MemoryStats::print_memory_usage();
    }

    std::process::exit(0);
}

fn workspace_path_for(paths: &[String]) -> Option<PathBuf> {
    let first_path = paths.first()?;
    fs::canonicalize(first_path).ok().filter(|path| path.is_dir())
}

fn main() {
    let args = Args::parse();

    if args.stats {
        Timer::set_global_timer(Timer::new());
    }

    let mut graph = Graph::new();

    if let Some(workspace_path) = workspace_path_for(&args.paths) {
        graph.set_workspace_path(workspace_path);
        if let Err(error) = graph.load_config(None) {
            eprintln!("{error}");
            std::process::exit(1);
        }
    }

    // Listing

    let (file_paths, errors) = time_it!(listing, {
        listing::collect_file_paths(args.paths, &graph.excluded_patterns())
    });

    for error in errors {
        eprintln!("{error}");
    }

    if let Some(StopAfter::Listing) = args.stop_after {
        return exit(args.stats);
    }

    // Indexing

    let backend = IndexerBackend::from(&args.indexer);

    // The incremental benchmark re-indexes files after the initial build, so keep a copy of the
    // paths before `index_files` consumes them.
    let incremental_paths = if args.incremental_cycle.is_some() {
        file_paths.clone()
    } else {
        Vec::new()
    };

    let errors = time_it!(indexing, { indexing::index_files(&mut graph, file_paths, backend) });

    for error in errors {
        eprintln!("{error}");
    }

    if let Some(StopAfter::Indexing) = args.stop_after {
        return exit(args.stats);
    }

    // Resolution

    time_it!(resolution, {
        let mut resolver = Resolver::new(&mut graph);
        resolver.resolve();
    });

    // Incremental resolution benchmark. Runs before the stop-after check so it can be combined with
    // `--stop-after=resolution`.
    if let Some(cycles) = args.incremental_cycle {
        run_incremental_resolution(&mut graph, &incremental_paths, cycles, args.incremental_files, backend);
    }

    if let Some(StopAfter::Resolution) = args.stop_after {
        return exit(args.stats);
    }

    // Integrity check
    if args.check_integrity {
        let errors = time_it!(integrity_check, { integrity::check_integrity(&graph) });

        if errors.is_empty() {
            println!("Integrity check passed: no issues found");
        } else {
            eprintln!("Integrity check found {} issue(s):", errors.len());

            for error in &errors {
                eprintln!("  - {error}");
            }

            std::process::exit(1);
        }
    }

    // Querying

    if args.stats {
        time_it!(querying, {
            graph.print_query_statistics();
        });
    }

    if args.stats {
        Timer::print_breakdown();
        MemoryStats::print_memory_usage();
    }

    // Orphan report
    if let Some(ref path) = args.report_orphans {
        match std::fs::File::create(path) {
            Ok(mut file) => {
                if let Err(e) = graph.write_orphan_report(&mut file) {
                    eprintln!("Failed to write orphan report: {e}");
                } else {
                    println!("Orphan report written to {path}");
                }
            }
            Err(e) => eprintln!("Failed to create orphan report file: {e}"),
        }
    }

    // Generate visualization or print statistics
    if args.dot {
        println!("{}", dot::DotBuilder::generate(&graph, args.show_builtins));
    } else {
        println!("Indexed {} files", graph.documents().len());
        println!("Found {} names", graph.declarations().len());
        println!("Found {} definitions", graph.definitions().len());
        println!("Found {} URIs", graph.documents().len());
    }

    // Forget the graph so we don't have to wait for deallocation and let the system reclaim the memory at exit
    mem::forget(graph);
}

/// Simulates incremental editing to measure incremental resolution cost. For each cycle it
/// re-indexes a rotating window of `files_per_cycle` files (parsing plus the same invalidation the
/// LSP performs on save) and then re-runs resolution over the resulting pending work, reporting
/// per-cycle and aggregate `resolve()` timings.
///
/// With `--stats`, the `compute_descendants` breakdown printed in the timing summary reflects the
/// last incremental cycle, since it is recorded on every `resolve()`.
fn run_incremental_resolution(
    graph: &mut Graph,
    paths: &[PathBuf],
    cycles: usize,
    files_per_cycle: usize,
    backend: IndexerBackend,
) {
    if paths.is_empty() || cycles == 0 || files_per_cycle == 0 {
        eprintln!("Skipping incremental resolution: nothing to re-index");
        return;
    }

    let files_per_cycle = files_per_cycle.min(paths.len());

    let mut reindex_total = Duration::ZERO;
    let mut resolve_total = Duration::ZERO;
    let mut resolve_min = Duration::MAX;
    let mut resolve_max = Duration::ZERO;

    println!();
    println!("Incremental resolution ({cycles} cycle(s), {files_per_cycle} file(s)/cycle)");
    println!("  Scenario: no-op reindex; files are indexed again without changing their contents.");
    println!("  This should be the fastest incremental resolution path.");
    println!("  Add scenario-based benchmarks before optimizing incremental resolution.");

    for cycle in 0..cycles {
        // Re-index the window of files for this cycle. Rotating the window across cycles samples
        // different parts of the codebase rather than measuring the same delta repeatedly.
        let reindex_start = Instant::now();
        for i in 0..files_per_cycle {
            let path = &paths[(cycle * files_per_cycle + i) % paths.len()];
            reindex_file(graph, path, backend);
        }
        reindex_total += reindex_start.elapsed();

        let resolve_start = Instant::now();
        Resolver::new(graph).resolve();
        let elapsed = resolve_start.elapsed();

        resolve_total += elapsed;
        resolve_min = resolve_min.min(elapsed);
        resolve_max = resolve_max.max(elapsed);

        println!(
            "  cycle {:>3}: resolve {:9.3}ms",
            cycle + 1,
            elapsed.as_secs_f64() * 1000.0
        );
    }

    let avg = resolve_total / u32::try_from(cycles).expect("cycle count fits in u32");

    println!(
        "  resolve total {:.3}ms  avg {:.3}ms  min {:.3}ms  max {:.3}ms",
        resolve_total.as_secs_f64() * 1000.0,
        avg.as_secs_f64() * 1000.0,
        resolve_min.as_secs_f64() * 1000.0,
        resolve_max.as_secs_f64() * 1000.0,
    );
    println!(
        "  reindex+invalidate total {:.3}ms (parse + document merge, excluded from resolve above)",
        reindex_total.as_secs_f64() * 1000.0,
    );
}

/// Re-indexes a single file into the graph, running the same invalidation the LSP performs on save.
fn reindex_file(graph: &mut Graph, path: &Path, backend: IndexerBackend) {
    let Ok(source) = fs::read_to_string(path) else {
        eprintln!("Failed to read file `{}`", path.display());
        return;
    };
    let Ok(url) = Url::from_file_path(path) else {
        eprintln!("Couldn't build URI from path `{}`", path.display());
        return;
    };

    let language = path.extension().map_or(LanguageId::Ruby, LanguageId::from);
    let local_graph = build_local_graph(url.to_string(), &source, &language, backend);
    graph.consume_document_changes(local_graph);
}
