use clap::{Parser, ValueEnum};
use std::{fs, mem, path::PathBuf};

use rubydex::{
    complexity, dot,
    indexing::{self, IndexerBackend},
    integrity, listing,
    model::graph::Graph,
    resolution::Resolver,
    stats::{
        memory::MemoryStats,
        timer::{Timer, time_it},
    },
};

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

    #[arg(long = "complexity", help = "Compute a complexity report instead of indexing")]
    complexity: bool,

    #[arg(
        long = "complexity-format",
        value_enum,
        default_value = "text",
        requires = "complexity",
        help = "Output format for the complexity report (text or json)"
    )]
    complexity_format: ComplexityFormat,

    #[arg(
        long = "complexity-top",
        default_value_t = 25,
        requires = "complexity",
        help = "Max entries in text output (0 = all)"
    )]
    complexity_top: usize,

    #[arg(
        long = "complexity-methods-only",
        requires = "complexity",
        help = "Skip code outside methods"
    )]
    complexity_methods_only: bool,

    #[arg(
        long = "complexity-details",
        requires = "complexity",
        help = "Show per-construct score breakdown under each method"
    )]
    complexity_details: bool,

    #[arg(
        long = "complexity-group",
        requires = "complexity",
        help = "Group and sort entries by class with subtotals"
    )]
    complexity_group: bool,

    #[arg(
        long = "complexity-diff",
        value_name = "FILE",
        requires = "complexity",
        help = "Baseline report JSON to diff against"
    )]
    complexity_diff: Option<PathBuf>,
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

#[derive(Debug, Clone, ValueEnum)]
enum ComplexityFormat {
    Text,
    Json,
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

/// Run the complexity analysis pass and print the report (or a diff against a baseline), then exit.
fn run_complexity(args: &Args) {
    if args.complexity_diff.is_some() && args.complexity_details {
        eprintln!("`--complexity-details` does not apply to diff output; remove it or drop `--complexity-diff`");
        std::process::exit(1);
    }
    if args.complexity_diff.is_some() && args.complexity_group {
        eprintln!("`--complexity-group` does not apply to diff output; remove it or drop `--complexity-diff`");
        std::process::exit(1);
    }
    if args.complexity_group && matches!(args.complexity_format, ComplexityFormat::Json) {
        eprintln!(
            "`--complexity-group` only affects text output; use `--complexity-format text` or drop `--complexity-group`"
        );
        std::process::exit(1);
    }
    let (report, errors) = match complexity::analyze(
        args.paths.clone(),
        args.complexity_methods_only,
        args.complexity_details && args.complexity_diff.is_none(),
    ) {
        Ok(result) => result,
        Err(error) => {
            eprintln!("{error}");
            std::process::exit(1);
        }
    };

    for error in errors {
        eprintln!("{error}");
    }

    let output = match &args.complexity_diff {
        Some(baseline_path) => {
            let baseline_json = match fs::read_to_string(baseline_path) {
                Ok(content) => content,
                Err(error) => {
                    eprintln!("Failed to read baseline report `{}`: {error}", baseline_path.display());
                    std::process::exit(1);
                }
            };
            let baseline = match complexity::Report::from_json(&baseline_json) {
                Ok(report) => report,
                Err(error) => {
                    eprintln!("Failed to parse baseline report: {error}");
                    std::process::exit(1);
                }
            };
            let diff = match complexity::Report::diff(&baseline, &report) {
                Ok(diff) => diff,
                Err(error) => {
                    eprintln!("{error}");
                    std::process::exit(1);
                }
            };
            match args.complexity_format {
                ComplexityFormat::Text => diff.render_text(args.complexity_top),
                ComplexityFormat::Json => diff.to_json(),
            }
        }
        None => match args.complexity_format {
            ComplexityFormat::Text => {
                report.render_text(args.complexity_top, args.complexity_details, args.complexity_group)
            }
            ComplexityFormat::Json => report.to_json(),
        },
    };

    print!("{output}");
}

fn main() {
    let args = Args::parse();

    if args.complexity {
        run_complexity(&args);
        return;
    }

    if args.stats {
        Timer::set_global_timer(Timer::new());
    }

    let mut graph = Graph::new();

    if let Some(workspace_path) = listing::workspace_path_for(&args.paths) {
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
