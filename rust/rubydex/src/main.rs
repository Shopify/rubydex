use clap::{Parser, ValueEnum};
use std::{collections::HashSet, mem};

use rubydex::{
    dot,
    indexing::{self, IndexerBackend},
    integrity, listing,
    model::graph::Graph,
    query::cypher::{self, OutputFormat},
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
    #[arg(value_name = "PATHS", default_value = ".")]
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

    #[arg(long = "query", value_name = "CYPHER", help = "Run a Cypher query against the graph")]
    query: Option<String>,

    #[arg(
        long = "schema",
        help = "Describe the queryable Cypher schema (labels, relationships, properties) and exit"
    )]
    schema: bool,

    #[arg(
        long = "format",
        value_enum,
        default_value = "table",
        help = "Output format for --query and --schema results"
    )]
    format: Format,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum Format {
    Table,
    Json,
}

impl From<Format> for OutputFormat {
    fn from(format: Format) -> Self {
        match format {
            Format::Table => OutputFormat::Table,
            Format::Json => OutputFormat::Json,
        }
    }
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

fn main() {
    let args = Args::parse();

    // The Cypher schema is static, so describe it without indexing the workspace.
    if args.schema {
        print!("{}", cypher::schema(args.format.into()));
        std::process::exit(0);
    }

    // Parse the query up front, before any indexing, so a malformed query fails fast.
    let parsed_query = args.query.as_ref().map(|query| match cypher::parse(query) {
        Ok(parsed) => parsed,
        Err(error) => {
            eprintln!("{error}");
            std::process::exit(1);
        }
    });

    if args.stats {
        Timer::set_global_timer(Timer::new());
    }

    // Listing

    let (file_paths, errors) = time_it!(listing, { listing::collect_file_paths(args.paths, &HashSet::new()) });

    for error in errors {
        eprintln!("{error}");
    }

    if let Some(StopAfter::Listing) = args.stop_after {
        return exit(args.stats);
    }

    // Indexing

    let mut graph = Graph::new();
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

    // Cypher query: execute the query parsed earlier against the now-built graph.
    if let Some(query) = &parsed_query {
        match time_it!(querying, { cypher::run_parsed(&graph, query, args.format.into()) }) {
            Ok(output) => print!("{output}"),
            Err(error) => {
                eprintln!("{error}");
                std::process::exit(1);
            }
        }

        if args.stats {
            Timer::print_breakdown();
            MemoryStats::print_memory_usage();
        }

        mem::forget(graph);
        return;
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
