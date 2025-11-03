use std::error::Error;

use clap::Parser;

use saturn::{
    errors::MultipleErrors,
    indexing::{self},
    model::graph::Graph,
    stats::{
        memory::MemoryStats,
        timer::{Timer, time_it},
    },
    visualization::dot,
};

#[derive(Parser, Debug)]
#[command(name = "saturn_cli", about = "A Static Analysis Toolkit for Ruby", version)]
#[allow(clippy::struct_excessive_bools)]
struct Args {
    #[arg(value_name = "DIR", default_value = ".")]
    dir: String,

    #[arg(
        long = "check-integrity",
        help = "Run integrity checks on the index after processing"
    )]
    check_integrity: bool,

    #[arg(long = "visualize")]
    visualize: bool,

    #[arg(long = "stats", help = "Show detailed performance statistics")]
    stats: bool,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    if args.stats {
        Timer::set_global_timer(Timer::new());
    }

    let mut graph = Graph::new();
    let (file_paths, errors) = time_it!(listing, { indexing::collect_file_paths(vec![args.dir]) });

    if !errors.is_empty() {
        return Err(Box::new(MultipleErrors(errors)));
    }

    time_it!(indexing, { indexing::index_in_parallel(&mut graph, file_paths) })?;

    // Run integrity checks if requested
    if args.check_integrity {
        time_it!(integrity_check, {
            let errors = Graph::integrity_checker().apply(&graph);

            if errors.is_empty() {
                println!("✓ Index integrity check passed");
            } else {
                eprintln!("✗ Index integrity check failed with {} errors:", errors.len());
                for error in &errors {
                    eprintln!("  - {error}");
                }
                std::process::exit(1);
            }
        });
    }

    if args.stats {
        time_it!(resolving, {
            graph.print_resolution_statistics();
        });
        time_it!(querying, {
            graph.print_query_statistics();
        });
    }

    if args.stats {
        Timer::print_breakdown();
        MemoryStats::print_memory_usage();
    }

    // Generate visualization or print statistics
    if args.visualize {
        println!("{}", dot::generate(&graph));
    } else {
        println!("Indexed {} files", graph.documents().len());
        println!("Found {} names", graph.declarations().len());
        println!("Found {} definitions", graph.definitions().len());
        println!("Found {} URIs", graph.documents().len());
    }

    Ok(())
}
