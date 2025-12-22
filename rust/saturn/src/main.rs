use std::{error::Error, mem};

use clap::Parser;

use saturn::{
    indexing, listing,
    model::graph::Graph,
    resolution,
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

    let (file_paths, errors) = time_it!(listing, { listing::collect_file_paths(vec![args.dir]) });

    for error in errors {
        eprintln!("{error}");
    }

    let mut graph = Graph::new();
    time_it!(indexing, { indexing::index_in_parallel(&mut graph, file_paths) })?;

    time_it!(resolution, {
        resolution::resolve_all(&mut graph);
    });

    // Run integrity checks if requested
    // if args.check_integrity {
    //     time_it!(integrity_check, {
    //         let errors = Graph::integrity_checker().apply(&graph);

    //         if errors.is_empty() {
    //             println!("✓ Index integrity check passed");
    //         } else {
    //             eprintln!("✗ Index integrity check failed with {} errors:", errors.len());
    //             for error in &errors {
    //                 eprintln!("  - {error}");
    //             }
    //             std::process::exit(1);
    //         }
    //     });
    // }

    if args.stats {
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
        let read_lock = graph.declarations().read().unwrap();
        println!("Found {} names", read_lock.len());
        println!("Found {} definitions", graph.definitions().len());
        println!("Found {} URIs", graph.documents().len());
    }

    // Forget the graph so we don't have to wait for deallocation and let the system reclaim the memory at exit
    mem::forget(graph);

    Ok(())
}
