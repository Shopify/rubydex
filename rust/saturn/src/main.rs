use std::{
    error::Error,
    // mem,
    path::PathBuf,
    sync::{
        Arc,
        Mutex,
        // mpsc
    },
};

use clap::Parser;

use saturn::{
    // errors::MultipleErrors,
    // indexing::{self},
    // indexing,
    job_queue::{FileDiscoveryJob, JobQueue},
    // model::graph::Graph,
    // resolution,
    stats::{
        memory::MemoryStats,
        timer::{Timer, time_it},
    },
    // visualization::dot,
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

#[allow(clippy::unnecessary_wraps)]
fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    Timer::set_global_timer(Timer::new());

    println!("Collecting and indexing files...");

    time_it!(listing2, {
        let queue = Arc::new(JobQueue::new());
        let errors_arc = Arc::new(Mutex::new(Vec::new()));
        let files_arc = Arc::new(Mutex::new(Vec::new()));
        // let (graph_tx, graph_rx) = mpsc::channel();

        queue.push(Box::new(FileDiscoveryJob::new(
            PathBuf::from(args.dir.clone()),
            Arc::clone(&queue),
            // graph_tx.clone(),
            Arc::clone(&files_arc),
            Arc::clone(&errors_arc),
        )));

        // drop(graph_tx);

        JobQueue::run(&queue);

        // let mut graph = Graph::new();
        // for local_graph in graph_rx {
        //     graph.update(local_graph);
        // }
        // graph

        let files = files_arc.lock().unwrap();
        println!("Found {} files", files.len());
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

    // if args.stats {
    //     time_it!(querying, {
    //         graph.print_query_statistics();
    //     });
    // }

    if args.stats {
        Timer::print_breakdown();
        MemoryStats::print_memory_usage();
    }

    // // Generate visualization or print statistics
    // if args.visualize {
    //     println!("{}", dot::generate(&graph));
    // } else {
    //     println!("Indexed {} files", graph.documents().len());
    //     println!("Found {} names", graph.declarations().len());
    //     println!("Found {} definitions", graph.definitions().len());
    //     println!("Found {} URIs", graph.documents().len());
    // }

    // // Forget the graph so we don't have to wait for deallocation and let the system reclaim the memory at exit
    // mem::forget(graph);

    Ok(())
}
