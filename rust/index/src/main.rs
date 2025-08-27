use std::{
    error::Error,
    sync::{Arc, Mutex},
};

use clap::Parser;

use index::{
    indexing::{self, errors::MultipleErrors},
    model::graph::Graph,
    visualization::dot,
};

#[derive(Parser, Debug)]
#[command(name = "index_cli", about = "A Ruby code indexer", version)]
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
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let mut graph = Graph::new();
    graph.set_configuration(format!("{}/graph.db", &args.dir))?;
    let (documents, errors) = indexing::collect_documents_in_parallel(vec![args.dir]);

    if !errors.is_empty() {
        return Err(Box::new(MultipleErrors(errors)));
    }

    let graph_arc = Arc::new(Mutex::new(graph));
    indexing::index_in_parallel(&graph_arc, &documents)?;

    // Run integrity checks if requested
    if args.check_integrity {
        let graph_lock = graph_arc.lock().unwrap();
        let errors = Graph::integrity_checker().apply(&graph_lock);

        if errors.is_empty() {
            println!("✓ Index integrity check passed");
        } else {
            eprintln!("✗ Index integrity check failed with {} errors:", errors.len());
            for error in &errors {
                eprintln!("  - {error}");
            }
            std::process::exit(1);
        }
    }

    // Generate visualization or print statistics
    let graph_lock = graph_arc.lock().unwrap();
    if args.visualize {
        println!("{}", dot::generate(&graph_lock));
    } else {
        println!("Indexed {} files", documents.len());
        println!("Found {} names", graph_lock.names().len());
        println!("Found {} definitions", graph_lock.definitions().len());
        println!("Found {} URIs", graph_lock.uri_pool().len());
    }

    Ok(())
}
