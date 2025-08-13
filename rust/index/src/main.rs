use std::{
    error::Error,
    sync::{Arc, Mutex},
};

use clap::Parser;

use index::{
    indexing::{self, errors::MultipleErrors},
    model::graph::Graph,
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
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let mut graph = Graph::new();
    graph.set_configuration(format!("{}/graph.db", &args.dir));
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

    Ok(())
}
