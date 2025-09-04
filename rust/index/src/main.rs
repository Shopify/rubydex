use std::error::Error;

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
    let (documents, errors) = indexing::collect_documents(vec![args.dir]);

    if !errors.is_empty() {
        return Err(Box::new(MultipleErrors(errors)));
    }

    indexing::index_in_parallel(&mut graph, &documents)?;

    // Run integrity checks if requested
    if args.check_integrity {
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
    }

    // Generate visualization or print statistics
    if args.visualize {
        println!("{}", dot::generate(&graph));
    } else {
        println!("Indexed {} files", documents.len());
        println!("Found {} names", graph.declarations().len());
        println!("Found {} definitions", graph.definitions().len());
        println!("Found {} URIs", graph.uri_pool().len());
    }

    Ok(())
}
