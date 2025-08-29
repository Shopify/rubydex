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

    #[arg(long = "timers")]
    timers: bool,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    let mut graph = Graph::new();
    graph.set_configuration(format!("{}/graph.db", &args.dir))?;

    graph.timers().collect_documents().start();
    let (documents, errors) = indexing::collect_documents(vec![args.dir]);
    graph.timers().collect_documents().stop();

    if !errors.is_empty() {
        return Err(Box::new(MultipleErrors(errors)));
    }

    graph.timers().index_documents().start();
    indexing::index_in_parallel(&mut graph, &documents)?;
    graph.timers().index_documents().stop();

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
        println!("Found {} names", graph.names().len());
        println!("Found {} definitions", graph.definitions().len());
        println!("Found {} URIs", graph.uri_pool().len());
    }

    graph.timers().clear_graph().start();
    graph.clear_graph_data();
    graph.timers().clear_graph().stop();

    if args.timers {
        graph.timers().print();
    }

    Ok(())
}
