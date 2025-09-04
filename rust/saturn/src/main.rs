use std::error::Error;

use clap::Parser;

use saturn::{
    indexing::{self, errors::MultipleErrors},
    model::graph::Graph,
    timer::{Timer, time_it},
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

    #[arg(long = "clear-db", help = "Clear the database before saving the graph")]
    clear_db: bool,

    #[arg(long = "stats", help = "Show detailed performance statistics")]
    stats: bool,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    if args.stats {
        Timer::set_global_timer(Timer::new());
    }

    let mut graph = time_it!(setup, {
        let mut graph = Graph::new();
        graph.set_configuration(format!("{}/graph.db", &args.dir))?;
        graph
    });

    let (documents, errors) = time_it!(listing, {
        let (documents, errors) = indexing::collect_documents(vec![args.dir.clone()]);
        Ok::<_, Box<dyn Error>>((documents, errors))
    })?;

    if !errors.is_empty() {
        return Err(Box::new(MultipleErrors(errors)));
    }

    time_it!(indexing, { indexing::index_in_parallel(&mut graph, documents) })?;

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

    if args.clear_db {
        graph.clear_database()?;
    }

    time_it!(database, { graph.save_to_database() })?;

    if args.stats {
        time_it!(querying, {
            graph.print_query_statistics();
        });
    }

    if args.stats {
        Timer::print_breakdown();
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
