use std::{error::Error, fs};

use clap::Parser;

use saturn::{
    indexing::{self, errors::MultipleErrors},
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

    #[arg(long = "name", help = "Filter visualization to show only the specified name")]
    name: Option<String>,

    #[arg(long = "use-db", help = "Store the graph in a database", default_value = "false")]
    use_db: bool,

    #[arg(long = "clear-db", help = "Clear the database before saving the graph")]
    clear_db: bool,

    #[arg(long = "stats", help = "Show detailed performance statistics")]
    stats: bool,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    validate_args(&args);

    if args.stats {
        Timer::set_global_timer(Timer::new());
    }

    let db_path = format!("{}/graph.db", &args.dir);
    if args.clear_db {
        let _ = fs::remove_file(&db_path);
    }

    let mut graph = time_it!(setup, {
        let mut graph = Graph::new();

        if args.use_db {
            graph.set_configuration(db_path)?;
        }

        graph
    });

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

    if args.use_db {
        time_it!(database, { graph.save_to_database() })?;
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

    if args.visualize {
        generate_visualization(&graph, args.name.as_deref())?;
    } else {
        print_statistics(&graph);
    }

    Ok(())
}

fn validate_args(args: &Args) {
    if args.name.is_some() && !args.visualize {
        eprintln!("Error: --name can only be used with --visualize");
        std::process::exit(1);
    }
}

fn generate_visualization(graph: &Graph, name_filter: Option<&str>) -> Result<(), Box<dyn Error>> {
    use saturn::visualization::renderer::GraphRenderer;

    let mut renderer = GraphRenderer::new(graph);

    if let Some(name) = name_filter {
        renderer = renderer.with_name_filter(name)?;
    }

    let renderable = renderer.render();
    let output = dot::generate(&renderable);
    println!("{output}");
    Ok(())
}

fn print_statistics(graph: &Graph) {
    println!("Indexed {} files", graph.documents().len());
    println!("Found {} names", graph.declarations().len());
    println!("Found {} definitions", graph.definitions().len());
    println!("Found {} URIs", graph.documents().len());
}
