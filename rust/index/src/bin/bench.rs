use std::error::Error;
use std::time::{Duration, Instant};

use clap::Parser;

use index::{
    indexing::{self, errors::MultipleErrors},
    model::graph::Graph,
};

#[derive(Parser, Debug)]
#[command(name = "bench", about = "Benchmark the Ruby indexer", version)]
struct Args {
    #[arg(value_name = "CORPUS_PATH")]
    corpus_path: String,
}

fn time_it<T, F>(f: F) -> (T, Duration)
where
    F: FnOnce() -> T,
{
    let start = Instant::now();
    let result = f();
    let duration = start.elapsed();
    (result, duration)
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let corpus_path = std::path::Path::new(&args.corpus_path);

    let total_start = Instant::now();
    let setup_duration;
    let indexing_duration;

    {
        let ((mut graph, documents, errors), setup_dur) = time_it(|| {
            let mut graph = Graph::new();
            graph
                .set_configuration(format!("{}/graph.db", corpus_path.display()))
                .unwrap();
            let (documents, errors) = indexing::collect_documents(vec![corpus_path.to_string_lossy().to_string()]);
            (graph, documents, errors)
        });
        setup_duration = setup_dur;

        if !errors.is_empty() {
            return Err(Box::new(MultipleErrors(errors)));
        }

        let ((), indexing_dur) = time_it(|| {
            indexing::index_in_parallel(&mut graph, documents).unwrap();
        });
        indexing_duration = indexing_dur;
    }

    let total_duration = total_start.elapsed();
    let cleanup_duration = total_duration - (setup_duration + indexing_duration);

    println!();
    println!("PERFORMANCE BREAKDOWN");
    println!();

    let total_time = total_duration.as_secs_f64();
    let setup_time = setup_duration.as_secs_f64();
    let indexing_time = indexing_duration.as_secs_f64();
    let cleanup_time = cleanup_duration.as_secs_f64();

    println!(
        "Initialization: {:8.3}s ({:5.1}%)",
        setup_time,
        setup_time * 100.0 / total_time
    );
    println!(
        "Indexing:       {:8.3}s ({:5.1}%)",
        indexing_time,
        indexing_time * 100.0 / total_time
    );
    println!(
        "Cleanup:        {:8.3}s ({:5.1}%)",
        cleanup_time,
        cleanup_time * 100.0 / total_time
    );
    println!("Total:          {total_time:8.3}s");

    println!();

    Ok(())
}
