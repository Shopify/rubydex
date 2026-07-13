#[cfg(all(feature = "jemalloc", not(target_os = "windows")))]
mod imp {
    use std::time::Instant;

    use rubydex::{
        indexing::{self, IndexerBackend},
        listing,
        model::graph::Graph,
        resolution::Resolver,
    };
    use tikv_jemalloc_ctl::{epoch, stats};

    /// Advance the jemalloc epoch (stats are cached between epochs) and read the
    /// number of bytes currently allocated by the application.
    fn allocated_bytes() -> usize {
        epoch::advance().expect("failed to advance jemalloc epoch");
        stats::allocated::read().expect("failed to read stats.allocated (is the `stats` feature on?)")
    }

    pub fn run() {
        let mut args = std::env::args().skip(1);

        let workspace = args
            .next()
            .expect("incorrect usage of cargo bench --bench graph_memory. Please use `utils/bench-graph-memory` instead of invoking this benchmark directly.");
        let workspace_path = std::fs::canonicalize(&workspace).expect("the workspace path must exist");
        assert!(workspace_path.is_dir(), "the workspace path must be a directory");

        let mut graph = Graph::new();
        graph.set_workspace_path(workspace_path);

        if let Err(error) = graph.load_config(None) {
            eprintln!("{error}");
        }

        let time = Instant::now();

        let (file_paths, _) = listing::collect_file_paths(args.collect(), &graph.excluded_patterns());
        println!("Listing {:.2}s", time.elapsed().as_secs_f64());

        let _ = indexing::index_files(&mut graph, file_paths, IndexerBackend::RubyIndexer);
        println!("Indexing {:.2}s", time.elapsed().as_secs_f64());

        Resolver::new(&mut graph).resolve();
        println!("Resolution {:.2}s", time.elapsed().as_secs_f64());

        // Compare the total memory used in the allocator before and after dropping the graph
        let before_drop = allocated_bytes();
        drop(graph);
        let after_drop = allocated_bytes();

        let total_graph_memory = before_drop.saturating_sub(after_drop);

        #[allow(clippy::cast_precision_loss)]
        let mega_bytes = total_graph_memory as f64 / 1024.0 / 1024.0;

        println!("Total graph memory: {mega_bytes:.2} MB");
    }
}

fn main() {
    #[cfg(all(feature = "jemalloc", not(target_os = "windows")))]
    imp::run();
}
