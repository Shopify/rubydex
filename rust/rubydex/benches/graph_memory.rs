#[cfg(all(feature = "jemalloc", not(target_os = "windows")))]
mod imp {
    use std::collections::HashSet;

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
        let paths: Vec<String> = std::env::args().skip(1).collect();
        let paths = if paths.is_empty() { vec![".".to_string()] } else { paths };
        let (file_paths, _) = listing::collect_file_paths(paths, &HashSet::new());

        let mut graph = Graph::new();
        let _ = indexing::index_files(&mut graph, file_paths, IndexerBackend::RubyIndexer);
        Resolver::new(&mut graph).resolve();

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
