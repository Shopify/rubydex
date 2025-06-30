use std::{
    sync::{Arc, Mutex},
    thread,
};

use crate::{repository::Repository, ruby_indexer::RubyIndexer};

pub mod c_interface;
mod declaration;
mod repository;
mod ruby_indexer;

pub fn index_in_parallel(repository: &Arc<Mutex<Repository>>, file_paths: &Arc<Mutex<Vec<String>>>) {
    let num_threads = thread::available_parallelism().map(|n| n.get()).unwrap_or(4);
    let mut threads = Vec::with_capacity(num_threads);

    for _ in 0..num_threads {
        let repository_clone = Arc::clone(repository);
        let queue_clone = Arc::clone(file_paths);

        let handle = thread::spawn(move || {
            let mut ruby_indexer = RubyIndexer::new(&repository_clone);

            loop {
                let maybe_path = {
                    let mut queue = queue_clone.lock().unwrap();
                    queue.pop()
                };

                if let Some(path) = maybe_path {
                    ruby_indexer.index(path);
                } else {
                    break;
                }
            }
        });

        threads.push(handle);
    }

    for handle in threads {
        handle.join().unwrap();
    }
}
