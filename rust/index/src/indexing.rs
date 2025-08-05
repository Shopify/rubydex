use std::{
    sync::{Arc, Mutex},
    thread,
};

use url::Url;

use crate::{indexing::ruby_indexer::RubyIndexer, model::index::Index};

pub mod ruby_indexer;

pub struct Document {
    uri: String,
    source: Option<String>,
}

impl Document {
    #[must_use]
    pub fn new(uri: String, source: Option<String>) -> Self {
        Self { uri, source }
    }
}

/// Indexes the given items, reading the content from disk and populating the given `Index` instance.
///
/// # Panics
/// This function will panic in the event of a thread dead lock, which indicates a bug in our implementation. There
/// should not be any code that tries to lock the same mutex multiple times in the same thread
pub fn index_in_parallel(index_arc: &Arc<Mutex<Index>>, documents: &Arc<Mutex<Vec<Document>>>) {
    let num_threads = thread::available_parallelism().map(std::num::NonZero::get).unwrap_or(4);
    let mut threads = Vec::with_capacity(num_threads);

    for _ in 0..num_threads {
        let queue_clone = Arc::clone(documents);
        let index_clone = Arc::clone(index_arc);

        // Each thread creates its own indexer instance, which they use to visit ASTs. The threads pop from a shared
        // queue to avoid being idle. Definitions are collected for all of the files that got processed and then
        // returned at the end
        let handle = thread::spawn(move || {
            let mut ruby_indexer = RubyIndexer::new();

            loop {
                // Pop a file path from the shared queue. This needs to happen in a small scope to avoid holding the
                // lock for too long, which would cause other threads to slow down
                let maybe_document = {
                    let mut queue = queue_clone.lock().unwrap();
                    queue.pop()
                };

                if let Some(document) = maybe_document {
                    if let Ok(uri) = Url::parse(&document.uri) {
                        let uri_id = index_clone.lock().unwrap().intern_uri(document.uri.to_string());

                        if let Some(source) = document.source {
                            ruby_indexer.index(uri_id, &source);
                        } else if let Ok(source) = std::fs::read_to_string(uri.path()) {
                            ruby_indexer.index(uri_id, &source);
                        }
                    }
                } else {
                    break;
                }
            }

            // Return the collected definitions when joining this thread
            ruby_indexer.definitions
        });

        threads.push(handle);
    }

    let mut results = Vec::new();

    for handle in threads {
        let definitions = handle.join().expect("indexing thread panicked");
        results.push(definitions);
    }

    // Insert all discovered definitions into the global index
    let mut index = index_arc.lock().unwrap();
    for definitions in results {
        index.merge_definitions(definitions);
    }
}
