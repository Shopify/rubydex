use std::panic::{self, AssertUnwindSafe};
use std::{
    fs,
    path::{Path, PathBuf},
    sync::atomic::{AtomicUsize, Ordering},
    sync::mpsc::Sender as GraphSender,
    sync::{Arc, Mutex},
    thread,
    time::Duration,
};

use crossbeam_deque::{Injector, Steal, Stealer, Worker};
use url::Url;

use crate::{
    errors::Errors,
    indexing::{local_graph::LocalGraph, ruby_indexer::RubyIndexer},
};

pub struct JobQueue {
    injector: Injector<Box<dyn Job + Send + 'static>>,
    in_flight: AtomicUsize,
}

impl JobQueue {
    #[must_use]
    pub fn new() -> Self {
        Self {
            injector: Injector::new(),
            in_flight: AtomicUsize::new(0),
        }
    }

    /// Enqueue a job for processing.
    ///
    /// # Panics
    ///
    /// Panics if locking the queue's mutex fails.
    pub fn push(&self, job: Box<dyn Job + Send + 'static>) {
        self.in_flight.fetch_add(1, Ordering::Relaxed);
        self.injector.push(job);
    }

    /// Run jobs until the queue is empty. Accepts the shared queue so jobs can enqueue
    /// more work while the runner is processing.
    ///
    /// # Panics
    ///
    /// Panics if locking the queue's mutex fails.
    pub fn run(queue: &Arc<JobQueue>) {
        let worker_count = thread::available_parallelism()
            .map(std::num::NonZeroUsize::get)
            .unwrap_or(4);

        Self::run_with_workers(queue, worker_count);
    }

    fn run_with_workers(queue: &Arc<JobQueue>, worker_count: usize) {
        let mut handles = Vec::with_capacity(worker_count);
        let mut workers = Vec::with_capacity(worker_count);
        let mut stealers = Vec::with_capacity(worker_count);

        for _ in 0..worker_count {
            let worker = Worker::new_fifo();
            stealers.push(worker.stealer());
            workers.push(worker);
        }

        let stealers = Arc::new(stealers);

        for worker in workers {
            let queue = Arc::clone(queue);
            let stealers = Arc::clone(&stealers);
            handles.push(thread::spawn(move || queue.worker_loop(worker, stealers)));
        }

        for handle in handles {
            handle.join().expect("Worker thread panicked");
        }
    }

    #[allow(clippy::needless_pass_by_value)] // workers own their queues; threads take ownership
    fn worker_loop(
        &self,
        local: Worker<Box<dyn Job + Send + 'static>>,
        stealers: Arc<Vec<Stealer<Box<dyn Job + Send + 'static>>>>,
    ) {
        let mut backoff = 0u32;
        loop {
            let Some(job) = Self::steal_job(&local, &stealers, &self.injector) else {
                if self.in_flight.load(Ordering::Acquire) == 0 {
                    break;
                }
                // Prefer to stay hot for short bursts; back off if we remain idle.
                if backoff < 4 {
                    thread::yield_now();
                } else {
                    let sleep_micros = 5u64.saturating_mul(1 << backoff.min(10));
                    thread::park_timeout(Duration::from_micros(sleep_micros));
                }
                backoff = (backoff + 1).min(10);
                continue;
            };

            backoff = 0;
            let result = panic::catch_unwind(AssertUnwindSafe(|| job.run()));

            self.in_flight.fetch_sub(1, Ordering::Relaxed);

            if let Err(payload) = result {
                panic::resume_unwind(payload);
            }
        }
    }

    fn steal_job(
        local: &Worker<Box<dyn Job + Send + 'static>>,
        stealers: &[Stealer<Box<dyn Job + Send + 'static>>],
        injector: &Injector<Box<dyn Job + Send + 'static>>,
    ) -> Option<Box<dyn Job + Send + 'static>> {
        if let Some(job) = local.pop() {
            return Some(job);
        }

        match injector.steal_batch_and_pop(local) {
            Steal::Success(job) => return Some(job),
            Steal::Retry => return None,
            Steal::Empty => {}
        }

        for stealer in stealers {
            match stealer.steal_batch_and_pop(local) {
                Steal::Success(job) => return Some(job),
                Steal::Retry => return None,
                Steal::Empty => {}
            }
        }

        None
    }
}

impl Default for JobQueue {
    fn default() -> Self {
        Self::new()
    }
}

// ------------------------------------------------------------
// Jobs
// ------------------------------------------------------------

pub trait Job: Send {
    fn run(&self);
}

pub struct FileDiscoveryJob {
    path: PathBuf,
    queue: Arc<JobQueue>,
    graph_tx: GraphSender<LocalGraph>,
    errors: Arc<Mutex<Vec<Errors>>>,
}

// ------------------------------------------------------------
// File Discovery Job
// ------------------------------------------------------------

impl FileDiscoveryJob {
    #[must_use]
    pub fn new(
        path: PathBuf,
        queue: Arc<JobQueue>,
        graph_tx: GraphSender<LocalGraph>,
        errors: Arc<Mutex<Vec<Errors>>>,
    ) -> Self {
        Self {
            path,
            queue,
            graph_tx,
            errors,
        }
    }
}

impl Job for FileDiscoveryJob {
    fn run(&self) {
        if self.path.is_dir() {
            for entry in self.path.read_dir().unwrap() {
                let Ok(entry) = entry else {
                    self.errors.lock().unwrap().push(Errors::FileReadError(format!(
                        "Unable to read entry under '{}'",
                        self.path.display()
                    )));
                    continue;
                };

                let Ok(file_type) = entry.file_type() else {
                    self.errors.lock().unwrap().push(Errors::FileReadError(format!(
                        "Unable to read metadata for '{}'",
                        entry.path().display()
                    )));
                    continue;
                };

                let subpath = entry.path();

                if file_type.is_dir() {
                    self.queue.push(Box::new(FileDiscoveryJob::new(
                        subpath,
                        Arc::clone(&self.queue),
                        self.graph_tx.clone(),
                        Arc::clone(&self.errors),
                    )));
                } else if file_type.is_file() {
                    if subpath.extension().is_some_and(|ext| ext == "rb") {
                        self.queue.push(Box::new(IndexingJob::new(
                            subpath,
                            self.graph_tx.clone(),
                            Arc::clone(&self.errors),
                        )));
                    }
                } else {
                    self.errors.lock().unwrap().push(Errors::FileReadError(format!(
                        "Path '{}' is not a file or directory",
                        subpath.display()
                    )));
                }
            }
        } else if self.path.is_file() {
            if self.path.extension().is_some_and(|ext| ext == "rb") {
                self.queue.push(Box::new(IndexingJob::new(
                    self.path.clone(),
                    self.graph_tx.clone(),
                    Arc::clone(&self.errors),
                )));
            }
        } else {
            self.errors.lock().unwrap().push(Errors::FileReadError(format!(
                "Path '{}' is not a file or directory",
                self.path.display()
            )));
        }
    }
}

// ------------------------------------------------------------
// Indexing Job
// ------------------------------------------------------------

pub struct IndexingJob {
    path: PathBuf,
    graph_tx: GraphSender<LocalGraph>,
    errors: Arc<Mutex<Vec<Errors>>>,
}

impl IndexingJob {
    #[must_use]
    pub fn new(path: PathBuf, graph_tx: GraphSender<LocalGraph>, errors: Arc<Mutex<Vec<Errors>>>) -> Self {
        Self { path, graph_tx, errors }
    }
}

impl Job for IndexingJob {
    fn run(&self) {
        if let Err(err) = self.index_file() {
            self.errors.lock().unwrap().push(err);
        }
    }
}

impl IndexingJob {
    fn index_file(&self) -> Result<(), Errors> {
        let source = fs::read_to_string(&self.path)
            .map_err(|e| Errors::FileReadError(format!("Failed to read {}: {e}", self.path.display())))?;

        let uri = path_to_uri(&self.path)?;

        let mut ruby_indexer = RubyIndexer::new(uri, &source);
        ruby_indexer.index();

        let local_graph = ruby_indexer.local_graph();

        self.graph_tx
            .send(local_graph)
            .expect("graph receiver dropped before merge");

        Ok(())
    }
}

fn path_to_uri(path: &Path) -> Result<String, Errors> {
    let canonicalized = fs::canonicalize(path)
        .map_err(|_e| Errors::FileReadError(format!("Failed to canonicalize path '{}'", path.display())))?;

    let url = Url::from_file_path(&canonicalized)
        .map_err(|_e| Errors::InvalidUri(format!("Couldn't build URI from path '{}'", canonicalized.display())))?;

    Ok(url.to_string())
}
