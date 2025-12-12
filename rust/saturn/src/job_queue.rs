use std::panic::{self, AssertUnwindSafe};
use std::{
    fs,
    path::{Path, PathBuf},
    sync::Arc,
    sync::atomic::{AtomicUsize, Ordering},
    sync::mpsc::Sender as GraphSender,
    thread,
};

use crossbeam_channel::Sender;
use crossbeam_deque::{Injector, Steal, Stealer, Worker};
use crossbeam_utils::Backoff;
use url::Url;

use crate::{
    errors::Errors,
    indexing::{local_graph::LocalGraph, ruby_indexer::RubyIndexer},
};

/// Work-stealing queue that balances jobs across worker threads.
///
/// Jobs are pushed onto the global `injector`, then workers move them into their
/// own local queues. Work stealing lets idle workers drain the global injector
/// first and then steal from peers, keeping the CPU busy without coarse locks.
/// `in_flight` tracks outstanding jobs so threads can tell when all work
/// (including work spawned by other jobs) has finished.
pub struct JobQueue {
    /// Global queue feeding newly discovered jobs to workers.
    injector: Injector<Box<dyn Job + Send + 'static>>,
    /// Count of jobs that have been queued but not yet completed.
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
    /// Panics if job execution unwinds.
    pub fn push(&self, job: Box<dyn Job + Send + 'static>) {
        self.in_flight.fetch_add(1, Ordering::Relaxed);
        self.injector.push(job);
    }

    /// Run jobs until the queue is empty. Accepts the shared queue so jobs can
    /// enqueue more work while the runner is processing.
    ///
    /// # Panics
    ///
    /// Panics if job execution unwinds.
    pub fn run(queue: &Arc<JobQueue>) {
        let worker_count = thread::available_parallelism()
            .map(std::num::NonZeroUsize::get)
            .unwrap_or(4);

        Self::run_with_workers(queue, worker_count);
    }

    /// Spin up `worker_count` threads, each with its own local queue, and block
    /// until all threads finish. Workers steal from each other and from the
    /// global injector to keep the work balanced.
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
    /// Drain work for a single worker.
    ///
    /// The worker prefers its local queue, then the global injector, then other
    /// workers. If no job is immediately available, it backs off briefly and
    /// exits once `in_flight` reaches zero (meaning no pending work anywhere).
    fn worker_loop(
        &self,
        local: Worker<Box<dyn Job + Send + 'static>>,
        stealers: Arc<Vec<Stealer<Box<dyn Job + Send + 'static>>>>,
    ) {
        let backoff = Backoff::new();
        loop {
            let Some(job) = Self::steal_job(&local, &stealers, &self.injector) else {
                if self.in_flight.load(Ordering::Acquire) == 0 {
                    break;
                }
                backoff.snooze();
                continue;
            };

            backoff.reset();
            let result = panic::catch_unwind(AssertUnwindSafe(|| job.run()));

            self.in_flight.fetch_sub(1, Ordering::Relaxed);

            if let Err(payload) = result {
                panic::resume_unwind(payload);
            }
        }
    }

    /// Find the next job for a worker.
    ///
    /// Priority: pop from the worker's local queue, steal a batch from the
    /// global injector, then try stealing from peer workers. Returning `None`
    /// signals the caller to back off or eventually exit if no jobs remain.
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
    // graph_tx: GraphSender<LocalGraph>,
    files_tx: Sender<PathBuf>,
    errors_tx: Sender<Errors>,
}

// ------------------------------------------------------------
// File Discovery Job
// ------------------------------------------------------------

impl FileDiscoveryJob {
    #[must_use]
    pub fn new(
        path: PathBuf,
        queue: Arc<JobQueue>,
        // graph_tx: GraphSender<LocalGraph>,
        files_tx: Sender<PathBuf>,
        errors_tx: Sender<Errors>,
    ) -> Self {
        Self {
            path,
            queue,
            // graph_tx,
            files_tx,
            errors_tx,
        }
    }
}

impl Job for FileDiscoveryJob {
    fn run(&self) {
        if self.path.is_dir() {
            for entry in self.path.read_dir().unwrap() {
                let Ok(entry) = entry else {
                    self.errors_tx
                        .send(Errors::FileReadError(format!(
                            "Unable to read entry under '{}'",
                            self.path.display()
                        )))
                        .expect("error receiver dropped before run completion");
                    continue;
                };

                let Ok(file_type) = entry.file_type() else {
                    self.errors_tx
                        .send(Errors::FileReadError(format!(
                            "Unable to read metadata for '{}'",
                            entry.path().display()
                        )))
                        .expect("error receiver dropped before run completion");
                    continue;
                };

                let subpath = entry.path();

                if file_type.is_dir() {
                    self.queue.push(Box::new(FileDiscoveryJob::new(
                        subpath,
                        Arc::clone(&self.queue),
                        // self.graph_tx.clone(),
                        self.files_tx.clone(),
                        self.errors_tx.clone(),
                    )));
                } else if file_type.is_file() {
                    if subpath.extension().is_some_and(|ext| ext == "rb") {
                        // self.queue.push(Box::new(IndexingJob::new(
                        //     subpath,
                        //     self.graph_tx.clone(),
                        //     Arc::clone(&self.errors),
                        // )));
                        self.files_tx
                            .send(subpath)
                            .expect("file receiver dropped before run completion");
                    }
                } else {
                    self.errors_tx
                        .send(Errors::FileReadError(format!(
                            "Path '{}' is not a file or directory",
                            subpath.display()
                        )))
                        .expect("error receiver dropped before run completion");
                }
            }
        } else if self.path.is_file() {
            if self.path.extension().is_some_and(|ext| ext == "rb") {
                // self.queue.push(Box::new(IndexingJob::new(
                //     self.path.clone(),
                //     self.graph_tx.clone(),
                //     Arc::clone(&self.files),
                //     Arc::clone(&self.errors),
                // )));
                self.files_tx
                    .send(self.path.clone())
                    .expect("file receiver dropped before run completion");
            }
        } else {
            self.errors_tx
                .send(Errors::FileReadError(format!(
                    "Path '{}' is not a file or directory",
                    self.path.display()
                )))
                .expect("error receiver dropped before run completion");
        }
    }
}

// ------------------------------------------------------------
// Indexing Job
// ------------------------------------------------------------

pub struct IndexingJob {
    path: PathBuf,
    graph_tx: GraphSender<LocalGraph>,
    errors_tx: Sender<Errors>,
}

impl IndexingJob {
    #[must_use]
    pub fn new(path: PathBuf, graph_tx: GraphSender<LocalGraph>, errors_tx: Sender<Errors>) -> Self {
        Self {
            path,
            graph_tx,
            errors_tx,
        }
    }
}

impl Job for IndexingJob {
    fn run(&self) {
        if let Err(err) = self.index_file() {
            self.errors_tx
                .send(err)
                .expect("error receiver dropped before run completion");
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
