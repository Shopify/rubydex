use std::panic::{self, AssertUnwindSafe};
use std::{
    sync::Arc,
    sync::atomic::{AtomicUsize, Ordering},
    thread,
};

use crossbeam_deque::{Injector, Steal, Stealer, Worker};
use crossbeam_utils::Backoff;

/// Work-stealing queue that balances jobs across worker threads.
///
/// Jobs are pushed onto the global `injector`, then workers move them into their
/// own local queues. Work stealing lets idle workers drain the global injector
/// first and then steal from peers, keeping the CPU busy without coarse locks.
/// `in_flight` tracks outstanding jobs so threads can tell when all work
/// (including work spawned by other jobs) has finished.
#[derive(Default)]
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
    pub fn push(&self, job: Box<dyn Job + Send + 'static>) {
        // Increment the count of in-flight jobs to indicate a new job has been enqueued.
        self.in_flight.fetch_add(1, Ordering::Relaxed);
        // Push the job onto the global injector queue for later execution by workers.
        self.injector.push(job);
    }

    /// Run jobs until the queue is empty.
    ///
    /// Accepts the shared queue so jobs can enqueue more work while the runner is processing.
    pub fn run(queue: &Arc<JobQueue>) {
        // Determine the number of worker threads to launch.
        // Use the number of available logical CPUs, falling back to 4 if detection fails.
        let worker_count = thread::available_parallelism()
            .map(std::num::NonZeroUsize::get)
            .unwrap_or(4);

        Self::run_with_workers(queue, worker_count);
    }

    /// Spin up `worker_count` threads, each with its own local queue, and block until all threads finish.
    ///
    /// Workers steal from each other and from the global injector to keep the work balanced.
    fn run_with_workers(queue: &Arc<JobQueue>, worker_count: usize) {
        let mut handles = Vec::with_capacity(worker_count);
        let mut workers = Vec::with_capacity(worker_count);
        let mut stealers = Vec::with_capacity(worker_count);

        for _ in 0..worker_count {
            let worker = Worker::new_fifo();
            stealers.push(worker.stealer()); // For stealing jobs from this queue
            workers.push(worker);
        }

        // Wrap all stealers in an Arc so they can be shared across all threads.
        let stealers = Arc::new(stealers);

        // Start a worker thread for each local queue.
        for worker in workers {
            let queue = Arc::clone(queue);
            let stealers = Arc::clone(&stealers);
            handles.push(thread::spawn(move || queue.worker_loop(&worker, &stealers)));
        }

        // Wait for all worker threads to finish before returning.
        for handle in handles {
            handle.join().expect("Worker thread panicked");
        }
    }

    /// Drain work for a single worker.
    ///
    /// The worker prefers its local queue, then the global injector, then other
    /// workers. If no job is immediately available, it backs off briefly and
    /// exits once `in_flight` reaches zero (meaning no pending work anywhere).
    fn worker_loop(
        &self,
        local: &Worker<Box<dyn Job + Send + 'static>>,
        stealers: &Arc<Vec<Stealer<Box<dyn Job + Send + 'static>>>>,
    ) {
        // Create a backoff utility for yielding when no job is immediately available
        let backoff = Backoff::new();

        // Loop until all work is done (in_flight reaches zero)
        loop {
            // Try to steal the next job to execute. Prioritize own local queue, then global, then peers.
            let Some(job) = Self::steal_job(local, stealers, &self.injector) else {
                if self.in_flight.load(Ordering::Acquire) == 0 {
                    // All work is done, exit the worker loop
                    break;
                }
                // No work found and still pending jobs: brief pause before retrying
                backoff.snooze();
                continue;
            };

            // Reset backoff as we've obtained a job to process
            backoff.reset();

            // Run the job and capture any panics so we can propagate them safely
            let result = panic::catch_unwind(AssertUnwindSafe(|| job.run()));

            // Job completed; decrement the in-flight job counter
            self.in_flight.fetch_sub(1, Ordering::Relaxed);

            // If the job panicked, resume the panic on this thread
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
        // First, try to pop a job from the worker's own local queue (fastest, lowest contention).
        if let Some(job) = local.pop() {
            return Some(job);
        }

        // If the local queue is empty, try to steal a batch of jobs from the global injector queue.
        match injector.steal_batch_and_pop(local) {
            Steal::Success(job) => return Some(job), // Successfully stole a job from the global injector.
            Steal::Retry => return None, // Transient failure; signal to caller to back off and try again later.
            Steal::Empty => {}           // No jobs available in the global injector; continue to peers.
        }

        // As a last resort, attempt to steal jobs from each of the peer workers' queues.
        for stealer in stealers {
            match stealer.steal_batch_and_pop(local) {
                Steal::Success(job) => return Some(job), // Successfully stole a job from a peer.
                Steal::Retry => return None,             // Retry advised; caller should back off and loop.
                Steal::Empty => {}                       // No jobs in this peer; try next peer.
            }
        }

        // No jobs available from any source.
        None
    }
}

/// Unit of work scheduled on a `JobQueue`.
///
/// # Example
///
/// ```
/// use std::sync::Arc;
/// use rubydex::job_queue::{Job, JobQueue};
///
/// struct PrintJob;
///
/// impl Job for PrintJob {
///     fn run(&self) {
///         println!("hello from a worker");
///     }
/// }
///
/// let queue = Arc::new(JobQueue::new());
/// queue.push(Box::new(PrintJob));
/// JobQueue::run(&queue);
/// ```
pub trait Job: Send {
    fn run(&self);
}
