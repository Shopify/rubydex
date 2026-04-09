# Autoresearch: File Listing Performance

## Objective
Optimize the file listing/discovery phase of Rubydex. This phase recursively walks a directory tree to find all `.rb` and `.rbs` files. The workload is Shopify core (`/Users/stock/world/trees/root/src/areas/core/shopify`) which is a large Ruby monorepo: 38K directories, 760K total files, 102K Ruby files.

## Metrics
- **Primary**: listing_time (seconds, lower is better)
- **Secondary**: memory_mb (MB, lower is better), file_count (count, must stay constant)

## How to Run
`./autoresearch.sh` — outputs `METRIC name=number` lines.

## Files in Scope
- `rust/rubydex/src/listing.rs` — File discovery and `collect_file_paths()`. The core code under optimization.
- `rust/rubydex/src/job_queue.rs` — Work-stealing queue infrastructure (no longer used by listing).
- `rust/rubydex/src/main.rs` — CLI entry point.

## Off Limits
- Test correctness: all existing tests in listing.rs must pass.
- The `Job` trait interface used by indexing must remain compatible.
- Do not modify indexing, resolution, or non-listing code.
- Do not add new crate dependencies without strong justification.
- Do not overfit for this specific codebase — RBS files matter even if rare in the test set.

## Constraints
- `cargo test -p rubydex listing` must pass.
- File count must remain identical.
- Symlinks and exclusions must work correctly.

## Results Summary
- **Baseline**: ~1.5s, ~41 MB
- **Current best**: ~0.95s, ~34 MB
- **Improvement**: ~37% faster, ~17% less memory
- **Already 3x faster than GNU `find`** (2.0s for same workload)

## What's Been Tried

### Kept (cumulative improvements)
1. **Replace JobQueue with direct thread::scope** — Eliminated Box<dyn Job> heap allocations, channel sends, Arc clones. Used crossbeam Injector<PathBuf> directly. ~3.5% improvement.
2. **Push subdirs to local Worker queue** — Better cache locality, reduces injector contention. Workers process nearby directories first, peers steal when idle. ~20% improvement.
3. **LIFO worker queues** — DFS-like traversal improves filesystem cache locality. ~14% improvement.
4. **Half the available threads** — Directory walking is I/O-bound. On Apple Silicon (6P+6E cores), n/2 threads uses only performance cores, avoiding slower E-cores. ~40% improvement (biggest single win).
5. **Reusable path buffer** — Maintain one PathBuf per directory, push/pop child names instead of allocating fresh via entry.path(). Reduces memory 22%.

### Discarded (did not improve or made things worse)
- **Skip PathBuf for non-Ruby files** — OsStr extension check was already fast; within noise.
- **WorkItem enum to avoid stat** — Enum tag overhead per queue item outweighed saving 1 stat call.
- **Pre-seed queue** — Reading root directory before workers didn't help; LIFO+stealing already distributes well.
- **Backoff vs yield_now** — No difference; workers rarely idle.
- **Raw libc readdir** — Eliminates OsString allocations (660K) but time improvement within noise. Not worth the unsafe code.
- **Mutex<Vec<PathBuf>> work stack** — Lock contention too high vs lock-free work stealing.
- **Inline recursive DFS** — Kills parallelism. I/O needs all threads busy.
- **Hybrid push-first-recurse-rest** — Same issue; loses too much parallelism.
- **Pre-allocated file vectors** — Vec amortized doubling already efficient.
- **Quarter/2/3/fixed thread counts** — n/2 is the sweet spot.

### Key Insights
- The workload is I/O-bound, not CPU-bound. Reducing threads helps more than reducing per-entry overhead.
- On Apple Silicon, n/2 threads likely maps to performance cores only, avoiding P/E core scheduling issues.
- Work-stealing with LIFO local queues is the right pattern: DFS locality + automatic load balancing.
- The crossbeam-deque work-stealing queue is extremely efficient; replacing it with Mutex was much worse.
- PathBuf allocation overhead is significant for memory but not for time (the allocator handles 140K small allocations fast).
- Fully recursive inline DFS destroys parallelism — even though it eliminates all synchronization overhead, the lost parallelism (from 6 threads down to ~1) makes it 2x slower.
