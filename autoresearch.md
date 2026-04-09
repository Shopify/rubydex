# Autoresearch: File Listing Performance

## Objective
Optimize the file listing/discovery phase of Rubydex. This phase recursively walks a directory tree to find all `.rb` and `.rbs` files. The workload is Shopify core (`/Users/stock/world/trees/root/src/areas/core/shopify`) which is a large Ruby monorepo.

The current implementation uses a work-stealing job queue (`crossbeam-deque`) where each directory becomes a `FileDiscoveryJob` that reads entries, filters by extension, and enqueues subdirectories. Results are collected via crossbeam unbounded channels.

## Metrics
- **Primary**: listing_time (seconds, lower is better)
- **Secondary**: memory_mb (MB, lower is better), file_count (count, must stay constant)

## How to Run
`./autoresearch.sh` — outputs `METRIC name=number` lines.

## Files in Scope
- `rust/rubydex/src/listing.rs` — File discovery job and `collect_file_paths()`. The core code under optimization.
- `rust/rubydex/src/job_queue.rs` — Work-stealing queue infrastructure (`JobQueue`, `Job` trait, worker loop).
- `rust/rubydex/src/main.rs` — CLI entry point; wires up listing, stats, and stop-after flags.
- `rust/rubydex/src/stats/timer.rs` — Timing infrastructure (time_it! macro, Timer).

## Off Limits
- Test correctness: all existing tests in listing.rs must continue to pass.
- The `Job` trait interface used by indexing must remain compatible.
- Do not modify indexing, resolution, or any non-listing code paths.
- Do not add new crate dependencies without strong justification.

## Constraints
- `cargo test -p rubydex listing` must pass after every change.
- File count output must remain identical (same files discovered).
- The solution must work correctly with symlinks and exclusions.

## What's Been Tried
- **Baseline**: ~1.4-1.5s listing time, ~41 MB RSS on Shopify core.
