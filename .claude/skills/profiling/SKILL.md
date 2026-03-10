---
name: profiling
description: >
  Profile Rubydex indexer performance — CPU flamegraphs, memory usage, phase-level timing.
  Use this skill whenever the user mentions profiling, performance, flamegraphs, benchmarking,
  "why is X slow", bottlenecks, hot paths, memory usage, or wants to understand where time
  is spent during indexing/resolution. Also trigger when comparing performance before/after
  a change.
---

# Profiling Rubydex

This skill helps you profile the Rubydex indexer to find CPU and memory bottlenecks.
The indexer has a multi-phase pipeline (listing → indexing → resolution → querying).
Use `--stats` to see which phase dominates, then profile to find what's expensive inside it.

## Profiling tool: samply

Use **samply** — a sampling profiler that opens results in Firefox Profiler (in-browser).
It captures call stacks at high frequency and produces interactive flamegraphs with filtering,
timeline views, and per-function cost breakdowns.

Install if needed:

```bash
cargo install samply
```

## Build profile

Profiling needs optimized code *with* debug symbols so you get real function names in the
flamegraph instead of mangled addresses. The workspace Cargo.toml has a custom profile for this:

```toml
# rust/Cargo.toml
[profile.profiling]
inherits = "release"
debug = true        # Full debug symbols for readable flamegraphs
strip = false       # Keep symbols in the binary
```

If this profile doesn't exist yet, **add it** to `rust/Cargo.toml` before profiling. The
release profile uses `lto = true`, `opt-level = 3`, `codegen-units = 1` — the profiling
profile inherits all of that and just adds debug info.

Build with:

```bash
cargo build --profile profiling
```

The binary lands at `rust/target/profiling/rubydex_cli` (not `target/release/`).

The first build is slow (LTO + single codegen unit must recompile everything). Subsequent
builds after small changes are faster since Cargo caches intermediate artifacts in
`target/profiling/`. Don't delete that directory between runs.

## Running a profile

### Full pipeline

```bash
samply record rust/target/profiling/rubydex_cli <TARGET_PATH> --stats
```

The `--stats` flag prints the timing breakdown and memory stats to stderr after completion,
so you get both the samply profile AND the summary stats in one run.

Useful samply flags:
- `--no-open` — don't auto-open the browser (useful for scripted runs)
- `--save-only` — save the profile to disk without starting the local server; load later
  with `samply load <profile.json>`

### Isolating a phase

Use `--stop-after` to profile only up to a specific stage. This is useful when you want
a cleaner flamegraph focused on one phase without the noise of later stages:

```bash
# Profile only listing + indexing (skip resolution)
samply record rust/target/profiling/rubydex_cli <TARGET_PATH> --stats --stop-after indexing

# Profile through resolution (skip querying)
samply record rust/target/profiling/rubydex_cli <TARGET_PATH> --stats --stop-after resolution
```

Valid `--stop-after` values: `listing`, `indexing`, `resolution`.

### Common target paths

The user should have a `DEFAULT_BENCH_WORKSPACE` configured pointing to a target codebase.

For synthetic corpora, use `utils/bench` with size arguments (tiny/small/medium/large/huge),
which auto-generates corpora at `../rubydex_corpora/<size>/`.

## Reading the results

When samply finishes, it automatically opens Firefox Profiler in the browser. Key things
to guide the user through:

### Firefox Profiler tips

1. **Call Tree tab** — shows cumulative time per function, sorted by total cost. Start here
   to find the most expensive call paths.

2. **Flame Graph tab** — visual representation where width = time. Look for wide bars — those
   are the hot functions. Click to zoom in.

3. **Timeline** — shows activity over time. Useful for spotting if one phase is unexpectedly
   long or if there are idle gaps.

4. **Filtering** — type a function name in the filter box to isolate it.

5. **Transform > Focus on subtree** — right-click a function to see only its callees. Perfect
   for drilling into a specific phase.

6. **Transform > Merge function** — collapse recursive calls to see aggregate cost.

### Text-based profiling with `sample` (macOS)

When you can't interact with a browser (e.g., running from a script or agent), use macOS's
built-in `sample` command for a text-based call tree:

```bash
# Start the indexer in the background, then sample it
rust/target/profiling/rubydex_cli <TARGET_PATH> --stats &
PID=$!
sample $PID -f /tmp/rubydex-sample.txt
wait $PID
```

Or for a simpler approach, sample for a fixed duration while the indexer runs:

```bash
rust/target/profiling/rubydex_cli <TARGET_PATH> --stats &
PID=$!
sleep 2  # let it get past listing/indexing into resolution
sample $PID 30 -f /tmp/rubydex-sample.txt  # sample for 30 seconds
wait $PID
```

The output is a text call tree with sample counts — sort by "self" samples to find hot functions.

### How to read the profile

Don't assume which functions are hot — let the data tell you. Hot paths change as the
codebase evolves.

1. **Sort by self-time** (time spent in the function itself, not its callees). This reveals
   the actual hot spots. High total-time but low self-time means the function is just a
   caller — drill into its children.

2. **Look for concentration vs. spread.** A single function dominating self-time suggests
   an algorithmic fix (memoization, better data structure). Time spread across many functions
   suggests the workload itself is large and optimization requires a different approach.

3. **Check for allocation pressure.** If `alloc` / `malloc` / `realloc` show up prominently
   in self-time, the bottleneck is memory allocation, not computation.

## Memory profiling

For memory, the `--stats` flag already reports Maximum RSS at the end. For deeper memory
analysis:

### Quick check with utils/mem-use

```bash
utils/mem-use rust/target/profiling/rubydex_cli <TARGET_PATH> --stats
```

This wraps the command with `/usr/bin/time -l` and reports:
- Maximum Resident Set Size (RSS)
- Peak Memory Footprint
- Execution Time

## Before/after comparison workflow

When the user has made a change and wants to measure impact:

1. **Get baseline** — run on the current main/branch before changes:
   ```bash
   samply record rust/target/profiling/rubydex_cli <TARGET_PATH> --stats 2>&1 | tee /tmp/rubydex-baseline.txt
   ```
   Save the samply profile URL from the browser (Firefox Profiler allows sharing via permalink).

2. **Apply changes** and rebuild:
   ```bash
   cargo build --profile profiling
   ```

3. **Get new measurement**:
   ```bash
   samply record rust/target/profiling/rubydex_cli <TARGET_PATH> --stats 2>&1 | tee /tmp/rubydex-after.txt
   ```

4. **Compare** — parse both output files and show a side-by-side delta of:
   - Total time and per-phase breakdown (listing, indexing, resolution, querying)
   - Memory (RSS)
   - Declaration/definition counts (sanity check that output is equivalent)

Present the comparison as a formatted table showing absolute values and % change.

### Quick benchmark (no flamegraph)

When the user just wants timing/memory numbers without the full profiler overhead:

```bash
# Release build (faster than profiling profile since no debug symbols)
cargo build --release
utils/bench                    # uses DEFAULT_BENCH_WORKSPACE
utils/bench medium             # synthetic corpus
utils/bench /path/to/project   # specific directory
```

## Timing phases (--stats output)

The `--stats` flag on rubydex_cli prints a timing breakdown using the internal timer system.
The phases are:

| Phase | What it measures |
|-------|-----------------|
| Initialization | Setup and configuration |
| Listing | File discovery (walking directories, filtering .rb files) |
| Indexing | Parsing Ruby files and extracting definitions/references |
| Resolution | Computing fully qualified names, resolving constants, linearizing ancestors |
| Integrity check | Validating graph consistency (optional) |
| Querying | Building query indices |
| Cleanup | Time not accounted for by other phases |

It also prints:
- Maximum RSS in bytes and MB
- Declaration/definition counts and breakdown by kind
- Orphan rate (definitions not linked to declarations)

## Troubleshooting

### samply permission errors on macOS

samply uses the `dtrace` backend on macOS which may need elevated permissions. If you get
permission errors:

```bash
sudo samply record rust/target/profiling/rubydex_cli <TARGET_PATH> --stats
```

Or grant Terminal/iTerm the "Developer Tools" permission in System Settings > Privacy & Security.

### Empty or unhelpful flamegraphs

If the flamegraph shows mostly `[unknown]` frames:
- Make sure you built with `--profile profiling` (not `--release`)
- Verify debug symbols: `dsymutil -s rust/target/profiling/rubydex_cli | head -20` should
  show symbol entries.
- On macOS, ensure `strip = false` in the profiling profile

### Comparing runs with variance

Indexer performance can vary ±5% between runs due to OS scheduling, file system caching, etc.
For reliable comparisons, run 3 times and take the median, or at minimum run twice and check
consistency before drawing conclusions.
