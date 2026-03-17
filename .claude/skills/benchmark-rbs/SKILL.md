---
name: benchmark-rbs
description: >
  Benchmark Rubydex indexing performance on RBS core/stdlib, comparing the current branch
  against main. Measures maximum RSS and execution time.
---

# Benchmark RBS Indexing

Compare Rubydex indexing performance between `main` and the current topic branch using
the RBS core and stdlib directories.

## Target paths

Clone the `https://github.com/ruby/rbs.git` repository under the repository root and use
the following paths as benchmark targets:

- `tmp/rbs/core`
- `tmp/rbs/stdlib`

The `tmp/` directory is at the rubydex repository root. Create it if it doesn't exist.

## Steps

### 0. Record branch names and commit SHAs

Before building, record the current branch name and commit SHA, and the main branch SHA.
These will be used in the results table, e.g. `main (abc1234)` and `my-branch (def5678)`.

```bash
TOPIC_BRANCH=$(git branch --show-current)
TOPIC_SHA=$(git rev-parse --short HEAD)
MAIN_SHA=$(git rev-parse --short main)
```

Also record the RBS repository branch and SHA after cloning:

```bash
RBS_BRANCH=$(git -C tmp/rbs branch --show-current)
RBS_SHA=$(git -C tmp/rbs rev-parse --short HEAD)
```

Print the RBS version in the results, e.g. `rbs: master (abc1234)`.

### 1. Build the current branch (release)

```bash
cargo build --release
cp rust/target/release/rubydex_cli tmp/rubydex_cli_branch
```

### 2. Build main (release)

```bash
git stash
git checkout main
cargo build --release
cp rust/target/release/rubydex_cli tmp/rubydex_cli_main
git checkout -
git stash pop
```

### 3. Run benchmarks

Run each binary 3 times and take the median. Use `utils/mem-use` for memory and time
measurement. Pass both `core` and `stdlib` together as a single benchmark run.

```bash
# Main
utils/mem-use tmp/rubydex_cli_main tmp/rbs/core tmp/rbs/stdlib --stats

# Branch
utils/mem-use tmp/rubydex_cli_branch tmp/rbs/core tmp/rbs/stdlib --stats
```

### 4. Compare results

Present a table comparing:

| Branch | Max RSS (MB) | Indexing (s) | Resolution (s) |
|--------|-------------|-------------|----------------|
| main (MAIN_SHA)           | ...  | ...  | ...  |
| TOPIC_BRANCH (TOPIC_SHA)  | ...  | ...  | ...  |

Include the delta (absolute and %) for RSS and each time metric.

## Metrics

- **Max RSS**: Maximum Resident Set Size — the peak physical memory used by the process,
  reported by `/usr/bin/time -l`.
- **Indexing time**: Time spent in the indexing phase (parsing files and extracting
  definitions/references), reported by the `--stats` flag's internal timer.
- **Resolution time**: Time spent in the resolution phase (computing fully qualified names,
  resolving constants, linearizing ancestors), reported by the `--stats` flag's internal timer.

## Notes

- Run each measurement 3 times and use the median to account for variance (±5% is normal).
- Make sure no other heavy processes are running during benchmarks.
