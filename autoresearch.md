# Autoresearch: Rubydex Memory Optimization (Session 2)

## Objective
Reduce peak memory usage (RSS) of rubydex when indexing a large Ruby codebase.
Resolution algorithm optimization is out of scope (PR #654). Focus on data structure
memory: struct sizes, heap allocations, collection overhead, and string storage.

## Workload
- Target: `/Users/hung-wulo/world/trees/root/src/areas/core/shopify/`
- 96,238 Ruby files
- 945,412 declarations, 1,063,171 definitions
- Current baseline on main: **3397 MB RSS** (peak footprint 4114 MB)

## Metrics
- **Primary**: `rss_mb` (MB, lower is better) — Maximum Resident Set Size
- **Secondary**: `peak_footprint_mb`, `total_time_s`, `resolution_time_s`, `indexing_time_s`,
  `declaration_count`, `definition_count`

## How to Run
`./autoresearch.sh` — builds profiling binary, runs 3x, outputs median METRIC lines.

## Files in Scope
- `rubydex/src/model/graph.rs` — Graph struct with 8 IdentityHashMaps
- `rubydex/src/model/declaration.rs` — Declaration/Namespace structs (224B namespace, 112B simple)
- `rubydex/src/model/definitions.rs` — Definition enum + 14 boxed definition structs (72-144B)
- `rubydex/src/model/references.rs` — ConstantReference (24B) and MethodRef (40B)
- `rubydex/src/model/name.rs` — Name (48B), NameRef (Box<Name>/Box<ResolvedName>)
- `rubydex/src/model/string_ref.rs` — StringRef: String + ref_count
- `rubydex/src/model/document.rs` — Document: URI + LineIndex + Vecs + diagnostic maps
- `rubydex/src/model/identity_maps.rs` — IdentityHashMap/IdentityHashSet
- `rubydex/src/model/comment.rs` — Comment: Offset + String
- `rubydex/src/model/id.rs` — Id<T> (8 bytes, u64)
- `rubydex/src/indexing.rs` — Parallel indexing + LocalGraph merge
- `rubydex/src/indexing/local_graph.rs` — Per-file LocalGraph
- `rubydex/src/resolution.rs` — Resolution pass
- `rubydex/src/main.rs` — CLI entry point with --stats
- `rubydex/src/stats/memory.rs` — RSS measurement

## Off Limits
- Resolution algorithm (PR #654 handles this)
- Public API / FFI layer (rubydex-sys/)
- rubydex-mcp crate
- Do NOT remove name_dependents reverse index (needed for incremental invalidation)
- Do NOT remove LineIndex from Document (needed for LSP)

## Constraints
- `cargo test -p rubydex` must pass
- Declaration count: 945,412; Definition count: 1,063,171 — must stay identical
- Orphan rate: 0.0% (13 InstanceVariable orphans expected)
- Total wall-clock time must not regress more than 5%
- Update `assert_mem_size!` macros when changing struct layouts

## Previous Session Results (from autoresearch session 1)
Experiments done on a prior branch. Only #5 is merged into current main:
1. shrink_to_fit on HashMaps — DISCARDED (no-op, variance masked results)
2. NonZeroU64 for Id<T> — **NOT MERGED**, saved ~500 MB
3. SortedVecMap for method_references — **NOT MERGED**, saved ~271 MB
4. Vec<ReferenceId> instead of IdentityHashSet in declarations — **NOT MERGED**
5. Concurrent LocalGraph merging — **MERGED** (commit fdc87cb)
6. SortedVecMap for constant_references — DISCARDED (24% resolution time regression)
7. Pre-allocate HashMap capacity — DISCARDED (no improvement)

## What's Been Tried (This Session)
(Updated as experiments accumulate)
