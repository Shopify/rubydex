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

### Experiment 1: NonZeroU64 for Id<T> (DISCARDED)
- Same as previous session's exp 2. On current main (post-#666 concurrent merging),
  struct shrinkage only saves ~80 MB (2.3%) because peak working set is smaller.
- Not worth the unsafe transmute in Deref for such modest gain.

### Experiment 2: Memory breakdown instrumentation (KEPT)
- Added `print_memory_breakdown()` to Graph — reveals per-collection heap usage.
- Key finding: method_references (686 MB) + constant_references (462 MB) = 1.15 GB = 47% of memory.
- name_dependents (400 MB), declarations (427 MB) are next largest.

### Experiment 3: Vec for method_references (KEPT)
- Replaced `IdentityHashMap<ReferenceId, MethodRef>` with `Vec<(ReferenceId, MethodRef)>`.
- Method references are bulk-loaded during indexing, never used in resolution.
- Lazy-sorted via `ensure_method_references_sorted()` on first query access.
- HashMap (14.7M capacity × 49 bytes) → Vec (9.0M × 48 bytes) = ~230 MB saved estimated.
- RSS dropped ~700-900 MB due to avoiding HashMap growth overhead during merging.

### Experiment 4: Graph::compact() after indexing (KEPT)
- Calls `shrink_to_fit()` on name_dependents Vecs and method_references Vec.
- name_dependents: 106 MB wasted capacity → 0 MB.
- method_references: ~46 MB wasted → 0 MB.
- ~150 MB total savings.

### Experiment 5: Vec<ReferenceId> in declarations (KEPT)
- Replaced IdentityHashSet<ReferenceId> with Vec<ReferenceId> in all declarations.
- No dedup needed (each ref resolves to exactly one declaration).
- swap_remove for incremental updates.
- Namespace: 224 → 216, Simple: 112 → 104.
- References heap: 98 → 58 MB estimated (-40 MB).

## Current Summary
| Metric | Baseline | Current | Change |
|--------|----------|---------|--------|
| Est. heap total | 2430 MB | 2009 MB | **-421 MB (-17%)** |
| RSS (single run) | ~3500 MB | ~2750 MB | **~-750 MB (-21%)** |
| Peak footprint | ~4200 MB | ~3700 MB | **~-500 MB (-12%)** |
| Declarations | 945,412 | 945,412 | identical |
| Definitions | 1,063,171 | 1,063,171 | identical |

## Next targets (from memory breakdown)
1. constant_references: 462 MB — hot during resolution, can't use Vec easily
2. method_references: 410 MB — already Vec, near minimum
3. declarations total: 387 MB (box structs 128 MB, name strings 62 MB, refs 58 MB, ancestors 38 MB, desc 28 MB)
4. name_dependents: 295 MB (table 115 MB, vecs 179 MB)
5. names: 196 MB (table 88 MB, box heap 108 MB)
6. definitions: 182 MB (table 44 MB, box structs 107 MB, comments 13 MB)
