# Autoresearch: Optimize Rubydex resolve stage

## Current State

- **Status**: Experiment loop active. 11 experiments run, 5 kept, 2 discarded.
- **Best `resolve_s`**: **11.142s** (commit `5f61461`); latest run 11.167s is the
  same within noise. Original baseline was 12.318s, so the total gain is ~9.5%.
- **Active branch**: `autoresearch/optimize-resolve-20260805`
  (base: `main`).
- **Current blocker**: None.
- **Next action**: Finish the `ChainState` refactor described in *Work In
  Progress* below, then benchmark it.
- **Related tasks / PRs**: None yet. No PR is open.

### Work In Progress (committed, not yet wired up)

Groundwork for a no-clone linearization path is committed but **not yet used**:

- `Ancestors::as_slice()` in `model/declaration.rs`.
- `NamespaceStore::take_ancestors()` and the `Declaration::take_ancestors()`
  forwarder, which move the chain out and leave an empty chain behind. This lets
  `propagate_descendants` run against the chain while the graph is mutably
  borrowed, with no clone.
- `ChainState` enum plus `ChainState::of` and `ChainState::record` in
  `resolution.rs`.

Remaining steps to complete the optimization:

1. Split `linearize_ancestors` into `linearize_ancestors_state(&mut self, id,
   context) -> ChainState`, which stores the chain on the declaration and
   returns only the state. Keep `linearize_ancestors` as a thin wrapper that
   calls the state function and then clones the chain back out of the graph.
   This is safe because `linearize_ancestors` **always** stores exactly the
   chain it returns, on all three paths (cache hit, cycle estimate, fresh
   linearization).
2. In the state function, replace `set_ancestors(result.clone())` with a move,
   and use `take_ancestors` + restore around `propagate_descendants` on the
   cache-hit path.
3. Change `linearize_mixins` to call `linearize_ancestors_state`, then read the
   chain from the graph with `ancestors().as_slice()`. This removes one full
   chain clone per mixin.
4. **Ordering caution** for the include branch: the original code runs
   `ids.retain(...)` before it pushes, so the dedup compares against the deque
   contents from before the push. When iterating a slice in reverse and pushing
   as you go, keep a `pushed` counter and compare only against
   `linearized_includes.range(pushed..)` to keep the same result.

## Objective
Reduce the wall-clock time of the Rubydex **resolution** stage when indexing the
Shopify core monolith at `/Users/dersam/world/trees/root/src/areas/core/shopify`.

Resolution computes fully qualified names, creates declarations, resolves
constant references, and linearizes ancestor chains. It is the slowest stage
(~11.5s of ~17.4s total on the target).

## Metrics
- **Primary**: `resolve_s` (seconds, lower is better) — the `Resolution` line in
  the Timer breakdown.
- **Secondary**: `total_s` — total wall clock from `/usr/bin/time -p real`.

## How to Run
`./autoresearch.sh` — outputs `METRIC name=resolve_s value=...` and
`METRIC name=total_s value=...` lines.

The script temporarily rewrites the target repo's legacy `rubydex.toml`
(top-level `exclude`) into the current `[graph] exclude` form and restores it
on exit. It does not modify rubydex source.

## Files in Scope
- `rust/rubydex/src/resolution.rs` — the Resolver: convergence loop, constant
  resolution, ancestor linearization.
- `rust/rubydex/src/model/declaration.rs` — Declaration/NamespaceStore: members,
  ancestors, descendants storage.
- `rust/rubydex/src/model/graph.rs` — Graph: work queue, record_resolved_*.
- `rust/rubydex/src/model/name.rs` — Name/NameRef/ParentScope.
- Other files under `rust/rubydex/src/` as needed for hot paths.

## Off Limits
- The benchmark target directory
  `/Users/dersam/world/trees/root/src/areas/core/shopify` — do not modify its
  source. The script only rewrites `rubydex.toml` temporarily and restores it.
- No new dependencies (Cargo.toml additions).
- No test changes that weaken correctness.

## Constraints
- All tests must pass: `cargo test` (workspace) and `bundle exec rake test`.
- No new dependencies.
- Code-only optimizations; behavior must be preserved.

## What's Been Tried

### Kept (cumulative: 12.318s → 11.142s, -9.5%)
- **resolve_alias_chains fast path**: return `vec![declaration_id]` at once for
  non-alias declarations, with no `VecDeque` or `HashSet` allocation. 11.142→
  11.167 (parity within noise; the run baseline had drifted to 11.57).
- **Skip redundant propagate_descendants on cache hit** when only descendant is self (already propagated during first linearization). Also preallocate ancestors Vec. 11.737→11.568.
- **Fast paths in handle_ancestor_unit and schedule_singleton_ancestors::Eager**: skip ancestors_of clone when ancestors already complete. 11.568→11.494.
- **search_ancestors fast path**: read cached ancestors by reference without clone or LinearizationContext allocation when chain is complete. 11.494→11.309.
- **get_superclass Vec elimination**: track only first superclass in Option instead of collecting into Vec. 11.309→11.142.

### Discarded
- **Arc<Vec<Ancestor>> for O(1) clones**: REGRESSION (12.802). Arc atomic refcount overhead + make_mut clones in mutation paths (linearize_mixins, linearize_superclass) outweigh clone savings for small chains.
- **HashSet dedup in `linearize_mixins`**: REGRESSION (13.785 against a best of
  11.142). Replacing the O(n) `VecDeque::contains` scans with
  `HashSet<DeclarationId>` sets costs more than it saves, because the sets must
  be allocated on every call and the chains are short in practice.

### Lesson learned
Removing an allocation from a hot path helps. Adding a data structure to a hot
path hurts, even when it improves the asymptotic complexity, because the
ancestor chains are short. Prefer fast paths and moves over new containers.

### Profile insights
- linearize_ancestors dominates (55%+ of resolution time). Heavy allocation from ancestor Vec clones and growth.
- get_superclass, resolve_alias_chains, resolve_to_namespace are secondary hot spots.
- Machine has high variance (load avg 7.4); min-of-5 runs used for stability.

## Key Hot-Path Observations
- `linearize_mixins` does O(n) `VecDeque::contains` / `Vec::contains` on
  ancestor lists for every id — O(n²) for modules with many ancestors.
- `propagate_descendants` is O(descendants × ancestors) and runs on every
  cached ancestor lookup in `linearize_ancestors`.
- `search_ancestors` iterates the ancestor chain and does a hashmap `member`
  lookup per ancestor.
- The convergence loop re-processes the whole queue each pass; retries are
  pushed to the back.
