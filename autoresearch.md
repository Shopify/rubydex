# Autoresearch: Optimize Rubydex resolve stage

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
- **Skip redundant propagate_descendants on cache hit** when only descendant is self (already propagated during first linearization). Also preallocate ancestors Vec. 11.737→11.568.
- **Fast paths in handle_ancestor_unit and schedule_singleton_ancestors::Eager**: skip ancestors_of clone when ancestors already complete. 11.568→11.494.
- **search_ancestors fast path**: read cached ancestors by reference without clone or LinearizationContext allocation when chain is complete. 11.494→11.309.
- **get_superclass Vec elimination**: track only first superclass in Option instead of collecting into Vec. 11.309→11.142.

### Discarded
- **Arc<Vec<Ancestor>> for O(1) clones**: REGRESSION (12.802). Arc atomic refcount overhead + make_mut clones in mutation paths (linearize_mixins, linearize_superclass) outweigh clone savings for small chains.

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
