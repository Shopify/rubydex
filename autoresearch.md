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
(baseline pending)

## Key Hot-Path Observations
- `linearize_mixins` does O(n) `VecDeque::contains` / `Vec::contains` on
  ancestor lists for every id — O(n²) for modules with many ancestors.
- `propagate_descendants` is O(descendants × ancestors) and runs on every
  cached ancestor lookup in `linearize_ancestors`.
- `search_ancestors` iterates the ancestor chain and does a hashmap `member`
  lookup per ancestor.
- The convergence loop re-processes the whole queue each pass; retries are
  pushed to the back.
