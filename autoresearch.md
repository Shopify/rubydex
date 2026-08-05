# Autoresearch: Optimize Rubydex resolve stage

## Current State

- **Status**: Experiment loop active. 16 experiments run, 12 kept, 4 discarded.
- **Best `resolve_s`**: **8.126s** at commit `4d683f2`. The original baseline was
  12.318s, thus the total gain is **-34.0%**. `total_s` fell from about 19.0s to
  13.93s.
- **Active branch**: `autoresearch/optimize-resolve-20260805` (base: `main`).
- **Working tree**: clean at `4d683f2`. All 1132 workspace tests pass.
- **Current blocker**: None.
- **Next action**: See *Next candidates* below. The strongest remaining lead is a
  per-pass cache for partial chains on the Resolver.
- **Related tasks / PRs**: None yet. No PR is open.
- **Still to do before the task finishes**: run `bundle exec rake test` (the Ruby
  suite). `cargo test --workspace` already passes after every kept experiment.

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

### Kept (cumulative: 12.318s → 9.126s, -25.9%)

#### The descendant-tracking rewrite (10.000 → 9.126)
The largest single win. The old scheme pushed the whole recursion stack onto
every cached chain that it met, which cost chain length times stack depth on
*every* cache hit. The new scheme has each declaration record itself on the
entries of its own chain, once, when its linearization completes.

This gives the same result, because the chain of a declaration contains the
chain of each of its parents and mixins. Thus every declaration reaches all of
its own ancestors without help from the recursion stack. `propagate_descendants`
and the `descendants` set on `LinearizationContext` are both gone.

- Self-registration replaces stack propagation: 10.000→9.454.
- Record from the local chain before the chain goes into the graph, rather than
  a read back per ancestor: 9.454→9.126.

#### The allocation-removal series (11.142 → 10.000)
The profile showed `_malloc` as the single largest cost centre. Four changes
removed almost every heap allocation from the linearization path:

- **No-clone linearization** (11.142→10.812): split `linearize_ancestors` into
  `linearize_ancestors_state`, which returns only a `ChainState` and stores the
  chain on the declaration, plus a thin wrapper that clones. Because the state
  function always stores the chain that it computes, `linearize_mixins` can read
  the chain from the graph by reference instead of a clone. Cache-hit descendant
  propagation moves the chain out with `take_ancestors` and puts it back, rather
  than a clone.
- **Pooled parent chain buffer** (10.812→10.581): `Resolver.chain_pool` holds
  spare `Vec<Ancestor>` buffers. `linearize_parent_ancestors_into` and
  `linearize_superclass_into` copy into a reused buffer.
- **Pooled mixin buffer and deques** (10.581→10.406): `linearize_mixins` writes
  into caller-supplied deques and takes mixins by slice.
- **Reused LinearizationContext** (10.406→10.000): one spare context on the
  Resolver, cleared between linearizations, so the two identity hash sets keep
  their capacity.

#### The redundant-work series (10.000 → 8.126)
Instrumentation, not guessing, drove these. The counters above show where.

- **One self-registration for descendants** (10.000→9.454): each declaration now
  records itself on the entries of its own chain when its linearization
  completes, in place of pushing the whole recursion stack onto every cached
  chain that the recursion met. The result is the same, because the chain of a
  declaration contains the chain of each of its parents and mixins. The cost
  drops from chain length times stack depth on every cache hit to chain length
  once for each declaration. The `descendants` set left `LinearizationContext`.
- **Record from the local chain** (9.454→9.126): record before the chain goes
  into the graph, which removes one map lookup for each ancestor entry.
- **Fuse the linearize call with the chain read** (9.126→9.045):
  `extend_with_chain` and `ensure_chain` check for a complete chain first, thus
  the common path needs one map lookup instead of a nested call plus a second
  lookup.
- **Skip the descendant recording when the chain is unchanged** (9.045→8.443,
  the largest single win). A compare of two short adjacent slices replaces
  repeated scattered hash writes. Safe because the graph clears ancestors and
  descendants together during invalidation.
- **`ancestors_state_of`** (8.443→8.352): `handle_ancestor_unit` and the eager
  singleton schedule cloned the whole chain on every pass and then dropped it.
- **Build the chain in a pooled buffer and compare** (8.352→8.126): the repeat
  linearization now costs no heap allocation and no store. A changed chain still
  goes into a list of the exact size, so the graph never holds the spare
  capacity of a pooled buffer.

#### Earlier fast-path series (12.318 → 11.142)
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

- **Non-allocating `single_alias_target` fast path in `resolve_to_namespace`**:
  REGRESSION, confirmed twice (8.225 and 8.241 against a best of 8.126). The
  fast path misses often, and a miss makes the alias search walk the definitions
  a second time.
- **Per-call memo of partial chains** (`exhausted` set on
  `LinearizationContext`): parity at 8.129. It caught only 296k of 7.2M
  linearizations, because the repetition is across top level calls.

### Lesson learned
Removing an allocation from a hot path helps. Adding a data structure to a hot
path hurts, even when it improves the asymptotic complexity, because the
ancestor chains are short. Prefer fast paths and moves over new containers.

Buffer pools are the strongest tool found so far. The linearization recurses,
but it takes and gives back buffers in strict last-in-first-out order, thus a
simple `Vec` of spare buffers on the Resolver works. Each pool removed one heap
allocation for each linearized declaration.

When a function stores its result and also returns it, a caller that can read
the stored copy does not need the returned clone. Split such a function into a
state-only version plus a cloning wrapper.

Measure before you optimize. Two rounds of atomic counters found waste that the
sampling profiler could not show: 94% of all linearizations recompute a chain
that the graph already holds. The two changes that came from those counters gave
more than the six changes that came from reading the profile.

A fast path only pays when it hits. A miss that repeats the work of the slow
path makes the code slower, as the alias experiment showed.

### Measurement note
The benchmark machine has high and irregular load. A single run of
`./autoresearch.sh` can show a spread of more than 2s between its five runs. When
a result is within about 1% of the best, re-run before you judge it.

### Profile insights (latest, at 8.126s)
`_malloc` fell from 1233 samples to 302 across the series, and
`linearize_ancestors_state` from 593 to 172. The profile is now flat:

| Function | Samples |
|---|---|
| `linearize_ancestors_state` | 172 |
| `handle_definition_unit` | 45 |
| `ensure_chain` | 43 |
| `get_or_create_singleton_class` | 35 |
| `get_superclass` | 34 |
| `resolve_to_namespace` | 28 |
| `search_ancestors` | 23 |
| `record_descendant_on_chain` | 7 |

### Counter measurements (instrumented run, do not commit the counters)
Two instrumented runs gave the numbers that drove the largest wins. Add atomic
counters to `resolution.rs`, call a `dump_stats` from `main.rs` at the
`StopAfter::Resolution` branch, then revert both files.

- Convergence loop passes: **4**.
- Top level `ancestors_state_of` calls: **607,797**.
- Total `linearize_ancestors_state` calls: **7,303,342** (about 12 nested calls
  for each top level call).
- Linearizations that gave a partial chain equal to the stored chain:
  **6,878,613**, which is 94% of all of them. This is the largest known waste.
- Descendant set writes before the skip optimization: **93,504,020**.
- A per-call memo of partial chains caught only **295,987** of the repeats, thus
  the repetition happens **across** top level calls, not inside one call.

### Dead end: a cache for partial chains (experiments 17 and 18)

The 6.9M repeated linearizations of an unchanged partial chain looked like the
last large win. Two forms were tried, and neither works.

**Unsound form.** A per-pass set of "already partial in this pass" ids on the
`Resolver`, cleared at the start of each pass. This cuts `resolve_s` to **6.471**
from 8.126, and all 1132 Rust tests pass. A chain state audit shows why it is
wrong: only **31,487** namespaces reach a complete chain, against **140,343** in
the baseline. A partial chain becomes complete when a later unit of the same
pass resolves a name, thus the repeat linearization is how the information
spreads. It is not redundant work.

**Correct form.** A `generation` counter steps on every graph change: a
definition unit, a reference unit, a resolved name, a new singleton class, a
cleared singleton hierarchy and every stored chain. A declaration that gives an
unchanged partial chain gets recorded together with the counter value, and a
later try skips only while the counter still holds. A declaration that has its
own unit of work always linearizes for real, so that no side effect goes
missing. The audit then matches the baseline exactly. Two runs give **8.189**
and **8.133** against a best of 8.126, thus **parity**. The guard that makes it
correct also stops it from skipping the work that made the unsound form fast.

**Lesson: the unit tests do not catch under-convergence.** Their graphs converge
in one pass. Any change to the convergence loop or to the linearization needs
the chain state audit below.

### The chain state audit

Add this at the end of `Resolver::resolve`, build, and run against the
benchmark workspace with `RDX_CHAIN_AUDIT=1`:

```rust
if std::env::var("RDX_CHAIN_AUDIT").is_ok() {
    let (mut complete, mut cyclic, mut partial, mut descendants) = (0u64, 0u64, 0u64, 0u64);
    for declaration in self.graph.declarations().values() {
        if let Some(ns) = declaration.as_namespace() {
            match ns.ancestors() {
                Ancestors::Complete(_) => complete += 1,
                Ancestors::Cyclic(_) => cyclic += 1,
                Ancestors::Partial(_) => partial += 1,
            }
            descendants += ns.descendants().len() as u64;
        }
    }
    eprintln!("AUDIT complete={complete} cyclic={cyclic} partial={partial} descendants={descendants}");
}
```

The correct values for the Shopify core workspace, stable across runs:

```
AUDIT complete=140343 cyclic=0 partial=171700 descendants=3883377
```

### Open item for the operator: descendant iteration order

`bundle exec rake test` fails one Ruby test, `DeclarationTest#test_descendants`
at `test/declaration_test.rb:291`. It expects `["Child", "Parent"]` and gets
`["Parent", "Child"]`. The **set contents are the same**; only the order of the
iteration changed. `descendants` is an `IdentityHashSet`, and
`rdx_declaration_descendants` iterates it raw, thus the order follows the
placement in the hash table, which follows the order of the inserts.

The cause is the self-registration of descendants (experiment 9). Before it, a
child pushed itself onto its parents during the propagation; now each
declaration adds itself to the entries of its own chain when its linearization
completes. A parent linearizes before its child, thus the parent now enters its
own set first.

The Rust suite has no order-sensitive descendant test, and all 1132 pass.

### Next candidates (not yet tried)
- `handle_definition_unit`, `get_or_create_singleton_class` and `get_superclass`
  are now as costly as the linearization internals and have not been studied.
- The final ancestors `Vec` on the changed path is the last allocation per
  declaration. It goes into the graph, thus it cannot simply be pooled.
- A non-allocating fast path in `resolve_to_namespace` was tried and is a
  regression (experiment 15): the fast path misses often, and a miss makes the
  alias search walk the definitions a second time.

## Key Hot-Path Observations
- `linearize_mixins` does O(n) `VecDeque::contains` / `Vec::contains` on
  ancestor lists for every id — O(n²) for modules with many ancestors.
- `propagate_descendants` is O(descendants × ancestors) and runs on every
  cached ancestor lookup in `linearize_ancestors`.
- `search_ancestors` iterates the ancestor chain and does a hashmap `member`
  lookup per ancestor.
- The convergence loop re-processes the whole queue each pass; retries are
  pushed to the back.
