# Unifying resolution under a single fixed-point loop

Status: proposal. Describes how to move from "constants in the loop, everything else after" to "every entity is solved
by the loop", so that a future type system can add entities and dependencies without new phases.

## 1. What exists today

`Resolver::resolve` (`rust/rubydex/src/resolution.rs`) has three stages:

| Stage | Handles | Retry mechanism |
|-------|---------|-----------------|
| `prepare_units` | Drains `pending_work`, dedups, sorts by `(name depth, uri, offset)`, splits "others" out | n/a |
| Loop over `unit_queue` | `Definition` (class, module, constant, alias, singleton class, `def self.x`), `ConstantRef`, `Ancestors` | Push to back of queue; stop when a full pass sets no `made_progress` |
| `handle_remaining_definitions` + `resolve_method_visibilities` | Methods, attrs, ivars, cvars, gvars, method/gvar aliases, constant and method visibility | None in this run. `graph.push_work` and hope the *next* `resolve()` call succeeds |

The scheduler knows three things about a unit: `Resolved`, `Unresolved` (definitive), or `Retry { partial_ancestors }`.
It never learns *what* a unit is waiting for, so every pass re-runs every stuck unit and progress is one global bool.

### Dependencies are encoded as phase order, not as dependencies

The split rests on the claim that post-loop items have no inverse dependencies. The code already contradicts it:

- `def self.foo` was moved into the loop because it creates singleton classes that need linearization.
- `def Foo.bar`, ivars, and `private_class_method` still create singletons and promote constants to `Todo` namespaces
  *after* the loop, hence `SingletonAncestors::Eager` ("only valid after the convergence loop").
- Method visibility needs all methods declared first, so it is a hand-rolled second pass inside the post-loop stage.
- `resolve_lexical_owner` walks up the nesting when an enclosing definition has no declaration yet, because an attr
  can be processed before its enclosing method.
- `ConstantVisibility` with an unresolved receiver is dropped with `continue` and never re-queued.
- `relinearize_singleton_hierarchy` clears and re-enqueues chains inside the loop: a fact is retracted by hand
  because nothing records that the singleton chain depended on the class's definitions.
- The depth sort is a correctness requirement, not an optimization: it stands in for "this name depends on that name".

### Absence is concluded before it is known

Constant lookup reads "scope `S` has no member `X`" while definitions that could add `X` are still pending. This is
wrong today (pinned by `resolving_reference_to_member_declared_through_alias` in `resolution_tests.rs`; the reference
resolves to `::X` instead of `A::X`):

```ruby
X = 1
module A
  X                    # resolved in pass 1 through the Object fallback
end
module Holder
  SELF = ::A
end
B = Holder::SELF
class B::X; end        # blocked on the alias in pass 1, declares A::X in pass 2
```

The same shape appears in the speculative `Object` lookup when a chain is still partial, and in eager `Todo` creation
for unknown parent scopes. Putting methods, and later types, in the loop multiplies these cases, so the loop needs a
rule for negative conclusions rather than more ordering.

## 2. The desired algorithm and where it exists

The target is a **worklist fixed point with dependency-directed wake-up and deferred negation**:

1. A unit runs. It either produces facts or reports the fact it is blocked on.
2. Producing a fact wakes exactly the units blocked on it.
3. When nothing is runnable (quiescence), the remaining blockers can never be satisfied positively. Only then are
   negative conclusions drawn (undefined constant, missing method, create `Todo`, break a cycle), which may produce
   facts and restart step 1.

| Project | What it does | What to take |
|---------|--------------|--------------|
| Sorbet `resolver.cc` (`resolveConstants`) | Separate todo lists for constants, ancestors, class aliases, type aliases; loops while any list shrinks. Failed jobs are *marked*, never stubbed, until the loop ends (stubbing early made results non-deterministic, sorbet#1126). Mixes-in-class-methods and required ancestors run after the loop | Same shape as Rubydex today, including the post-loop tail. Confirms the "no negative conclusions inside the loop" rule. Their comment on ordering: "an optimization, should not matter semantically" |
| mypy semantic analyzer (`semanal_main.py`) | Any node may `defer()` when a name is incomplete; namespaces are tracked as incomplete; iterates until no progress, then runs a `final_iteration` where remaining placeholders become errors | Closest match to step 3. Every kind of node goes through the same loop |
| rustc_resolve and rust-analyzer `DefCollector` | Imports and macro expansion iterate to a fixed point with `Determined` / `Undetermined` results; errors are reported in a finalize step | Three-valued outcome with an explicit "not yet knowable" |
| Datalog engines (Soufflé, CodeQL) | Semi-naive evaluation: rules re-fire only on new facts. Negation is only allowed over a completed lower stratum | The theory behind steps 2 and 3 |
| Build systems (Shake's suspending scheduler) | A task that hits an unbuilt dependency suspends and is resumed when it is built | Step 1 and 2 as a scheduler design |
| Salsa (ty, rust-analyzer), pyrefly, TypeScript checker | Demand-driven memoized queries; cycles get a provisional value and the SCC is iterated until stable | Cycle handling for types. The pull model itself fits poorly: Ruby declarations are open, so "members of `Foo`" has no known set of inputs until every definition name is resolved. Rubydex needs push |

No project puts *everything* in one push-based loop with precise wake-up; Sorbet and mypy stop at blind retry. That is
a reasonable first milestone here too.

## 3. Target shape

`Unit` barely changes: `Unit::Definition` already covers every definition kind. What changes is the outcome and the
scheduler.

```rust
enum Outcome {
    Done,            // facts were written to the graph
    Blocked(Dep),    // cannot proceed until this fact exists
    Failed,          // definitive; only reachable from the quiescence step
}

enum Dep {
    Name(NameId),                     // name resolved to a declaration
    Declared(DefinitionId),           // definition has a declaration (lexical owner, self receiver)
    AliasTarget(DeclarationId),       // alias has a resolved target
    Ancestors(DeclarationId),         // chain is complete
    Member(DeclarationId, StringId),  // member exists in the namespace or its chain
}

struct Scheduler {
    ready: VecDeque<Unit>,
    waiting: HashMap<Dep, Vec<Unit>>,
}
```

- Facts are produced in few places, all already centralized: `record_resolved_name`, `add_declaration`, `add_member`,
  `set_ancestors`, `record_resolved_reference`. Each wakes `waiting[dep]`.
- Loop ends when `ready` is empty. `made_progress` and the pass structure disappear.
- Quiescence step: re-run the still-waiting units in a `final` mode where a missing fact is definitive. Order by
  kind so that lower-level absences settle first (names, then ancestors, then members). If it wrote any fact, resume
  the loop.
- What is still waiting at the end is persisted instead of dumped into `pending_work`. The next `resolve()` wakes only
  units whose `Dep` was produced by the edit, instead of retrying every unresolved reference on every run.
- New entity kinds (method references, inferred return types) add a `Unit` variant, the facts it writes and the
  `Dep`s it can block on. No new phase. Cyclic type dependencies surface as a stuck cycle at quiescence, broken with
  a provisional value and re-run until stable, as Salsa and pyrefly do.

## 4. How to get there

Each step is shippable and keeps the test suite green.

1. **Pin order independence.** Add a test helper that resolves the same fixture with the unit order shuffled and
   asserts identical graphs. Run it over existing fixtures. Failures are the list of hidden ordering assumptions
   (the example in section 1 is one). This is the safety net for everything below. `GraphTest::resolve` does this for
   every fixture (`rust/rubydex/src/test_utils/order_independence.rs`); fixtures that still depend on the order call
   `GraphTest::order_dependent` with the first level that changes them and the reason, which is the list to burn down.
2. **Move the tail into the loop, keeping blind retry.** `prepare_units` stops returning `others`;
   `handle_definition_unit` dispatches every definition kind and returns an `Outcome`. Every
   `graph.push_work(..); continue` becomes `Retry`. Delete `SingletonAncestors::Eager`, the second visibility pass
   and the walk-up workaround in `resolve_lexical_owner`. Keep the current sort and place member definitions after
   constant references: now purely an optimization so most units succeed first try. As shipped, member definitions
   already conclude a missing owner or visibility target (drop or diagnostic) only in a final pass that runs after a
   pass without progress; references and ancestors skip that pass and conclude as before. `resolve_lexical_owner`
   still walks past methods and past a definition that reopened a non-namespace (`FOO = 1; class FOO; end`), which is
   error recovery rather than ordering.
3. **Add the quiescence step.** When a pass makes no progress, run the stuck units once with `final = true`. Move
   every negative conclusion behind that flag: undefined-visibility diagnostics, `Todo` creation for unknown
   parents, the `Object` fallback for incomplete chains, `Outcome::Unresolved`. Steps 2 and 3 together are
   mypy's design.
4. **Replace blind retry with `Blocked(Dep)`.** Convert `Retry` sites one `Dep` kind at a time (each already knows
   its blocker; `partial_ancestors` is a one-bit version of it). Introduce `waiting` and the wake-ups, then delete
   `made_progress`.
5. **Make retractions generic.** Lookups that read absence in a scope that later gains a member, and chains that
   change when a declaration gains a definition, need the dependents re-run. The graph already has this logic for
   incremental updates (`invalidate_graph`, `name_dependents`). Call it from the loop when a fact changes, and
   delete `relinearize_singleton_hierarchy`. Add a per-unit re-run cap that emits a diagnostic, since Ruby allows
   programs with no stable answer.
6. **Persist `waiting` across `resolve()` calls** and drop stale entries the way `prepare_units` drops stale units.
7. Update `docs/architecture.md`: the "single convergence point" pillar becomes "all solving", and the paragraph
   about a separate step for items without inverse dependencies goes away.

### Risks to measure

- **Throughput.** Methods are most of the definitions. They must stay a single visit each; steps 2 and 4 should be
  benchmarked with `utils/bench` before and after.
- **Memory.** `waiting` only holds blocked units, so it is small. Recording dependencies for *resolved* units (full
  Salsa-style tracking) would be large; step 5 avoids it by reusing the existing reverse indexes.
- **Step 5 is the hard one.** The alternative to retraction is refusing to read a scope until it is closed, but a
  scope is never provably closed while any `class X::Y` is still blocked. Retraction with a cap is the practical
  choice, and incremental invalidation already requires it to be correct.
