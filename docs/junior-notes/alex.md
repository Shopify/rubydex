# Alex's Learning Notes -- Saturn/Rubydex Codebase Review

> Notes from exploring the Rust side of the Saturn/Rubydex codebase -- a high-performance code indexer and static analysis toolkit for large-scale Ruby projects. I read through the core model types, the indexer, the resolution engine, the parallel job queue, and the CLI entry point.

---

## Architecture & Pipeline

### Q: Why does the resolution phase use a fixed-point loop instead of a single pass?

**Dr. Chen:**

Ruby allows **forward references** and **circular dependencies** between constants, and the order files are indexed is arbitrary. A single pass can't handle this. Consider:

```ruby
# file_a.rb (indexed first)
module Foo
  class Bar::Baz; end   # needs Bar resolved first
end

# file_b.rb (indexed second)
module Foo
  class Bar; end         # defines Bar, but wasn't available when file_a was processed
end
```

When processing `Bar::Baz`, the resolver needs to know what `Bar` resolves to (`Foo::Bar`), but that declaration might not exist yet. So it returns `Outcome::Retry` and pushes the unit back onto the queue. The three `Outcome` variants map to this logic:
- **`Resolved`**: Found the answer. Mark progress.
- **`Unresolved`**: Had everything needed but the name doesn't exist -- a genuine error (missing constant), not an ordering issue.
- **`Retry`**: Missing a dependency that might be resolved in a future pass. Push it back.

The `made_progress` flag is the termination condition -- if an entire pass resolves nothing new, everything remaining is either genuinely unresolvable or stuck in a circular dependency.

The sorting by `name_depth()` in `prepare_units()` (resolution.rs:1389) is an optimization to reduce retries. Simple names like `Foo` (depth 1) never depend on other names, so they resolve first. Complex names like `Foo::Bar::Baz` (depth 3) go last because they need the simpler parts resolved first. This means the first pass often resolves most names.

Ancestor linearization adds another layer: mixins (`include`, `prepend`) reference constants that may not be resolved yet. If `include SomeModule` can't resolve `SomeModule`, the ancestor chain is `Partial`, and any class inheriting from this one also gets partial. The `Unit::Ancestors` work items handle re-linearization as more constants resolve.

### Q: Why is there a distinction between Definition and Declaration?

**Dr. Chen:**

This exists because **Ruby classes and modules can be reopened** -- one of Ruby's most fundamental features, used extensively in real codebases. A **Definition** is the per-file, per-occurrence view (syntax-level). A **Declaration** is the global, semantic view.

Here's a realistic Rails example:

```ruby
# app/models/user.rb
class User < ApplicationRecord
  def full_name; "#{first_name} #{last_name}"; end
end

# app/models/concerns/user/authentication.rb
class User
  def authenticate(password); end
end

# config/initializers/user_patches.rb
class User
  def admin?; role == "admin"; end
end
```

There are **3 definitions** (one per file) but only **1 declaration** for `User`. The declaration aggregates all definitions -- `declaration.definitions()` returns all 3 IDs, and `declaration.members()` includes `full_name`, `authenticate`, and `admin?`. Each definition retains its own source location, comments, and file URI.

You can't merge them at indexing time because indexing is parallel (each file on its own thread, no knowledge of other files). The merge happens in `graph.rs:81-99` at `add_declaration`: if a declaration already exists for that fully qualified name, the new definition ID is added to the existing one. If not, a new declaration is created.

Other real-world cases: gems monkey-patching stdlib classes (`class String; def blank?; end; end`), test files reopening classes, and Rails engines extending application classes.

### Q: How does constant resolution handle Ruby's complex scoping rules?

**Dr. Chen:**

The `Name` struct in `name.rs` has three fields that together encode everything needed for constant resolution:
- `str: StringId` -- the unqualified name (e.g., "Bar" for both `Foo::Bar` and `::Bar`)
- `parent_scope: ParentScope` -- what comes before `::` in qualified references
- `nesting: Option<NameId>` -- the lexical scope where this name was written

Here's how three different Ruby expressions get represented:

**`::Foo::Bar` (top-level qualified)** creates TWO Name entries:
1. `Name { str: "Foo", parent_scope: TopLevel, nesting: <current scope> }` -- TopLevel means "start from root, ignore lexical scope"
2. `Name { str: "Bar", parent_scope: Some(name_id_of_foo), nesting: <scope> }` -- look for Bar inside whatever Foo resolves to

**`Foo::Bar` (relative qualified)** is almost identical, except:
1. `Name { str: "Foo", parent_scope: None, nesting: Some(enclosing_scope) }` -- None means "no explicit prefix, use the full resolution algorithm"
2. `Name { str: "Bar", parent_scope: Some(name_id_of_foo), nesting: <scope> }`

The difference is in how `Foo` resolves. With `parent_scope: None`, resolution goes through `run_resolution()` (resolution.rs:1158), which searches in order: (1) lexical scopes -- walks up the nesting chain, (2) inheritance chain -- checks ancestors, (3) top-level -- checks Object's members.

**`Bar` (unqualified)** gets: `Name { str: "Bar", parent_scope: None, nesting: Some(enclosing_scope) }` -- same three-phase search.

The **nesting linked list** is key. For `module A; module B; class C; Foo; end; end; end`, the nesting chain is `C -> B -> A -> None (top level)`. When resolving `Foo`, `search_lexical_scopes()` walks this chain checking `A::B::C::Foo`, then `A::B::Foo`, then `A::Foo`. This matches exactly how Ruby's constant lookup works at runtime -- lexical scope first, then inheritance, then top-level.

**`ParentScope::Attached`** is specifically for singleton class names. `class << self` inside `class Foo` triggers `get_or_create_singleton_class()`, creating the declaration `Foo::<Foo>`. The Attached variant means "this is the singleton class of whatever the attached name resolves to."

### Q: What are "orphan" definitions and what causes them?

**Dr. Chen:**

An orphan is a definition that was indexed (found in source code) but never "claimed" by any declaration during resolution. The stats code at `graph.rs:807-833` computes this by collecting all definition IDs referenced by declarations, then finding any definition not in that set.

Concrete causes (with code locations):

1. **Unresolvable constant definitions** (the biggest source) -- e.g. `class UnknownParent::MyClass; end` where `UnknownParent` can't be resolved. The definition gets `Outcome::Unresolved` at resolution.rs:207-209 and is simply dropped.
2. **Top-level instance variables** -- `@top_level_var = 1` is explicitly skipped at resolution.rs:337-339 because there's no `<main>` representation yet.
3. **Methods with unresolvable receivers** -- `def UnknownClass.my_method; end` is skipped at resolution.rs:278-285 because the system doesn't know which class to attach it to.
4. **Top-level class variables** -- `@@cvar = 1` (a RuntimeError in Ruby) produces an orphan because `resolve_class_variable_owner()` returns `None`.

Some orphan rate is **normal and expected** in large codebases: dynamically-defined classes (`Struct.new`, `Class.new`), monkey-patching of external gems, DSL-heavy code (Rails routes, RSpec). But a high orphan rate is a red flag. The breakdown by definition type is a diagnostic tool -- high method orphans might indicate a receiver resolution bug, high instance variable orphans might point to the `<main>` gap. It's essentially a **health metric for the analyzer itself**: "how much of the codebase can we understand statically?"

### Q: How does the ancestor linearization handle Ruby's MRO?

**Asked: Dr. Chen**

The `linearize_ancestors` method in `resolution.rs` builds the ancestor chain in this order:
1. Prepended modules (from `prepend`)
2. The declaration itself
3. Included modules (from `include`)
4. Parent class ancestors (recursively)

This matches Ruby's actual method resolution order. The three states -- `Complete`, `Partial`, and `Cyclic` -- handle edge cases:
- **Partial**: when a mixin references a constant that hasn't been resolved yet (e.g., `include SomeModule` where `SomeModule` isn't resolved). The system retries in the next pass.
- **Cyclic**: when a module includes itself (directly or indirectly). The system breaks the cycle and estimates ancestors (classes get `Object`, modules get empty).
- **Complete**: everything resolved successfully.

The name-depth sorting in `prepare_units` is an optimization, not strictly required for correctness, but it dramatically reduces the number of retries by resolving leaf names first.

---

## Rust Patterns & Code Quality

### Q: Why do all the ID types use PhantomData and marker types instead of plain u64?

**Morgan:**

This is the **"newtype via generics"** or **"phantom type tagging"** pattern. With 6 different ID types (`DefinitionId`, `DeclarationId`, `UriId`, `StringId`, `ReferenceId`, `NameId`) being passed around constantly, a single mix-up would be silent, hard-to-debug data corruption. Without PhantomData, accidentally passing a definition ID where a declaration ID is expected compiles fine and silently does the wrong thing. With marker types, the compiler catches it immediately.

The beauty is that `PhantomData` is a **zero-sized type** (ZST) -- `assert_mem_size!(DefinitionId, 8)` confirms `Id<T>` is exactly 8 bytes (just the u64). All the type safety is purely compile-time with zero runtime cost. This is a very common Rust pattern seen in `std::num::NonZeroU64`, database ORMs, and any system with multiple identifiers of the same underlying type.

### Q: Why use a custom IdentityHasher instead of the standard HashMap hasher?

**Morgan:**

The standard HashMap uses SipHash, which would process the 8 bytes through multiple rounds of mixing on every lookup. But our IDs are already the output of xxhash3 (`id.rs:47`), a high-quality hash function. Running SipHash on top of xxhash3 output is pure waste -- you're hashing a hash. The distribution won't improve (it's already uniform), but you pay ~5-10ns per lookup for the SipHash computation.

The IdentityHasher at `identity_maps.rs:14-30` is a passthrough: the `Hash` impl calls `state.write_u64(self.value)`, and the hasher just stores that value directly as the bucket index. With millions of entries across 7 `IdentityHashMap` instances (see `graph.rs:24-39`) and every resolution step hitting these maps, the savings add up.

The `unreachable!()` at line 16 is a safety valve -- it panics if anyone tries to use `write(&[u8])` instead of `write_u64`, catching misuse of IDs that didn't go through xxhash3.

### Q: Why is Definition an enum wrapping Box<T> for each variant?

**Morgan:**

Morgan laid out a detailed comparison of three approaches:

**A) Current: Enum + Box** -- 16 bytes per Definition (tag + pointer). Preserves exhaustive pattern matching, each variant has completely different fields (no wasted space), no vtable overhead. One heap allocation per definition.

**B) Trait objects (`Box<dyn DefinitionTrait>`)** -- also 16 bytes (data + vtable pointer), but you lose exhaustive matching. Adding a new definition type wouldn't produce compile errors at existing code. You'd rely on runtime `downcast()` calls and pay virtual dispatch overhead.

**C) One big struct with all fields** -- enormous size because every definition pays for every field via `Option<>`. A `GlobalVariableDefinition` would carry empty slots for `parameters`, `mixins`, `superclass_ref`, etc. No compile-time enforcement of which fields are valid for which kind.

The enum + Box approach wins because: (1) the different definition types genuinely have different data, (2) without Box the enum would be 144 bytes (size of largest variant `ClassDefinition`), and (3) the compiler-enforced exhaustive matching means adding a new variant produces errors everywhere it needs handling.

### Q: Is the all_definitions! macro pattern fragile when adding new variants?

**Morgan:**

This is called a **"dispatch macro"** -- a common Rust pattern seen in `serde`, `syn`, and other major projects. The macro expands to a `match` expression, and Rust's `match` is **exhaustive**. If you add `Definition::NewThing(Box<NewThingDefinition>)`, every call site of `all_definitions!` would immediately produce a compile error ("non-exhaustive pattern"). So it's not fragile -- the compiler enforces correctness.

Where it IS slightly fragile: if you add the variant to the macro but don't implement the expected method (like `id()`) on the new type, you'd get a different error ("method `id` not found"), but you'd still get an error -- it wouldn't silently compile.

The macro is essentially a DRY tool. Without it, `Definition::id()`, `uri_id()`, `offset()`, `comments()`, `lexical_nesting_id()`, and `is_deprecated()` would each need 14 match arms -- that's 84 identical match arms. The macro reduces them to 6 one-liners.

### Q: Why does StringRef implement its own reference counting instead of using Rc or Arc?

**Morgan:**

The key insight is that `Rc`/`Arc` and Saturn's `StringRef` solve **fundamentally different problems**. `Rc` tracks "how many live pointers exist to this heap allocation." Saturn's ref count tracks "how many documents reference this string" -- a domain-level concept, not a memory management one.

In Saturn's architecture, nobody holds a pointer to the `StringRef`. Everything stores a `StringId` (a u64 hash) and does O(1) lookups in the `strings` HashMap. There is exactly ONE owner: the `Graph.strings` map. So there's nothing to `Rc::clone()`.

The lifecycle illustrates this: when file A and file B both use string "foo", the merge at `graph.rs:535-545` does a single `increment_ref_count(string_ref.ref_count())` -- one addition that accounts for all uses from an entire file. With `Rc`, you'd need N separate `Rc::clone()` calls. When file A is deleted, `untrack_string` at `graph.rs:395-401` decrements the count; at zero the string is evicted.

Additional reasons `Rc`/`Arc` wouldn't work:
- **`Rc` is `!Send`** -- indexing uses multiple threads via `JobQueue`, so `Rc` literally can't cross thread boundaries. `Arc` would work but uses expensive atomic operations, which are unnecessary since all ref count manipulation happens on the main thread during merge/cleanup.
- **Batch increment** -- Saturn can add a whole file's worth of references in one addition. `Rc` would need N separate clones.
- **Eviction control** -- Saturn needs explicit cleanup when documents change (`remove_definitions_for_uri`). `Rc` handles this automatically but the pointers would be scattered across definition structs, making the cleanup harder to reason about.

Morgan called this pattern **"arena-style deduplication with explicit reference counting"** or "interning with manual lifecycle tracking" -- common in compilers where you have a central store of interned values, everything refers by ID, and you need precise eviction control. The same pattern is used for `NameRef` in `name.rs`.

### Q: Is treating module_function as a Visibility variant a design smell?

**Asked: Morgan**

In `visibility.rs`, `ModuleFunction` is listed alongside `Public`, `Protected`, and `Private`. In Ruby, `module_function` isn't really a visibility -- it creates TWO method copies (public singleton + private instance). The indexer handles this specially at `ruby_indexer.rs:1442` by creating two definitions.

Modeling it as a visibility is a pragmatic shortcut. It reuses the existing visibility stack mechanism in the indexer (push/pop on scope entry/exit). The alternative would be a separate concept, but since `module_function` follows the same syntactic pattern as `private`/`public` in Ruby code (it's a method-level modifier), treating it as a visibility variant keeps the indexer logic simple.

---

## System Design & Scalability

### Q: Why use a custom crossbeam work-stealing queue instead of Rayon?

**Jordan:**

The key difference is that this `JobQueue` needs to support **dynamically spawned work** -- jobs that create more jobs while running. In `listing.rs:89-95`, when a `FileDiscoveryJob` encounters a subdirectory, it pushes a new job onto the same queue. This is a recursive tree walk where work is discovered as you go.

The `in_flight` atomic counter is specifically designed for this -- it tracks how many jobs have been enqueued but not yet completed, so workers know when ALL work (including dynamically spawned children) is done.

The custom queue also gives control over: (1) **no global thread pool** -- Rayon uses a global pool by default, while this creates fresh threads per pipeline stage, avoiding contention between listing and indexing, (2) **channel integration** -- jobs send results through `crossbeam_channel` senders, which fits naturally in the crossbeam ecosystem.

Jordan noted that Rayon could absolutely work here -- it's a different set of tradeoffs. The custom approach is more explicit and gives full control.

### Q: Why mem::forget(graph) at the end of main.rs?

**Jordan:**

For Rails-scale codebases, the `Graph` contains 7 `IdentityHashMap`s with ~100K+ definitions, ~50K+ declarations (each with `Vec` members, ancestors, etc.), ~200K+ strings, ~100K+ names, plus references. When Rust drops a `HashMap`, it iterates every entry and drops each value, recursively -- a deep deallocation tree. The OS, on the other hand, reclaims the entire address space in one operation when the process exits.

So `mem::forget` turns O(n) deallocation into O(1), potentially saving hundreds of milliseconds.

**Important caveat** from Jordan: this trick is ONLY appropriate for CLI tools that are about to exit. You would NOT do this in a library, LSP server, or anywhere the process continues running -- that would be an actual memory leak.

### Q: Is the sequential LocalGraph-to-Graph merge a bottleneck after parallel indexing?

**Jordan:**

The merge at `Graph::extend()` (graph.rs:527-576) is all hash map insertions with pre-hashed u64 keys (identity hasher), so each insertion is essentially O(1). The merge of one `LocalGraph` is proportional to the definitions in that single file -- typically 10s to low 100s of entries.

The expensive part is parsing (Prism AST walking + definition extraction) -- that's what parallelism targets. The channel also acts as a buffer: while one `LocalGraph` is being merged, other threads are still parsing and queuing results.

Jordan noted it COULD become a bottleneck with millions of tiny files where parse time approaches merge overhead, but Ruby files are large enough that parsing dominates. If needed, a tree-reduce pattern (merge pairs of LocalGraphs in parallel) could help, but it adds complexity for likely-negligible gain.

### Q: Why are the three pipeline stages (Listing, Indexing, Resolution) strictly sequential?

**Jordan:**

Resolution fundamentally needs ALL definitions present because of Ruby's open class semantics. Consider `class Foo < Bar; end` in file_a.rb and `class Bar; end` in file_b.rb -- if we try to resolve `Foo`'s superclass before file_b is indexed, we'd incorrectly conclude `Bar` doesn't exist. The fixpoint loop handles inter-definition dependencies (where resolving one constant depends on another being resolved first), but it assumes all definitions are present.

Jordan made an interesting point about **partial resolution**: in theory, you could resolve definitions that don't depend on anything else (like top-level `module Foo; end`) as soon as they're indexed. But tracking "which definitions can safely resolve now" would likely cost more than it saves, since resolution is already fast relative to indexing.

For **LSP server usage** (where you don't want to re-resolve the entire graph on every file change), the path forward is incremental resolution (only re-resolve affected declarations), not overlapping stages. The infrastructure is partially in place -- `Graph::update()` already handles single-file re-indexing by removing old data and extending with new.

### Q: How does the system handle hash collisions in release builds?

**Asked: Jordan**

All IDs (DefinitionId, DeclarationId, NameId, etc.) are 64-bit xxhash values. In `graph.rs:extend()`, collisions are detected with `debug_assert!` -- which is stripped in release builds.

With 64-bit hashes, the birthday paradox gives a 50% collision probability at about 5 billion items. A large Ruby codebase like Shopify's might have tens of millions of definitions, so the actual collision probability is extremely low (roughly 1 in 10^7 for 10M items).

In practice, this is an acceptable risk for the performance benefit of using hash-based IDs rather than, say, sequential integers with a global counter (which would require synchronization across threads). If a collision did occur in production, it would silently corrupt the graph -- but the probability is so low that it's a reasonable engineering tradeoff.

---

## Summary Observations

### Things I found most clever:
1. **The IdentityHasher optimization** -- since IDs are already hashed, skipping double-hashing is a zero-cost performance win
2. **PhantomData marker types for IDs** -- compile-time type safety with zero runtime cost
3. **Sorting work units by name depth** -- simple optimization that dramatically reduces the fixed-point loop iterations
4. **The LocalGraph-per-file design** -- enables embarrassingly parallel indexing with no locks
5. **`mem::forget(graph)` at exit** -- acknowledging that correct Drop behavior doesn't matter when the process is dying
6. **Compile-time size assertions** (`assert_mem_size!`) -- catches memory regression from struct changes at compile time, not at runtime
7. **The `module_function` dual-definition pattern** -- correctly models Ruby's subtle semantics where one keyword creates two methods

### Things I found most confusing:
1. **The `ParentScope::Attached` variant** -- the singleton class naming convention (`Foo::<Foo>::<<Foo>>`) took a while to understand
2. **The ancestor linearization with cycle detection** -- the interaction between `Partial`, `Complete`, `Cyclic`, and the `descendants` set is complex
3. **The difference between `NameId` and `StringId`** -- both represent "names" but `NameId` includes scoping context (parent_scope + nesting) while `StringId` is just an interned raw string
4. **Why resolution clears ALL declarations and rebuilds** (`clear_declarations()` at the top of `resolve_all`) -- this seems wasteful for incremental updates, but there's a TODO noting it's temporary
5. **The `Nesting` enum's three variants** (LexicalScope, Owner, Method) -- the distinction between "scopes that can own things" and "scopes that just provide nesting context" is subtle

### My understanding of the system (in simple terms):
Saturn/Rubydex is a static analysis engine that reads an entire Ruby codebase and builds an in-memory graph of "what exists where." It works in three phases:

1. **Listing**: Walk the filesystem in parallel to find all `.rb` files
2. **Indexing**: Parse each file in parallel to extract "definitions" -- classes, modules, methods, constants, variables, and references to constants/methods. Each file produces a self-contained `LocalGraph` that gets merged into the global `Graph`.
3. **Resolution**: Process all definitions and references to figure out fully qualified names (is `Bar` inside `Foo` actually `Foo::Bar` or `::Bar`?), group multiple definitions of the same thing into single "declarations," resolve constant references to their targets, and compute the inheritance hierarchy (ancestor chains).

The graph then supports queries like "find all definitions of X", "what are the ancestors of Y", "where is Z referenced" -- which power IDE features like go-to-definition, find-references, and code navigation. The Rust implementation is designed for maximum performance on huge codebases (like Shopify's Rails monolith), exposed to Ruby through a C FFI layer.
