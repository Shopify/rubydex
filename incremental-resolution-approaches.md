# Incremental Resolution Approaches

## Approach 1: Diff-Based (Snapshot Comparison)

### Overview

Capture a snapshot of document state before and after an update, diff them to determine what changed, then find affected references using indexes.

### Data Captured in Snapshot

For each document:
- **Members**: `Map<DeclarationId, Vec<(StringId, Position)>>` - what members each declaration contributes (ordered by position)
- **Mixins**: `Map<DeclarationId, Vec<(StringId, kind)>>` - mixin declarations (order matters)
- **Superclasses**: `Map<DeclarationId, StringId>` - superclass for each class declaration
- **Alias targets**: `Map<DeclarationId, StringId>` - what each alias points to
- **Reference IDs**: all constant references in the document
- **Declaration IDs**: all declarations this document contributes definitions to

### Diff Computation

Compare old vs new snapshot to find:
1. **Member changes**: members added/removed from declarations
2. **Ancestor changes**: mixins/superclasses changed (including reordering)
3. **Alias changes**: alias targets changed
4. **Reference changes**: references added/removed

### Finding Affected References

Uses two indexes:
- `nesting_to_references[decl_id]` - refs with decl_id anywhere in lexical nesting chain
- `descendants[decl_id]` - declarations that inherit from decl_id

When member changes in declaration X:
1. Collect refs from `nesting_to_references[X]`
2. Collect refs from `nesting_to_references[D]` for each descendant D of X
3. Filter to refs whose name matches the changed member

### Position-Dependent Resolution

In Ruby, a constant is only visible after it's defined in source order:

```ruby
module Foo
  puts BAR  # resolves to ::BAR
  BAR = 2   # Foo::BAR defined after reference
end
```
vs
```ruby
module Foo
  BAR = 2   # Foo::BAR defined before reference
  puts BAR  # now resolves to Foo::BAR
end
```

To handle this, use ordered member tracking: store members as `Vec<(StringId, Position)>` instead of `Set<StringId>`. When the ordered list differs (including order changes), treat it the same as an add/remove and invalidate references to that name. This is conservative (may over-invalidate when definitions reorder) but correct.

### Limitations

1. **Over-invalidation**: May re-resolve references that weren't actually affected because we check "could be affected" not "was affected". In practice this is acceptable — re-resolving a subset of references is still much faster than full codebase re-resolution.

2. **Alias chains**: When alias target changes, finding all references that resolved *through* that alias requires either tracking resolution paths or conservatively re-resolving many references.

### Flow

1. Capture old snapshot (before update)
2. Update graph (remove old definitions/references, add new ones)
3. Process definitions (creates/updates declarations)
4. Capture new snapshot (after definitions processed)
5. Diff snapshots to find affected references
6. Re-resolve affected references

Note: New snapshot must be captured *after* definition processing because we need declarations to exist to know which declaration each definition contributes to.

---

## Approach 2: Dependency Tracking

### Overview

Track the exact resolution path for each reference. When anything in that path changes, invalidate precisely those references.

### Dependencies to Track

For each resolved reference, record:
- **Negative dependencies**: `Vec<(DeclarationId, StringId)>` - "looked here for this name, not found"
- **Ancestor dependencies**: `Vec<DeclarationId>` - "depended on this declaration's ancestor chain"
- **Positive dependency**: `Option<DeclarationId>` - "resolved to this declaration"

### Example

```ruby
module Outer
  FOO = "outer"

  module Inner
    include Mixin
    FOO  # reference
  end
end

module Mixin
  FOO = "mixin"
end
```

Resolution of `FOO` in `Inner`:
1. Check Inner.members["FOO"] → not found (negative dep)
2. Check Inner.ancestors → [Mixin] (ancestor dep on Inner)
3. Check Mixin.members["FOO"] → found! (positive dep)

Dependencies:
```
negative: [(Inner, "FOO")]
ancestors_of: [Inner]
resolved_to: Mixin
```

### Invalidation

| Change | Re-resolve? |
|--------|-------------|
| Add FOO to Inner | Yes (negative dep violated) |
| Remove FOO from Mixin | Yes (positive dep violated) |
| Change Inner's ancestors | Yes (ancestor dep) |
| Add FOO to Outer | No (never checked there) |

### Advantages

- More precise than diff-based (no over-invalidation)
- Handles "didn't look there" cases correctly

### Challenges

- Storage cost for dependencies per reference
- Index maintenance when references are re-resolved
- Complexity of tracking negative dependencies at each lookup step

### Implementation Locations

- Record dependencies during resolution in `resolution.rs`
- Store on `ConstantReference` or as separate index on `Graph`
- Query index when determining affected references

---

## Comparison

| Aspect | Diff-Based | Dependency Tracking |
|--------|------------|---------------------|
| Precision | May over-invalidate | Precise |
| Storage | Snapshots (temporary) | Per-reference deps (persistent) |
| Complexity | Simpler | More complex |
| Position-aware | Yes (with ordered members) | Could be added |
| Maintenance | Rebuild indexes on resolve | Update deps on each resolve |

---

## Implementation (Diff-Based Approach)

### Implemented Components

Located in `model/snapshot.rs`:

- `DocumentSnapshot` — captures ordered members, mixins, superclasses, alias targets, reference IDs
- `DocumentSnapshot::capture()` — builds snapshot from graph for a given URI
- `DocumentSnapshot::diff()` — compares two snapshots, returns `SnapshotDiff`
- `find_affected_references()` — uses diff + graph's nesting index to find refs needing re-resolution
- `compute_affected_references()` — convenience function combining diff + find

Located in `model/graph.rs`:

- `Graph::incremental_update()` — single method that performs the full incremental update flow
- `Graph::nesting_to_references` — persistent index mapping declarations to nested references
- `Graph::rebuild_nesting_index()` — rebuilds the index (called automatically after resolution)
- `Graph::remove_reference_from_nesting_index()` — updates index when documents change

Located in `resolution.rs`:

- `Resolver::ensure_builtins()` — creates Object, Module, Class if they don't exist
- `Resolver::resolve_definitions()` — processes specific definitions to create/update declarations
- `Resolver::resolve_references()` — re-resolves a specific set of references

### Usage Example

Simple usage with the integrated method:

```rust
// Incrementally update a single document
graph.incremental_update(new_local_graph);
```

Or manually for more control:

```rust
use crate::model::snapshot::{DocumentSnapshot, compute_affected_references};
use crate::resolution::Resolver;

let uri_id = new_local_graph.uri_id();
let old_snapshot = DocumentSnapshot::capture(&graph, uri_id);
let new_def_ids: Vec<_> = new_local_graph.definitions().keys().copied().collect();

graph.update(new_local_graph);

{
    let mut resolver = Resolver::new(&mut graph);
    resolver.resolve_definitions(&new_def_ids);
}

let new_snapshot = DocumentSnapshot::capture(&graph, uri_id);
let affected = compute_affected_references(&graph, &old_snapshot, &new_snapshot);

let mut resolver = Resolver::new(&mut graph);
resolver.resolve_references(&affected);
```
