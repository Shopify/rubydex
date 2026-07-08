# Architecture

Rubydex analyzes Ruby codebases in two distinct phases: **Indexing** and **Resolution**. Understanding this separation
is crucial for working with the codebase.

## Phases diagram

```

                          | workspace path
                          |
                          ▼
            ┌─────────────────────────────┐
            |           Listing           |  Traverse directories and list files in parallel
            └─────────────────────────────┘
                          | document list
                          │
                          ▼
    ┌───────────────── Indexing ─────────────────┐
    |       ┌─────────────────────────────┐      |
    |       |          Indexing           |      | Documents are indexed in parallel
    |       └─────────────────────────────┘      | Each document produces a single local graph
    |                     |                      |
    |        local graphs stream in as parsed    |
    |                     ▼                      |
    |       ┌─────────────────────────────┐      |
    |       |         Graph merge         |      | Main thread keeps merging local graphs into
    |       └─────────────────────────────┘      | the global one
    └────────────────────────────────────────────┘
                          | definitions + references pending resolution
                          | on initial boot, this is all of them
                          ▼
    ┌──────────────── Resolution ────────────────┐
    |       ┌─────────────────────────────┐      |
    |       |        Order worklist       |      |
    |       |     based on dependencies   |      |
    |       └─────────────────────────────┘      |
    |                     | Unit queue           |
    |                     |                      |
    |                     ▼                      |
    |       ┌─────────────────────────────┐      |
    |       |    Fixed-point resolution   |      |
    |       |            loop             |      |
    |       └─────────────────────────────┘      |
    └────────────────────────────────────────────┘
                          |
                          |
                          ▼
              Global graph with semantic
              representation of the codebase
```

## Core model

Rubydex represents Ruby code through a graph, which is essentially a collection of nodes (entities) and edges
(relationships). For example, if a class `Foo` inherits from class `Bar`, we have two entities related to each other
through inheritance.

All top level nodes and edges are stored in hashmaps in `rust/rubydex/src/model/graph.rs`. Note that certain nodes also
store edges within their structs (e.g.: `rust/rubydex/src/model/declaration.rs`,
`rust/rubydex/src/model/definitions.rs`).

A **Definition** represents a single source-level construct found at a specific location in the code. It captures
exactly what was found in the file, with no attempts to derive semantic meaning from the data. Definitions are produced
during indexing.

A **Declaration** represents the global semantic concept of an entity, combining all definitions that contribute to the
same fully qualified name. Declarations are produced during resolution.

A **Document** represents a file whether it is committed to disk or not. Documents are produced during indexing.

Consider this example:

```ruby
# foo.rb
module Foo # 1
  class Bar # 2
  end
end

# other_foos.rb
class Foo::Bar # 3
end
class Foo::Bar # 4
end
```

Indexing extracts 4 definitions:

- module Foo (1 in foo.rb)
- class Bar (2 foo.rb)
- class Foo::Bar (3 in other_foos.rb)
- class Foo::Bar (4 in other_foos.rb)

Which produce 2 declarations during resolution: `Foo` and `Foo::Bar`. All graph edges are represented using IDs (defined
in `rust/rubydex/src/model/ids.rs`), which are deterministic hashes derived from node characteristics wrapper in
distinct types to provide higher type safety. For example, `DeclarationId` and `UriId` are distinct types, which
internally store a u64 number, and each is derived by hashing the declaration's fully qualified name and a URI,
respectively. Despite the underlying number being the same type, it is intentionally not possible to pass a
`DeclarationId` where a `UriId` is expected.

The graph can be traversed from the bottom (documents) or from the top (declarations). For example, if a consumer wants
to discover all declarations that are defined in a document, they enter the graph from the bottom by using a UriId
(derived from a string URI). If a consumer wants to access data about a specific declaration directly, they enter the
graph through the top using a DeclarationId (derived from the fully qualified name).

```
    ┌─────────────┐
    | Declaration |  one per fully qualified name
    └─────────────┘
           ▲
           |   many definitions that contribute to the same fully qualified name -> one declaration
           ▼
    ┌─────────────┐
    | Definition  |   a single source-level construct
    └─────────────┘
           ▲
           |   one document -> the many definitions found inside it
           ▼
    ┌─────────────┐
    |  Document   | a file
    └─────────────┘
```

### References

The Rubydex graph also tracks references, which are a URI + offsets combination in the code where we found a usage of a
certain declaration. In constant references, there's an important distinction between a reference and a name (explained
below), which is essentially what makes each data structure unique.

```ruby
class Foo; end

Foo
Foo
```

In the example above, there are 2 constant references to the class declaration `Foo`, since it is used 2 times. However,
the constant name being referred to is the same in both cases. Constant references are unique based on their URI +
offsets combination (i.e.: where they appeared in the code), names are unique based on their lexical scope structure.

### Names

Names are unique constant name structures that connect all of the pieces required to resolve them against the graph
(implemented in `rust/rubydex/src/model/name.rs`). They contain the unqualified name of the constant (interned string),
an optional parent scope (an ID to another name) and an optional nesting (an ID to another name). It is the connection
of the 3 pieces that allows resolution to determine what a constant refers to.

For example:

```ruby
# Foo: Name(str: StringId(Foo), parent_scope: None, nesting: None)
module Foo
  # Bar: Name(str: StringId(Bar), parent_scope: None, nesting: NameId(Foo))
  # Qux: Name(str: StringId(Qux), parent_scope: NameId(Bar), nesting: NameId(Foo))
  class Bar::Qux
  end
end
```

## Phases

### Indexing

Indexing traverses ASTs that might be coming from different languages (Ruby, RBS) and extracts the information exactly
as found, without attempting to derive semantic meaning from it. Consider the following example:

```ruby
# foo.rb
module Foo
end

# bar.rb
module Bar
  class Foo::Qux
  end
end
```

The files `foo.rb` and `bar.rb` can be indexed in arbitrary order, as it's not possible to statically predict Ruby's
file loading order due to `autoload` or requires that depend on runtime behavior (like loops or conditionals). Because
of this, we wouldn't know what the `Foo` constant reference in `Foo::Qux` points to when `bar.rb` is indexed. If we
consider that there are no other files being analyzed:

- If `foo.rb` is indexed first, then the `Foo` reference resolves to top level module `Foo`
- If `bar.rb` is indexed first, then the `Foo` reference is undefined and we don't know where `Qux` is being defined

The key insight regarding this initial phase is that we can only capture document-specific data and any understanding
that requires cross-file knowledge can only be derived once indexing is complete.

### Resolution

Resolution combines all of the data discovered during indexing to build a semantic representation of the codebase. The
full resolution implementation is in `rust/rubydex/src/resolution.rs`.

- Compute fully qualified names and create declarations for all definitions (e.g.: `Foo::Bar`, `Foo#method()`,
  `Bar#@ivar`)
- Resolve constant references to their target declarations
- Linearize ancestor chains and track descendants
- Assign semantic ownership (which methods/constants belong to which class)
- Create implicit singleton classes

The resolution algorithm is implemented as a worklist (unit queue) loop with multiple passes. Since constants in Ruby
have inter-dependencies with lexical scopes and ancestors, we first order all of the work based on fewest to highest
number of dependencies.

Each pass goes through the entire worklist once and remembers whether we made some progress, which means either
successfully resolving a constant or linearizing an ancestor chain. Making progress means that we solved some
dependencies, which might unblock completing more work. If progress was made, we continue iterating in the next pass.

If the loop ends with no progress, then we have converged to a fixed-point, which exits the loop. Any unresolved
constants are stored as pending work, so that the next round of resolution can attempt to resolve them again (for
example, if a user edits a file). A constant being left as pending work means it is undefined from the perspective of
the analysis.

The purpose of the loop is to solve all of the work that has inverse dependencies. For example, resolving a constant may
depend on another constant being resolved. After we're done, we have a separate step for processing definitions that
have no inverse dependencies (like instance variables).

#### Important design and implementation note

When making changes to resolution, it is critical to understand if they fit the overall approach of the algorithm. The
design is intended to iterate through the worklist until we converge to maximum constant resolution and, after we solved
as many dependencies as possible, we move on to processing items that have no inverse dependencies (for example, no
resolution depends on a method definition or an instance variable being processed).

The worklist is populated from pending work left from previous iterations. On the first resolution, everything is
pending, and future passes operate on the smallest possible subset of work that needs to be resolved again.

When making changes, we have to ensure that it is following the natural path of the algorithm. Avoid treating symptoms
and uncover the root cause of why something is not supported or not working. Here are the main pillars that should not
be violated:

- Single source of truth: the unit queue (worklist) and pending work (feeds worklist) dictate the items that must be
  processed as part of the fixed-point loop. Introducing competing data structures that track similar state is a smell
  that the implementation is not fitting the algorithm.
- Single convergence point: all constant related resolution and dependency solving must happen inside of the resolution
  loop. If a change requires moving constant related solving out of the loop, it's a smell that the change does not fit.
- Minimal work: resolution always consumes the least amount of work that needs to be solved. If a change is
  requiring a full graph traversal, then it's a smell that it does not fit the algorithm.

## Incremental invalidation

Rubydex aims to provide extremely fast incremental analysis. When a document is changed, we want to determine the
minimum set of graph state that needs to be invalidated and re-analyzed. The invalidation algorithm lives in
`rust/rubydex/src/model/graph.rs` and it produces the pending work that feeds the resolution pass.

In its ideal form, Rubydex should be able to perform layered incremental invalidation by asking layered questions to
decide which level of invalidation is needed.

Let's say we have:

```rb
# Comment
class Foo; end

class Bar < Foo; end
```

1. Does the change modify things that do not impact resolution?
    - If a change modifies comments or adds blank lines, then we need to update the definition/reference data, but no
    resolution is required as a result
2. Does the change modify definitions or constant references?
    - If a change defines new constants or adds new references (like adding `, Qux` to an include), then that can
    potentially invalidate resolution data, including what a reference resolves to and ancestor chain information
3. Does the change modify ancestor chains?
    - If a change modifies ancestors, like changing parent classes, includes, prepends or extends, then every constant
    reference inside of that namespace may now resolve to a different declaration. Additionally, all descendants of the
    namespace also have their own ancestor chains invalidated because they depend on the namespace that got modified,
    which recurses and keeps triggering invalidation until we reach the end of all dependent declarations and references

The essence of the algorithm is that we want to invalidate as little as possible to produce accurate analysis. In our
example:

- If the ancestors of `Bar` change because of a new include, we do not have to invalidate anything related to `Foo`
- However, if we add an include to `Foo`, then the ancestors of `Bar` are also impacted (it is a descendant of `Foo`),
  which means we need to recurse and invalidate `Bar` as well

In more abstract terms, changing a document causes the invalidation of a subset of the Rubydex graph. The key to
achieving optimal performance is ensuring that the subset is as small as possible while maintaining correctness. Any
changes that involve resolution or incremental invalidation must comply with this over-arching goal.

## FFI Layer

The Rust crate exposes a C-compatible FFI API through `rubydex-sys`. The C extension in `ext/rubydex/` wraps this API for Ruby.

### Naming Conventions

- `rdx_*`: Rust FFI exports (e.g., `rdx_graph_new()`)
- `rdxr_*`: Ruby method callbacks (e.g., `rdxr_graph_alloc()`)
- `rdxi_*`: Shared C helpers (e.g., `rdxi_str_array_to_char()`)
- Static functions have no prefix
