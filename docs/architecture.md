# Architecture

Rubydex indexes Ruby codebases in two distinct stages: **Discovery** and **Resolution**. Understanding this separation is crucial for working with the codebase.

## Core Concepts: Definition vs Declaration

A **Definition** represents a single source-level construct found at a specific location in the code. It captures exactly what the parser sees without making assumptions about runtime behavior.

A **Declaration** represents the global semantic concept of a name, combining all definitions that contribute to the same fully qualified name. Declarations are produced during resolution.

Consider this example:

```ruby
# foo.rb
module Foo
  class Bar; end
end

# other_foos.rb
class Foo::Bar; end
class Foo::Bar; end
```

**Definitions** (4 total - what the indexer discovers):

1. Module definition for `Foo` in `foo.rb`
2. Class definition for `Bar` (nested inside `Foo`) in `foo.rb`
3. Class definition for `Foo::Bar` in `other_foos.rb`
4. Class definition for `Foo::Bar` in `other_foos.rb`

**Declarations** (2 total - what resolution produces):

1. `Foo` - A module that has a constant `Bar` under its namespace
2. `Foo::Bar` - A class, composed of definitions 2, 3, and 4

## Two-Stage Indexing Pipeline

### Stage 1: Discovery

Discovery walks the AST and extracts definitions from source code. It captures **only what is explicitly written**, making no assumptions about runtime behavior.

**What Discovery does:**

- Creates `Definition` objects for classes, modules, methods, constants, variables
- Records source locations, comments, and lexical ownership (`owner_id`)
- Captures unresolved constant references (e.g., `Foo::Bar` as a `NameId`)
- Records mixins (`include`, `prepend`, `extend`) on their containing class/module

**What Discovery does NOT do:**

- Compute fully qualified names
- Resolve constant references to declarations
- Determine inheritance hierarchies
- Assign semantic membership

#### Why No Assumptions During Discovery?

Consider this example:

```ruby
module Bar; end

class Foo
  class Bar::Baz; end
end
```

Without resolving constant references, it may appear that `Bar::Baz` is created under `Foo`. But it's actually not - `Bar` resolves to the top-level `Bar`, so the class is `Bar::Baz`, not `Foo::Bar::Baz`.

Discovery cannot know this without first resolving `Bar`. This is why fully qualified names and semantic membership are computed during Resolution, not Discovery.

### Stage 2: Resolution

Resolution combines the discovered definitions to build a semantic understanding of the codebase.

**What Resolution does:**

- Compute fully qualified names for all definitions
- Create `Declaration` objects that group definitions by fully qualified name
- Resolve constant references to their target declarations
- Linearize ancestor chains (including resolving mixins)
- Assign semantic membership (which methods/constants belong to which class)
- Create implicit singleton classes from `def self.method` patterns

## Graph Structure

Rubydex represents the codebase as a graph, where entities are nodes and relationships are edges. The visualization below shows the conceptual structure (implemented as an adjacency list using IDs).

[Open in Excalidraw](https://excalidraw.com/#json=hQiLSD8nJRVxONhuwtSn4,L78TkfeB4YL1HJTf5L0bvw)

![Graph visualization](images/graph.png)

### Key Files

- `model/document.rs`: Represents a registered file (e.g., `foo.rb`, `other_foos.rb`)
- `model/definitions.rs`: Individual definitions discovered from source code
- `model/declaration.rs`: Global declarations produced during resolution
- `model/graph.rs`: The main graph structure containing all entities

### ID Types

Connections between nodes use hashed IDs defined in `ids.rs`:

- `DefinitionId`: Hash of URI, byte offset, and name
- `DeclarationId`: Hash of fully qualified name (e.g., `Foo::Bar` or `Foo#my_method`)
- `NameId`: Hash of unqualified name combined with parent scope and lexical nesting context
- `UriId`: Hash of file URI
- `StringId`: ID for interned string values
- `ReferenceId`: ID for constant or method reference occurrences (combines reference kind, URI, and offset)

## MCP Server

The `rubydex-mcp` crate exposes rubydex's code intelligence as MCP tools over stdio JSON-RPC. The server indexes the codebase on startup, then serves tool requests against the immutable graph.

### Pagination

Three tools return paginated collections: `search_declarations`, `get_descendants`, and `find_constant_references`. Each accepts `offset` and `limit` parameters and returns a `total` count.

Pagination uses a two-pass approach: first collect all entries that pass filtering into a `Vec`, then apply `skip(offset).take(limit)`. This ensures `total` accurately reflects the number of results the caller can page through.

### Result Ordering

All collection-returning tools iterate over `IdentityHashMap` or `IdentityHashSet` structures. These use a deterministic hasher (xxh3, no random seed), so iteration order is fixed for a given map state.

- **Within a server session**: Order is consistent between requests as long as the graph has not been re-indexed. Incremental re-indexing (e.g., after a file save) may change the graph between paginated requests, causing items to shift, appear, or disappear. Callers should not assume pagination stability across graph changes.
- **Across server restarts**: Order may change. Indexing is parallelized, so thread scheduling affects insertion order into the graph, which determines HashMap/HashSet bucket layout.

### Key Files

- `rubydex-mcp/src/server.rs`: Tool handler implementations and pagination logic
- `rubydex-mcp/src/tools.rs`: Parameter struct definitions with JSON Schema annotations
- `rubydex-mcp/tests/mcp.rs`: Integration tests (full MCP protocol over stdio)

## FFI Layer

The Rust crate exposes a C-compatible FFI API through `rubydex-sys`. The C extension in `ext/rubydex/` wraps this API for Ruby.

### Naming Conventions

- `rdx_*`: Rust FFI exports (e.g., `rdx_graph_new()`)
- `rdxr_*`: Ruby method callbacks (e.g., `rdxr_graph_alloc()`)
- `rdxi_*`: Shared C helpers (e.g., `rdxi_str_array_to_char()`)
- Static functions have no prefix
