# Architecture

Rubydex uses a graph to represent Ruby code, which gets populated and analyzed in multiple phases.

## Core Concepts

### Definition vs Declaration

A **Definition** represents a single source-level construct found at a specific location in the code. It captures key
information from the AST without making major transformations or assumptions about runtime behavior. Definitions are
captured during extraction.

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

**Definitions** (4 total - what extraction discovers):

1. Module definition for `Foo` in `foo.rb`
2. Class definition for `Bar` (nested inside `Foo`) in `foo.rb`
3. Class definition for `Foo::Bar` in `other_foos.rb`
4. Class definition for `Foo::Bar` in `other_foos.rb`

**Declarations** (2 total - what resolution produces):

1. `Foo` - A module that has a constant `Bar` under its namespace
2. `Foo::Bar` - A class, composed of definitions 2, 3, and 4

### Documents

Documents represent a single resource in the codebase, which might be committed to disk or virtual. Documents are
connected to all concepts extracted from it, like definitions.

## Graph Structure

Rubydex represents the codebase as a graph, where entities are nodes and relationships are edges. The visualization below shows the conceptual structure (implemented as an adjacency list using IDs).

[Open in Excalidraw](https://excalidraw.com/#json=utleYxF0AaAgEMLpwp1LE,RrLk4AKECjnuhsVd32saxw)

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
- `NameId`: Hash combining unqualified name, parent scope and nesting
- `UriId`: Hash of document URI
- `StringId`: Hash of an interned string

## Phases of analysis

The code analysis happens in phases, which are documented in their own markdown files. Stages are used just to help
clarify at the goal of the steps.

- Indexing: stage for building the knowledge about the codebase
    - Phase 1: [Extraction](extraction.md)
    - Phase 2: [Resolution](resolution.md)
