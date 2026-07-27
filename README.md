# Rubydex

This project is a high-performance static analysis toolkit for the Ruby language. The goal is to be a solid
foundation to power a variety of tools, such as type checkers, linters, language servers and more.

[Ruby API Documentation](https://shopify.github.io/rubydex/)

## Usage

Both Ruby and Rust APIs are made available through a gem and a crate, respectively. Here's a simple example
of using the Ruby API:

```ruby
# Create a new graph representing the current workspace
graph = Rubydex::Graph.new
# Configuring graph LSP encoding
graph.encoding = "utf16"
# Index the entire workspace with all dependencies
graph.index_workspace
# Or index specific file paths
graph.index_all(["path/to/file.rb"])
# Transform the initially collected information into its semantic understanding by running resolution
graph.resolve
# Get all diagnostics acquired during the analysis
graph.diagnostics

# Iterating over graph nodes
graph.declarations
graph.documents
graph.constant_references
graph.method_references

# Analyzing require paths
graph.resolve_require_path("rails/engine", load_paths) # => document pointed by `rails/engine`
graph.require_paths(load_paths) # => array of all indexed require paths

# Querying
graph["Foo"] # Get declaration by fully qualified name
graph.search("Foo#b") # Name search
graph.resolve_constant("Bar", ["Foo", "Baz::Qux"]) # Resolve constant reference based on nesting

# Declarations
declaration = graph["Foo"]

# All declarations include
declaration.name
declaration.unqualified_name
declaration.definitions
declaration.owner

# Namespace declarations include
declaration.member("bar()")
declaration.member("@ivar")
declaration.singleton_class
declaration.ancestors
declaration.descendants

# Documents
document = graph.documents.first
document.uri
document.definitions # => list of definitions discovered in this document

# Definitions
definition = declaration.definitions.first
definition.location
definition.comments
definition.name
definition.deprecated?
definition.name_location

# Locations
location = definition.location
location.path

# Diagnostics
diagnostic = graph.diagnostics.first
diagnostic.rule
diagnostic.message
diagnostic.location
```

## Ractor Safety

A resolved `Rubydex::Graph` can be shared across Ractors without copying:

```ruby
graph = Rubydex::Graph.new
graph.index_workspace
graph.resolve
Ractor.make_shareable(graph)

# Worker Ractors can now read the graph in parallel
ractor = Ractor.new(graph) { |g| g["Foo"]&.name }
ractor.value
```

Thread safety comes from an `RwLock` on the Rust side, **not** from Ruby's
`freeze`. This is a deliberate, but potentially surprising, choice:

- A frozen (or `make_shareable`'d) graph **can still be mutated** — methods like
  `index_source`, `resolve`, `exclude_patterns`, and `encoding=` work on a frozen
  graph. This supports interactive use cases (LSP/MCP) that need incremental
  edits while worker Ractors read concurrently.
- Because the same underlying allocation is shared, callers are responsible for
  ordering concurrent writes and reads, as with any shared mutable state across
  Ractors.
- Graphs cannot be `dup`'d or `clone`'d; both raise `RuntimeError` to avoid
  aliasing the Rust allocation (which would double-free on GC) or ballooning
  memory with a deep copy.

## Querying with Cypher

Rubydex exposes the indexed graph through a read-only subset of the
[Cypher](https://opencypher.org/) query language. Only read clauses (`MATCH`,
`WHERE`, `RETURN`, ...) are supported; there is no way to mutate the graph.

From the command line:

```bash
# Run a query against the current workspace
bundle exec rdx query "MATCH (c:Class)-[:DEFINES]->(m:Method) RETURN c.name, m.name"

# Render results as JSON instead of a table
bundle exec rdx query "MATCH (c:Class) RETURN c.name" --format json

# Describe the queryable schema (node labels and relationship types) without indexing
bundle exec rdx query --schema
```

From Ruby:

```ruby
graph = Rubydex::Graph.new
graph.index_workspace
graph.resolve

# Parse once, render against a graph as a table or JSON string
query = Rubydex::Query.parse("MATCH (c:Class) RETURN c.name")
puts query.render(graph, "table")
puts query.render(graph, "json")

# Describe the schema
puts Rubydex::Query.schema("table")
```

## Code Complexity

Rubydex computes ABC complexity scores (assignments, branches, calls) over Ruby
source, with Ruby-aware weights and compounding nesting penalties.

Thanks to [Ryan Davis](https://github.com/zenspider) for [flog](https://github.com/seattlerb/flog),
which the scoring rules and report shape here are modeled on.

A score is reported per method as `sqrt(a² + b² + c²)`, where `a`, `b`, and `c`
accumulate assignment, branch, and call weight respectively (each scaled by the
current nesting multiplier). The report totals all method scores and reports the
per-method average. `.rbs` files are excluded; everything else the indexer treats
as Ruby (`.rb`, `.rake`, `.ru`, …) is scored.

Exclusions are configured in `rubydex.toml` and are **decoupled from indexing**: the
top-level `exclude` key affects indexing only, while a separate `[complexity]` table
controls what `rdx complexity` skips. This lets you keep a file in the graph but out of
the complexity report. Both share the default skipped directories (`.git`, `node_modules`,
`tmp`, …).

```toml
# rubydex.toml
exclude = ["vendor/**"]              # indexing only; complexity still scores these

[complexity]
exclude = ["app/assets/**", "**/*_spec.rb"]   # complexity only; still indexed
```

From the command line:

```bash
# Compute a report for the current directory (top 25 methods by default)
bundle exec rdx complexity

# Scope to specific paths and show more entries
bundle exec rdx complexity app/models lib/services --top 50

# Render the full report as JSON (use --top 0 for every method)
bundle exec rdx complexity app/models --format json --top 0 > report.json

# Diff a fresh report against a baseline JSON report to track drift
bundle exec rdx complexity app/models --diff baseline.json

# Show what drives each method's score: per-construct contributions
bundle exec rdx complexity app/models --details --top 10

# Skip code outside methods: drops top-level `#none` noise
bundle exec rdx complexity app/models --methods-only

# Group entries by class with per-class subtotals
bundle exec rdx complexity app/models --group
```

This is the default text report for a small codebase:

```
    17.2: total complexity
     5.7: average complexity

    10.3: Rubydex::Complexity.analyze              /path/to/repo/lib/rubydex/complexity.rb:8-12
     5.0: Rubydex::Complexity#none                 /path/to/repo/lib/rubydex/complexity.rb:5-19
     1.9: Rubydex::Complexity.diff                 /path/to/repo/lib/rubydex/complexity.rb:15-17
```

The JSON report (`schema_version: 1`) carries `total`, `average`,
`methods_count`, and a `methods` array of entries with per-bucket breakdowns and
`start_line`/`end_line` locations. With `--details`, each entry also includes a
`details` array of per-construct contributions (`assignment`,
`branch`, `block_pass`, `magic_number`, or the called method's name) so you can see
what drives a score; the field is omitted when detail collection is off. Diff output
splits changes into regressions, improvements, added, and removed methods, each capped
at `--top` rows. `--details` works for both text and JSON reports; `--group` is
text-only (rejected with `--format json`). Neither `--details` nor `--group` applies
to `--diff` (both are rejected with it), and `--diff` requires the baseline to have
been generated with the same `--methods-only` setting.

`--details` adds a per-method breakdown:

```
    10.3: Rubydex::Complexity.analyze              /path/to/repo/lib/rubydex/complexity.rb:8-12
     1.8:   class
     1.7:   block_pass
     1.7:   map
     1.6:   raise
     1.5:   branch
     0.4:   magic_number
```

From Ruby:

```ruby
# Compute a report: returns the text or JSON string ready to print
puts Rubydex::Complexity.analyze(["app/models"], format: :text, top: 25)
json = Rubydex::Complexity.analyze(["app/models"], format: :json, top: 0)

# Per-construct breakdown, methods-only, and grouping
puts Rubydex::Complexity.analyze(["app/models"], details: true, top: 10)
puts Rubydex::Complexity.analyze(["app/models"], methods_only: true)
puts Rubydex::Complexity.analyze(["app/models"], group: true)

# Diff two JSON reports (e.g. a committed baseline against a fresh run).
# Both reports must share the same `methods_only` setting or diff raises ArgumentError.
puts Rubydex::Complexity.diff(baseline_json, json, format: :text, top: 25)
```

Run `rdx complexity --help` for the full set of options.

## MCP Server (Experimental)

Rubydex can run as an MCP (Model Context Protocol) server, enabling AI assistants
like Claude to semantically query your Ruby codebase.

### Setup

1. Add Rubydex to the Ruby project you want to index:
   ```ruby
   gem "rubydex"
   ```

2. Install the bundle:
   ```bash
   bundle install
   ```

3. Configure your MCP client to run `bundle exec rdx mcp`.

   Using Claude Code as an example:
   ```bash
   claude mcp add --scope project rubydex -- bundle exec rdx mcp
   ```

   Using Codex as an example:
   ```bash
   codex mcp add rubydex -- bundle exec rdx mcp
   ```

   Start your MCP client from that project directory. The MCP server indexes
   the project at startup and provides semantic code intelligence tools through
   the tools below.

### Available MCP Tools

| Tool | Description |
|------|-------------|
| `search_declarations` | Fuzzy search for classes, modules, methods, constants |
| `get_declaration` | Full details by fully qualified name with docs, ancestors, members |
| `get_descendants` | What classes/modules inherit from or include this one |
| `find_constant_references` | All precise, resolved constant references across the codebase |
| `get_file_declarations` | List declarations defined in a specific file |
| `codebase_stats` | High-level statistics about the indexed codebase |

## Contributing

See [the contributing documentation](CONTRIBUTING.md).
