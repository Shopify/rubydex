# Rubydex

This project is a high-performance static analysis toolkit for the Ruby language. The goal is to be a solid
foundation to power a variety of tools, such as type checkers, linters, language servers and more.

[Ruby API Documentation](https://shopify.github.io/rubydex/)

## Usage

Rubydex caches the index by default. The first run indexes all files and writes a snapshot file.
Later runs read the snapshot and index only the files that changed since the last run.
The snapshot file lives under the user cache directory, in a file named for the workspace.
Set `XDG_CACHE_HOME` to change the user cache directory.
Pass `--no-cache` on the command line, or `cache: false` to `index_workspace` in the Ruby API, to skip the cache.

Both Ruby and Rust APIs are made available through a gem and a crate, respectively. Here's a simple example
of using the Ruby API:

```ruby
# Create a new graph representing the current workspace
graph = Rubydex::Graph.new
# Configuring graph LSP encoding
graph.encoding = "utf16"
# Index the entire workspace with all dependencies. This reads a cached snapshot when one exists,
# indexes only the files that changed, and resolves them, all in one call.
graph.index_workspace
# Or index specific file paths, bypassing the cache. This does not resolve; call `resolve` yourself.
graph.index_all(["path/to/file.rb"])
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
diagnostic.severity
diagnostic.related_information
```

## Ractor Safety

A resolved `Rubydex::Graph` can be shared across Ractors without copying:

```ruby
graph = Rubydex::Graph.new
graph.index_workspace
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

## Tools

All built-in tools are experimental. These tools can change without deprecation warnings.

### `rdx query`

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

# Parse once, then run against a graph. `run` executes the query and returns the result set.
query = Rubydex::Query.parse("MATCH (c:Class) RETURN c.name")
result = query.run(graph)

# Read the rows as Ruby objects, or render the same result set as a table or JSON string
result.rows.each { |row| puts row["c.name"] }
puts result.render("table")
puts result.render("json")

# Describe the schema
puts Rubydex::Query.schema("table")
```

### `rdx lint`

Put rule files under `rubydex_linter/rules`. Each rule must inherit from `Rubydex::Linter::Rule`.

Run the linter:

```bash
bundle exec rdx lint [PATH]
```

If you omit `PATH`, Rubydex uses the current directory.

Configure a rule in `rubydex.toml`:

```toml
[linter.rules.<Rule name>]
enabled = true
exclude = ["path_to_skip/**"]
```

### `rdx mcp`

Rubydex can run as an MCP (Model Context Protocol) server, enabling AI assistants
like Claude to semantically query your Ruby codebase.

#### Setup

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

#### Available MCP Tools

| Tool | Description |
|------|-------------|
| `search_declarations` | Fuzzy search for classes, modules, methods, constants |
| `get_declaration` | Full details by fully qualified name with docs, ancestors, members |
| `get_descendants` | What classes/modules inherit from or include this one |
| `find_constant_references` | All precise, resolved constant references across the codebase |
| `get_file_declarations` | List declarations defined in a specific file |
| `codebase_stats` | High-level statistics about the indexed codebase |

## Skill Library (Experimental)

Rubydex ships with a built in skill library. These skills are referenced by id by Rubydex tools (for example: `send-private-method` maps to `skills/send-private-method/SKILL.md`).

The intention is for Rubydex to provide deterministic skill loading for agents. If a rule returns a skill id, the agent can then fetch the skill with `rdx skill <id>`.

This removes the need for agents to load a large number of possibly unrelated skill descriptions when they may or may not be necessary. The information only surfaces if there is a verified violation to fix, and it only needs to be loaded once, rather than inlining it on every violation in every run of the tool (the common pattern for linters and analyzers).

## Contributing

See [the contributing documentation](CONTRIBUTING.md).
