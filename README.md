# Rubydex

This project is a high performance static analysis toolkit for the Ruby language. The goal is to be a solid
foundation to power a variety of tools, such as type checkers, linters, language servers and more.

## Usage

Both Ruby and Rust APIs are made available through a gem and a crate, respectively. Here's a simple example
of using the Ruby API:

```ruby
# Create a new graph representing the current workspace
graph = Rubydex::Graph.new
# Configuring graph LSP encoding
graph.set_encoding("utf16")
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

## Contributing

See [the contributing documentation](CONTRIBUTING.md).
