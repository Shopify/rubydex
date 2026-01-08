# Rubydex

This project is a high performance static analysis toolkit for the Ruby language. The goal is to be a solid
foundation to power a variety of tools, such as type checkers, linters, language servers and more.

## Usage

Both Ruby and Rust APIs are made available through a gem and a crate, respectively. Here's a simple example
of using the Ruby API:

```ruby
# Create a new graph representing the current workspace
graph = Rubydex::Graph.new
# Index the entire workspace with all dependencies
graph.index_workspace
# Transform the initially collected information into its semantic understanding by running resolution
graph.resolve

# Access the information as needed
graph["Foo"]
```

## Contributing

See [the contributing documentation](CONTRIBUTING.md).
