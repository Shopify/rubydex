# Architecture

Saturn's representation of the Ruby code it analyzes is a graph, where key entities are represented as nodes and the
relationship between them as edges. The visualization below demonstrates the conceptual structure of the graph. In
practice, it is implemented through IDs between the entities - an adjacency list approach.

[Open in Excalidraw](https://excalidraw.com/#json=hQiLSD8nJRVxONhuwtSn4,L78TkfeB4YL1HJTf5L0bvw)

![Graph visualization](images/graph.png)

The concepts mentioned are implemented in files described below. To understand the concepts in this graph, let's take a
simple example:

```ruby
# foo.rb
class Foo
end

# other_foos.rb
class Foo; end
class Foo; end
```

- `model/document.rs`: The representation of a document registered in the system, generally a file like `foo.rb` or
`other_foos.rb` in our example
- `model/declaration.rs`: The global concept of a name, here `Foo` in our example, which may be composed of multiple
definitions
- `model/definitions.rs`: A single definition that contributes to a declaration. In our example one comes from `foo.rb`
and two more from `other_foos.rb`

Connections between the nodes are made using IDs defined in `ids.rs`:

- `DeclarationId`: the hashed version of a fully qualified name (e.g.: `Foo::Bar` or `Foo#my_method`)
- `DefinitionId`: the hashed version of the URI and byte offset combination where we found the definition in addition to
its name
- `NameId`: the hashed version of an unqualified name (e.g.: `Bar` instead of `Foo::Bar`)
- `UriId`: the hashed version of a string URI
