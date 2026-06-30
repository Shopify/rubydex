# frozen_string_literal: true

module Rubydex
  module MCPServer
    class GetDescendantsTool < Tool
      tool_name "get_descendants"
      description "Returns all known descendants for the given namespace including itself and all transitive descendants. Can be used to understand how a module/class is used across the codebase. Results are paginated: the response includes `total`. If `total` exceeds the number of returned results, use `offset` to fetch subsequent pages."
      input_schema(
        properties: {
          name: { type: "string", description: "Fully qualified name of the class or module" },
          limit: { type: "integer", description: "Maximum number of descendants to return (default 50, max 500)" },
          offset: { type: "integer", description: "Number of descendants to skip for pagination (default 0)" },
        },
        required: ["name"],
      )

      class << self
        #: (name: String, ?limit: Integer, ?offset: Integer, server: Server) -> Tool::Response
        def call(name:, limit: nil, offset: nil, server:)
          graph = server.graph_or_error

          case graph
          when Error
            MCPServer.response(graph)
          else
            declaration = MCPServer.lookup_declaration(graph, name)

            case declaration
            when Error
              MCPServer.response(declaration)
            when Rubydex::Namespace
              page, total = MCPServer.paginate(declaration.descendants, offset, limit, 500)
              descendants = page.map do |descendant|
                {
                  name: descendant.name,
                  kind: MCPServer.declaration_kind(descendant),
                }
              end

              MCPServer.response(name: declaration.name, descendants: descendants, total: total)
            else
              MCPServer.response(
                Error.new(
                  "invalid_kind",
                  "'#{name}' is not a class or module (it is a #{MCPServer.declaration_kind(declaration)})",
                  "get_descendants only works on classes and modules, not methods or constants",
                ),
              )
            end
          end
        end
      end
    end
  end
end
