# frozen_string_literal: true

module Rubydex
  module MCPServer
    class FindConstantReferencesTool < Tool
      tool_name "find_constant_references"
      description "Find all resolved references to a Ruby class, module, or constant across the codebase. Returns file paths, line numbers, and columns for each usage. Results are paginated: the response includes `total`. If `total` exceeds the number of returned results, use `offset` to fetch subsequent pages."
      input_schema(
        properties: {
          name: { type: "string", description: "Fully qualified name of the class, module, or constant to find references for" },
          limit: { type: "integer", description: "Maximum number of references to return (default 50, max 200)" },
          offset: { type: "integer", description: "Number of references to skip for pagination (default 0)" },
        },
        required: ["name"],
      )

      class << self
        #: (name: String, ?limit: Integer, ?offset: Integer, server_state: State) -> Tool::Response
        def call(name:, limit: nil, offset: nil, server_state:)
          graph = server_state.graph_or_error

          case graph
          when Error
            MCPServer.response(graph)
          else
            declaration = MCPServer.lookup_declaration(graph, name)

            case declaration
            when Error
              MCPServer.response(declaration)
            else
              references = case declaration
              when Rubydex::Namespace, Rubydex::Constant, Rubydex::ConstantAlias
                declaration.references
              else
                []
              end
              page, total = MCPServer.paginate(references, offset, limit, 200)
              root_path = server_state.root_path
              payload = page.map do |reference|
                display = reference.location.to_display
                {
                  path: MCPServer.format_path(display.uri, root_path),
                  line: display.start_line,
                  column: display.start_column,
                }
              end

              MCPServer.response(name: name, references: payload, total: total)
            end
          end
        end
      end
    end
  end
end
