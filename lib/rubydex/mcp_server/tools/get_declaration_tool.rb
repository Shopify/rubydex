# frozen_string_literal: true

module Rubydex
  module MCPServer
    class GetDeclarationTool < Tool
      tool_name "get_declaration"
      description 'Get complete information about a Ruby class, module, method, or constant by its exact fully qualified name. Returns file locations, documentation comments, ancestor chain, and members with locations. FQN format: "Foo::Bar" for classes/modules/constants, "Foo::Bar#method_name" for instance methods.'
      input_schema(
        properties: {
          name: { type: "string", description: "Fully qualified name of the declaration (e.g. 'Foo::Bar', 'Foo::Bar#baz')" },
        },
        required: ["name"],
      )

      class << self
        #: (name: String, server: Server) -> Tool::Response
        def call(name:, server:)
          graph = server.graph_or_error

          case graph
          when Error
            MCPServer.response(graph)
          else
            declaration = MCPServer.lookup_declaration(graph, name)

            case declaration
            when Error
              MCPServer.response(declaration)
            else
              root_path = server.root_path
              definitions = declaration.definitions.map do |definition|
                MCPServer.display_location(definition.location, root_path).merge(
                  comments: definition.comments.map do |comment|
                    comment.string.delete_prefix("# ")
                  end,
                )
              end

              ancestors = if declaration.is_a?(Rubydex::Namespace)
                declaration.ancestors.map do |ancestor|
                  {
                    name: ancestor.name,
                    kind: MCPServer.declaration_kind(ancestor),
                  }
                end
              else
                []
              end

              members = if declaration.is_a?(Rubydex::Namespace)
                declaration.members.map do |member|
                  payload = {
                    name: member.name,
                    kind: MCPServer.declaration_kind(member),
                  }

                  definition = member.definitions.first
                  payload[:location] = MCPServer.display_location(definition.location, root_path) if definition
                  payload
                end
              else
                []
              end

              MCPServer.response(
                name: declaration.name,
                kind: MCPServer.declaration_kind(declaration),
                definitions: definitions,
                ancestors: ancestors,
                members: members,
              )
            end
          end
        end
      end
    end
  end
end
