# frozen_string_literal: true

module Rubydex
  module MCPServer
    class GetDeclarationTool < BaseTool
      tool_name "get_declaration"
      description "Get a Ruby declaration's definition locations, documentation, ancestors, and members."
      input_schema(
        properties: {
          name: { type: "string", description: 'Exact fully qualified name: "Foo::Bar", "Foo::Bar#method_name()" (instance method), "Foo::Bar::<Bar>" (singleton class), or "Foo::Bar::<Bar>#method_name()" (class method)' },
        },
        required: ["name"],
      )

      #: (name: String) -> Tool::Response
      def call(name:)
        declaration = lookup_declaration(name)

        case declaration
        when Error
          response(declaration)
        else
          definitions = declaration.definitions.map do |definition|
            display_location(definition.location).merge(
              comments: definition.comments.map do |comment|
                comment.string.delete_prefix("# ")
              end,
            )
          end

          ancestors = if declaration.is_a?(Rubydex::Namespace)
            declaration.ancestors.map do |ancestor|
              {
                name: ancestor.name,
                kind: declaration_kind(ancestor),
              }
            end
          else
            []
          end

          members = if declaration.is_a?(Rubydex::Namespace)
            declaration.members.map do |member|
              payload = {
                name: member.name,
                kind: declaration_kind(member),
              }

              definition = member.definitions.first
              payload[:location] = display_location(definition.location) if definition
              payload
            end
          else
            []
          end

          response(
            name: declaration.name,
            kind: declaration_kind(declaration),
            definitions: definitions,
            ancestors: ancestors,
            members: members,
          )
        end
      end
    end
  end
end
