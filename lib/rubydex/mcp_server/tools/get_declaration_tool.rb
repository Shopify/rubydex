# frozen_string_literal: true

module Rubydex
  module MCPServer
    class GetDeclarationTool < BaseTool
      tool_name "get_declaration"
      description 'Get complete information about a Ruby class, module, method, or constant by its exact fully qualified name. Returns file locations, documentation comments, ancestor chain, and members with locations. FQN format: "Foo::Bar" for classes/modules/constants, "Foo::Bar#method_name" for instance methods, "Foo::Bar::<Bar>" for singleton classes, and "Foo::Bar::<Bar>#method_name" for class methods.'
      input_schema(
        properties: {
          name: { type: "string", description: "Fully qualified name of the declaration (e.g. 'Foo::Bar', 'Foo::Bar#baz')" },
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
