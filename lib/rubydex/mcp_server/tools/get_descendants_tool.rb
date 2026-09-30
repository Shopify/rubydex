# frozen_string_literal: true

module Rubydex
  module MCPServer
    class GetDescendantsTool < BaseTool
      tool_name "get_descendants"
      description "List a class or module and its known descendants, including transitive descendants."
      input_schema(
        properties: {
          name: { type: "string", description: "Fully qualified class or module name" },
          limit: { type: "integer", description: "Page size (default 50, capped at 500)" },
          offset: { type: "integer", description: "Number of results to skip (default 0)" },
        },
        required: ["name"],
      )

      #: (name: String, ?limit: Integer, ?offset: Integer) -> Tool::Response
      def call(name:, limit: nil, offset: nil)
        declaration = lookup_declaration(name)

        case declaration
        when Error
          response(declaration)
        when Rubydex::Namespace
          page, total = paginate(declaration.descendants, offset, limit, 500)
          descendants = page.map do |descendant|
            {
              name: descendant.name,
              kind: declaration_kind(descendant),
            }
          end

          response(name: declaration.name, descendants: descendants, total: total)
        else
          response(
            Error.new(
              "invalid_kind",
              "'#{name}' is not a class or module (it is a #{declaration_kind(declaration)})",
              "get_descendants only works on classes and modules, not methods or constants",
            ),
          )
        end
      end
    end
  end
end
