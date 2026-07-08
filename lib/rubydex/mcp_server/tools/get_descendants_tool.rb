# frozen_string_literal: true

module Rubydex
  module MCPServer
    class GetDescendantsTool < BaseTool
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
