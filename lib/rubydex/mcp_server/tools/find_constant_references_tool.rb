# frozen_string_literal: true

module Rubydex
  module MCPServer
    class FindConstantReferencesTool < BaseTool
      tool_name "find_constant_references"
      description "Locate resolved references to a Ruby class, module, or constant."
      input_schema(
        properties: {
          name: { type: "string", description: "Fully qualified class, module, or constant name" },
          limit: { type: "integer", description: "Page size (default 50, capped at 200)" },
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
        else
          references = case declaration
          when Rubydex::Namespace, Rubydex::Constant, Rubydex::ConstantAlias
            declaration.references
          else
            []
          end
          page, total = paginate(references, offset, limit, 200)
          payload = page.map do |reference|
            display = reference.location.to_display
            {
              path: format_path(display.uri),
              line: display.start_line,
              column: display.start_column,
            }
          end

          response(name: name, references: payload, total: total)
        end
      end
    end
  end
end
