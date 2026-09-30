# frozen_string_literal: true

module Rubydex
  module MCPServer
    class FindDeadCodeCandidatesTool < BaseTool
      tool_name "find_dead_code_candidates"
      description "Find Ruby classes, modules, and constants with no detected references, including dependency code. Dynamic usage through DSLs or metaprogramming may not be detected."
      input_schema(
        properties: {
          limit: { type: "integer", description: "Page size (default 50, capped at 100)" },
          offset: { type: "integer", description: "Number of results to skip (default 0)" },
        },
      )

      #: (?limit: Integer, ?offset: Integer) -> Tool::Response
      def call(limit: nil, offset: nil)
        declarations = @graph.dead_code_candidates.sort_by(&:name)
        page, total = paginate(declarations, offset, limit, 100)
        candidates = page.map do |declaration|
          {
            name: declaration.name,
            kind: declaration_kind(declaration),
            locations: declaration.definitions.sort_by(&:location).map do |definition|
              display_location(definition.location)
            end,
          }
        end

        response(candidates: candidates, total: total)
      end
    end
  end
end
