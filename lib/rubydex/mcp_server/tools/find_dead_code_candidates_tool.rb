# frozen_string_literal: true

module Rubydex
  module MCPServer
    class FindDeadCodeCandidatesTool < BaseTool
      tool_name "find_dead_code_candidates"
      description "Find Ruby classes, modules, and constants with no detected references. Returns candidate names, kinds, and definition locations, including dependency definitions. Candidates may still be used through DSLs or metaprogramming; verify before deleting code. Results are paginated: the response includes `total`. If `total` exceeds the number of returned candidates, use `offset` to fetch subsequent pages."
      input_schema(
        properties: {
          limit: { type: "integer", description: "Maximum number of candidates to return (default 50, max 100)" },
          offset: { type: "integer", description: "Number of candidates to skip for pagination (default 0)" },
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
