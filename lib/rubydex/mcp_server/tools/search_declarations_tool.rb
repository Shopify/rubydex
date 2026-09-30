# frozen_string_literal: true

module Rubydex
  module MCPServer
    class SearchDeclarationsTool < BaseTool
      tool_name "search_declarations"
      description "Find definitions of Ruby classes, modules, methods, or constants by full or partial name."
      input_schema(
        properties: {
          query: { type: "string", description: "Text to match against fully qualified names. An empty string matches all declarations." },
          kind: { type: "string", description: "Case-insensitive declaration kind, such as Class, Module, Method, or Constant" },
          match_mode: { type: "string", description: '"fuzzy" (default) matches characters in order, ignoring ASCII case. "exact" matches a case-sensitive substring.' },
          limit: { type: "integer", description: "Page size (default 50, capped at 100)" },
          offset: { type: "integer", description: "Number of results to skip (default 0)" },
        },
        required: ["query"],
      )

      #: (query: String, ?kind: String, ?match_mode: String, ?limit: Integer, ?offset: Integer) -> Tool::Response
      def call(query:, kind: nil, match_mode: nil, limit: nil, offset: nil)
        declarations = case match_mode
        when nil, "fuzzy"
          @graph.fuzzy_search(query)
        when "exact"
          @graph.search(query)
        else
          return response(
            Error.new(
              "invalid_match_mode",
              "Invalid match_mode '#{match_mode}'",
              'Use "fuzzy" or "exact"',
            ),
          )
        end

        if kind
          declarations = declarations.lazy.select { |declaration| declaration_kind(declaration).casecmp?(kind) }
        end

        page, total = paginate(declarations, offset, limit, 100)
        results = page.map do |declaration|
          {
            name: declaration.name,
            kind: declaration_kind(declaration),
            locations: declaration.definitions.map do |definition|
              display_location(definition.location)
            end,
          }
        end

        response(results: results, total: total)
      end
    end
  end
end
