# frozen_string_literal: true

module Rubydex
  module MCPServer
    class SearchDeclarationsTool < Tool
      tool_name "search_declarations"
      description 'Search for Ruby classes, modules, methods, or constants by name. Use this INSTEAD OF Grep when you know part of a Ruby identifier name and want to find its definition. Returns fully qualified names, kinds, and file locations. Use the `kind` filter ("Class", "Module", "Method", "Constant") to narrow results. Set `match_mode` to "exact" for precise substring matching or "fuzzy" for LSP-style workspace symbol search (default). Results are paginated: the response includes `total` (the full count of matches). If `total` exceeds the number of returned results, use `offset` to fetch subsequent pages.'
      input_schema(
        properties: {
          query: { type: "string", description: "Search query to match against declaration names" },
          kind: { type: "string", description: "Filter by declaration kind: Class, Module, Method, Constant, etc." },
          match_mode: { type: "string", description: 'Matching mode: "fuzzy" (default) for LSP-style workspace symbol search, or "exact" for precise substring matching' },
          limit: { type: "integer", description: "Maximum number of results to return (default 50, max 100)" },
          offset: { type: "integer", description: "Number of results to skip for pagination (default 0)" },
        },
        required: ["query"],
      )

      class << self
        #: (query: String, ?kind: String, ?match_mode: String, ?limit: Integer, ?offset: Integer, server_state: State) -> Tool::Response
        def call(query:, kind: nil, match_mode: nil, limit: nil, offset: nil, server_state:)
          graph = server_state.graph_or_error

          case graph
          when Error
            MCPServer.response(graph)
          else
            declarations = case match_mode
            when nil, "fuzzy"
              graph.fuzzy_search(query)
            when "exact"
              graph.search(query)
            else
              return MCPServer.response(
                Error.new(
                  "invalid_match_mode",
                  "Invalid match_mode '#{match_mode}'",
                  'Use "fuzzy" or "exact"',
                ),
              )
            end

            if kind
              declarations = declarations.lazy.select { |declaration| MCPServer.declaration_kind(declaration).casecmp?(kind) }
            end

            page, total = MCPServer.paginate(declarations, offset, limit, 100)
            root_path = server_state.root_path
            results = page.map do |declaration|
              {
                name: declaration.name,
                kind: MCPServer.declaration_kind(declaration),
                locations: declaration.definitions.map do |definition|
                  MCPServer.display_location(definition.location, root_path)
                end,
              }
            end

            MCPServer.response(results: results, total: total)
          end
        end
      end
    end
  end
end
