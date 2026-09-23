# frozen_string_literal: true

require "rubydex/dead_code"

module Rubydex
  module MCPServer
    class FindDeadCodeCandidatesTool < BaseTool
      tool_name "find_dead_code_candidates"
      description "Find Ruby classes, modules, and constants defined in the workspace with no detected references. Returns candidates for review, not proof that code is dead or safe to delete. Methods are not supported. Honors [dead-code].exclude from rubydex.toml. Results contain names, kinds, and workspace-relative file locations with one-based lines and columns. Results are sorted by name and paginated; `total` is the filtered candidate count. Use `offset` to fetch subsequent pages."
      input_schema(
        properties: {
          path: { type: "string", description: "Workspace-relative file path or glob to filter definition locations. Use forward slashes; * matches within a directory, while **/ and a trailing /** include nested directories. References across the entire indexed graph still count." },
          limit: { type: "integer", description: "Maximum number of candidates to return (default 50, max 100)" },
          offset: { type: "integer", description: "Number of candidates to skip for pagination (default 0)" },
        },
      )

      #: (?path: String, ?limit: Integer, ?offset: Integer) -> Tool::Response
      def call(path: nil, limit: nil, offset: nil)
        page, total = paginate(DeadCode.candidates(@graph, path: path), offset, limit, 100)
        response(candidates: page, total: total)
      end
    end
  end
end
