# frozen_string_literal: true

module Rubydex
  module MCPServer
    class CodebaseStatsTool < BaseTool
      tool_name "codebase_stats"
      description "Get an overview of the indexed Ruby codebase: total file count, declaration counts, and breakdown by kind (classes, modules, methods, constants). Use this to understand codebase size and composition, or to verify that indexing completed successfully."
      input_schema(properties: {})

      #: -> Tool::Response
      def call
        declaration_count = 0
        breakdown = Hash.new(0)
        @graph.declarations.each do |declaration|
          declaration_count += 1
          breakdown[declaration_kind(declaration)] += 1
        end

        response(
          files: @graph.documents.count,
          declarations: declaration_count,
          definitions: @graph.documents.sum { |document| document.definitions.count },
          constant_references: @graph.constant_references.count,
          method_references: @graph.method_references.count,
          breakdown_by_kind: breakdown,
        )
      end
    end
  end
end
