# frozen_string_literal: true

require "pathname"

module Rubydex
  module Linter
    class Runner
      #: Graph
      attr_reader :graph

      #: Array[singleton(CustomRule)]
      attr_reader :custom_rules

      #: (Graph, custom_rules: Array[singleton(CustomRule)], config: LinterConfig) -> void
      def initialize(graph, custom_rules:, config:)
        @graph = graph
        @config = config
        @custom_rules = custom_rules.select { |rule| config.rule_enabled?(rule) }.sort_by(&:rule_name)
        @dependency_paths = Gem.path #: Array[String]
      end

      #: () -> Result
      def run
        rule_diagnostics = @custom_rules.flat_map do |rule_class|
          rule = rule_class.new(@graph, config: @config)
          rule.lint
          filter_diagnostics(rule.diagnostics, @config.excludes_for(rule_class))
        end

        graph_diagnostics = filter_diagnostics(@graph.diagnostics, [])
        diagnostics = (graph_diagnostics + rule_diagnostics).select do |diagnostic|
          diagnostic_in_workspace?(diagnostic)
        end.sort_by do |diagnostic|
          location = diagnostic.location
          [
            location.uri,
            location.start_line,
            location.start_column,
            location.end_line,
            location.end_column,
            diagnostic.rule,
            diagnostic.message,
          ]
        end

        Result.new(diagnostics)
      end

      private

      #: (Array[Diagnostic], Array[String]) -> Array[Diagnostic]
      def filter_diagnostics(diagnostics, exclude_patterns)
        diagnostics.reject do |diagnostic|
          location_excluded?(diagnostic.location, exclude_patterns)
        end
      end

      #: (Location, Array[String]) -> bool
      def location_excluded?(location, patterns)
        path = location.to_file_path
        return true if @dependency_paths.any? do |dependency_path|
          path == dependency_path || path.start_with?("#{dependency_path}/")
        end

        Helpers::PathHelpers.path_matches_patterns?(
          path,
          patterns,
          workspace: @graph.workspace_path,
          flags: Helpers::PathHelpers::RUBOCOP_EXCLUDE_FNMATCH_FLAGS,
        )
      rescue Location::NotFileUriError
        false
      end

      #: (Diagnostic) -> bool
      def diagnostic_in_workspace?(diagnostic)
        path = diagnostic.location.to_file_path
        workspace_path = Pathname.new(File.expand_path(@graph.workspace_path))
        relative_path = Pathname.new(File.expand_path(path)).relative_path_from(workspace_path)

        relative_path.each_filename.first != ".."
      rescue Location::NotFileUriError
        true
      rescue ArgumentError
        false
      end
    end
  end
end
