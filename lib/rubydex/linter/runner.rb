# frozen_string_literal: true

require "pathname"

module Rubydex
  module Linter
    class Runner
      #: Graph
      attr_reader :graph

      #: Array[singleton(Rule)]
      attr_reader :rules

      #: (Graph, rules: Array[singleton(Rule)], config: LinterConfig) -> void
      def initialize(graph, rules:, config:)
        @graph = graph
        @config = config
        @rules = rules.select { |rule| config.rule_enabled?(rule) }.sort_by { |rule| rule.name.to_s }
        @dependency_paths = Gem.path #: Array[String]
      end

      #: () -> Result
      def run
        rule_diagnostics = @rules.flat_map do |rule_class|
          rule = rule_class.new(@graph, config: @config)
          rule.lint
          rule.diagnostics
        end

        # Graph diagnostics are surfaced by the linter, but not owned by it. Linter configuration controls
        # surfacing here, not registration in the graph.
        diagnostics = (@graph.diagnostics + rule_diagnostics).select do |diagnostic|
          !location_in_dependency_path?(diagnostic.location) &&
            diagnostic_included_by_config?(diagnostic) &&
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

      #: (Diagnostic) -> bool
      def diagnostic_included_by_config?(diagnostic)
        rule_config = @config.rules[diagnostic.rule]
        return true unless rule_config

        rule_config.enabled? && !location_matches_patterns?(diagnostic.location, rule_config.exclude_patterns)
      end

      #: (Location) -> bool
      def location_in_dependency_path?(location)
        path = location.to_file_path

        @dependency_paths.any? do |dependency_path|
          path == dependency_path || path.start_with?("#{dependency_path}/")
        end
      rescue Location::NotFileUriError
        false
      end

      #: (Location, Array[String]) -> bool
      def location_matches_patterns?(location, patterns)
        return false if patterns.empty?

        Helpers::PathHelpers.path_matches_patterns?(
          location.to_file_path,
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
