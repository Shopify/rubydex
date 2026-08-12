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

        diagnostics = (@graph.diagnostics + rule_diagnostics).select do |diagnostic|
          !location_in_dependency_path?(diagnostic.location) &&
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

      #: (Location) -> bool
      def location_in_dependency_path?(location)
        path = location.to_file_path

        @dependency_paths.any? do |dependency_path|
          path == dependency_path || path.start_with?("#{dependency_path}/")
        end
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
