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
        @dependency_patterns = Gem.path.map { |path| "#{path}/**/*" } #: Array[String]
      end

      #: () -> Array[Diagnostic]
      def run
        diagnostics = @graph.diagnostics

        @custom_rules.each do |rule_class|
          rule = rule_class.new(@graph, config: @config)
          rule.lint
          diagnostics.concat(rule.diagnostics)
        end

        # Make sure we have the forward/back slash at the end to avoid accidentally matching sibling directories that
        # share a prefix.
        workspace_path = File.join(@graph.workspace_path, "")

        # Filter out diagnostics that are excluded, inside dependencies or otherwise not a part of the workspace.
        diagnostics.select! do |diagnostic|
          rule = diagnostic.rule
          next false unless @config.rule_enabled?(rule)

          excluded_patterns = @config.excludes_for(rule).map { |pattern| "#{workspace_path}#{pattern}" }
          excluded_patterns.concat(@dependency_patterns)

          path = diagnostic.location.to_file_path

          path.start_with?(workspace_path) &&
            excluded_patterns.none? { |p| File.fnmatch?(p, path, Helpers::PathHelpers::EXCLUDE_FNMATCH_FLAGS) }
        rescue Location::NotFileUriError
          true
        end

        diagnostics
      end
    end
  end
end
