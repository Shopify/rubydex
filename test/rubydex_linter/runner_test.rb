# frozen_string_literal: true

require "test_helper"
require "helpers/context"
require "rubydex/linter"

module Rubydex
  module Linter
    class RunnerTest < Minitest::Test
      include Test::Helpers::WithContext

      def test_built_in_diagnostics_can_be_filtered_through_config
        with_context do |context|
          context.write!("rubydex.toml", <<~TOML)
            [linter.rules.ParseError]
            exclude = ["**/foo.rb"]
          TOML
          graph = index(context)

          config = Config.load(context.absolute_path)
          diagnostics = Runner.new(graph, custom_rules: [], config: config.linter).run
          assert_empty(diagnostics)
        end
      end

      def test_built_in_diagnostics_can_be_disabled
        with_context do |context|
          context.write!("rubydex.toml", <<~TOML)
            [linter.rules.ParseError]
            enabled = false
          TOML
          graph = index(context)

          config = Config.load(context.absolute_path)
          diagnostics = Runner.new(graph, custom_rules: [], config: config.linter).run
          assert_empty(diagnostics)
        end
      end

      def test_changing_severity_of_built_in_diagnostics
        with_context do |context|
          context.write!("rubydex.toml", <<~TOML)
            [linter.rules.ParseError]
            severity = "hint"
          TOML
          graph = index(context)
          config = Config.load(context.absolute_path)

          diagnostics = Runner.new(graph, custom_rules: [], config: config.linter).run
          rules = diagnostics.map(&:rule).uniq
          assert_equal([Severity::Hint], rules.map { |rule| rule.severity(config.linter) })
        end
      end

      private

      #: (Test::Helpers::Context) -> Graph
      def index(context)
        graph = Graph.configure_for_workspace(context.absolute_path)

        context.write!("foo.rb", "module Foo")
        graph.index_all([context.absolute_path_to("foo.rb")])
        graph.resolve
        graph
      end
    end
  end
end
