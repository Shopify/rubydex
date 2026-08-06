# frozen_string_literal: true

require "test_helper"
require "helpers/context"
require "rubydex/linter"

class LinterTest < Minitest::Test
  include Test::Helpers::WithContext

  class WarningRule < Rubydex::Linter::Rule
    def severity = Rubydex::Severity::Warning

    def lint
      add_diagnostic(
        "A warning.",
        location("untitled:warning"),
        related_information: [
          Rubydex::RelatedInformation.new("Related context.", location("untitled:related")),
        ],
      )
    end

    private

    def location(uri)
      Rubydex::Location.new(uri: uri, start_line: 0, end_line: 0, start_column: 0, end_column: 1)
    end
  end

  class ErrorRule < Rubydex::Linter::Rule
    def severity = Rubydex::Severity::Error

    def lint
      add_diagnostic(
        "An error.",
        Rubydex::Location.new(
          uri: "untitled:error",
          start_line: 0,
          end_line: 0,
          start_column: 0,
          end_column: 1,
        ),
      )
    end
  end

  class SilentRule < Rubydex::Linter::Rule
    def severity = Rubydex::Severity::Hint
    def lint; end
  end

  class OutsideWorkspaceRule < Rubydex::Linter::Rule
    def severity = Rubydex::Severity::Information

    def lint
      sibling = File.join(File.dirname(graph.workspace_path), "#{File.basename(graph.workspace_path)}-other", "file.rb")
      sibling.prepend("/") if Gem.win_platform?
      uri = URI::File.build(path: sibling).to_s

      add_diagnostic(
        "Outside the workspace.",
        Rubydex::Location.new(uri: uri, start_line: 0, end_line: 0, start_column: 0, end_column: 1),
      )
    end
  end

  def test_runner_builds_diagnostics_with_rule_severity_and_related_information
    result = Rubydex::Linter::Runner.new(Rubydex::Graph.new, rules: [WarningRule], config: linter_config).run
    diagnostic = result.diagnostics.fetch(0)

    assert_equal("WarningRule", diagnostic.rule)
    assert_equal("A warning.", diagnostic.message)
    assert_equal(Rubydex::Severity::Warning, diagnostic.severity)
    assert_equal(["Related context."], diagnostic.related_information.map(&:message))
  end

  def test_rule_exposes_linter_config
    config = linter_config
    rule = WarningRule.new(Rubydex::Graph.new, config:)

    assert_same(config, rule.config)
  end

  def test_runner_drops_disabled_rules
    config = linter_config("WarningRule" => false)
    runner = Rubydex::Linter::Runner.new(
      Rubydex::Graph.new,
      rules: [WarningRule, ErrorRule],
      config:,
    )

    assert_equal([ErrorRule], runner.rules)
    assert_equal(["ErrorRule"], runner.run.diagnostics.map(&:rule))
  end

  def test_runner_allows_every_rule_to_be_disabled
    config = linter_config("WarningRule" => false)
    runner = Rubydex::Linter::Runner.new(Rubydex::Graph.new, rules: [WarningRule], config:)

    assert_empty(runner.rules)
    assert_predicate(runner.run, :success?)
  end

  def test_result_fails_only_for_error_diagnostics
    non_error_diagnostics = [
      Rubydex::Severity::Warning,
      Rubydex::Severity::Information,
      Rubydex::Severity::Hint,
    ].map { |severity| diagnostic(severity) }

    assert_predicate(Rubydex::Linter::Result.new(non_error_diagnostics), :success?)
    refute_predicate(
      Rubydex::Linter::Result.new([*non_error_diagnostics, diagnostic(Rubydex::Severity::Error)]),
      :success?,
    )
  end

  def test_runner_includes_native_graph_diagnostics
    graph = Rubydex::Graph.new
    path = File.join(graph.workspace_path, "broken.rb")
    path.prepend("/") if Gem.win_platform?
    graph.index_source(URI::File.build(path: path).to_s, "class Broken", "ruby")

    result = Rubydex::Linter::Runner.new(graph, rules: [SilentRule], config: linter_config).run

    assert_equal(["parse-error", "parse-error"], result.diagnostics.map(&:rule))
    assert(result.diagnostics.all? { |diagnostic| diagnostic.severity == Rubydex::Severity::Information })
    assert_predicate(result, :success?)
  end

  def test_runner_requires_rules
    error = assert_raises(ArgumentError) do
      Rubydex::Linter::Runner.new(Rubydex::Graph.new, rules: [], config: linter_config)
    end

    assert_equal("At least one linter rule is required", error.message)
  end

  def test_runner_filters_diagnostics_outside_the_workspace
    with_context do |context|
      context.write!("workspace/inside.rb")
      context.write!("workspace-other/file.rb")
      graph = Rubydex::Graph.configure_for_workspace(context.absolute_path_to("workspace"))

      result = Rubydex::Linter::Runner.new(graph, rules: [OutsideWorkspaceRule], config: linter_config).run

      assert_empty(result.diagnostics)
    end
  end

  def test_runner_keeps_diagnostics_indexed_through_a_symlinked_workspace_path
    with_context do |context|
      context.write!("workspace/inside.rb")
      context.write!("outside/broken.rb", "class Broken")
      link = context.absolute_path_to("workspace/link")
      File.symlink(context.absolute_path_to("outside"), link)
      graph = Rubydex::Graph.configure_for_workspace(context.absolute_path_to("workspace"))
      graph.index_all([link])

      result = Rubydex::Linter::Runner.new(graph, rules: [SilentRule], config: linter_config).run

      expected_uri = context.uri_to("workspace/link/broken.rb")
      assert_equal([expected_uri, expected_uri], result.diagnostics.map { |diagnostic| diagnostic.location.uri })
    end
  end

  private

  #: (?Hash[String, bool] rules) -> Rubydex::LinterConfig
  def linter_config(rules = {})
    Rubydex::LinterConfig.new(
      rules.to_h { |name, enabled| [name, Rubydex::RuleConfig.new(name, enabled)] },
    )
  end

  #: (singleton(Rubydex::Severity::Base) severity) -> Rubydex::Diagnostic
  def diagnostic(severity)
    Rubydex::Diagnostic.new(
      rule: "TestRule",
      message: "Test diagnostic.",
      location: Rubydex::Location.new(
        uri: "untitled:test",
        start_line: 0,
        end_line: 0,
        start_column: 0,
        end_column: 1,
      ),
      severity: severity,
    )
  end
end
