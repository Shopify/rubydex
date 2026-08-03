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
    result = Rubydex::Linter::Runner.new(Rubydex::Graph.new, rules: [WarningRule]).run
    diagnostic = result.diagnostics.fetch(0)

    assert_equal("WarningRule", diagnostic.rule)
    assert_equal("A warning.", diagnostic.message)
    assert_equal(Rubydex::Severity::Warning, diagnostic.severity)
    assert_equal(["Related context."], diagnostic.related_information.map(&:message))
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

    result = Rubydex::Linter::Runner.new(graph, rules: [SilentRule]).run

    assert_equal(["parse-error", "parse-error"], result.diagnostics.map(&:rule))
    assert(result.diagnostics.all? { |diagnostic| diagnostic.severity == Rubydex::Severity::Information })
    assert_predicate(result, :success?)
  end

  def test_runner_requires_rules
    error = assert_raises(ArgumentError) do
      Rubydex::Linter::Runner.new(Rubydex::Graph.new, rules: [])
    end

    assert_equal("At least one linter rule is required", error.message)
  end

  def test_runner_filters_diagnostics_outside_the_workspace
    with_context do |context|
      context.write!("workspace/inside.rb")
      context.write!("workspace-other/file.rb")
      graph = Rubydex::Graph.configure_for_workspace(context.absolute_path_to("workspace"))

      result = Rubydex::Linter::Runner.new(graph, rules: [OutsideWorkspaceRule]).run

      assert_empty(result.diagnostics)
    end
  end

  private

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
