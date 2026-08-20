# frozen_string_literal: true

require "test_helper"
require "helpers/context"
require "mocha/minitest"
require "rubydex/linter"

class LinterTest < Minitest::Test
  include Test::Helpers::WithContext

  class WarningRule < Rubydex::Linter::CustomRule
    class << self
      def default_severity = Rubydex::Severity::Warning
    end

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

  class ErrorRule < Rubydex::Linter::CustomRule
    class << self
      def default_severity = Rubydex::Severity::Error
    end

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

  class SilentRule < Rubydex::Linter::CustomRule
    class << self
      def default_severity = Rubydex::Severity::Hint
    end

    def lint; end
  end

  class OutsideWorkspaceRule < Rubydex::Linter::CustomRule
    class << self
      def default_severity = Rubydex::Severity::Information
    end

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

  class DependencyPathRule < Rubydex::Linter::CustomRule
    class << self
      def default_severity = Rubydex::Severity::Information
    end

    def lint
      path = File.join(graph.workspace_path, "vendor/bundle/gems/example.rb")
      path.prepend("/") if Gem.win_platform?
      uri = URI::File.build(path: path).to_s

      add_diagnostic(
        "Inside a dependency path.",
        Rubydex::Location.new(uri: uri, start_line: 0, end_line: 0, start_column: 0, end_column: 1),
      )
    end
  end

  class ExcludedPrimaryRule < Rubydex::Linter::CustomRule
    class << self
      def default_severity = Rubydex::Severity::Information
    end

    def lint
      add_diagnostic("Excluded primary location.", workspace_location("components/legacy/example.rb"))
    end

    private

    def workspace_location(relative_path)
      path = File.join(graph.workspace_path, relative_path)
      path.prepend("/") if Gem.win_platform?
      Rubydex::Location.new(
        uri: URI::File.build(path: path).to_s,
        start_line: 0,
        end_line: 0,
        start_column: 0,
        end_column: 1,
      )
    end
  end

  class ExcludedRelatedInformationRule < ExcludedPrimaryRule
    def lint
      add_diagnostic(
        "Included primary location.",
        workspace_location("components/current/example.rb"),
        related_information: [
          Rubydex::RelatedInformation.new(
            "Excluded related location.",
            workspace_location("components/legacy/example.rb"),
          ),
        ],
      )
    end
  end

  def test_runner_builds_diagnostics_with_the_rule_and_related_information
    diagnostics = Rubydex::Linter::Runner.new(
      Rubydex::Graph.new,
      custom_rules: [WarningRule],
      config: linter_config,
    ).run
    diagnostic = diagnostics.fetch(0)

    assert_equal(WarningRule, diagnostic.rule)
    assert_equal("A warning.", diagnostic.message)
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
      custom_rules: [WarningRule, ErrorRule],
      config:,
    )

    assert_equal([ErrorRule], runner.custom_rules)
    assert_equal([ErrorRule], runner.run.map(&:rule))
  end

  def test_runner_allows_every_rule_to_be_disabled
    config = linter_config("WarningRule" => false)
    runner = Rubydex::Linter::Runner.new(Rubydex::Graph.new, custom_rules: [WarningRule], config:)

    assert_empty(runner.custom_rules)
    assert_empty(runner.run)
  end

  def test_runner_includes_native_graph_diagnostics
    graph = Rubydex::Graph.new
    path = File.join(graph.workspace_path, "broken.rb")
    path.prepend("/") if Gem.win_platform?
    graph.index_source(URI::File.build(path: path).to_s, "class Broken", "ruby")

    diagnostics = Rubydex::Linter::Runner.new(graph, custom_rules: [SilentRule], config: linter_config).run

    assert_equal([Rubydex::Rules::ParseError, Rubydex::Rules::ParseError], diagnostics.map(&:rule))
  end

  def test_runner_accepts_no_rules
    diagnostics = Rubydex::Linter::Runner.new(Rubydex::Graph.new, custom_rules: [], config: linter_config).run

    assert_empty(diagnostics)
  end

  def test_runner_filters_diagnostics_outside_the_workspace
    with_context do |context|
      context.write!("workspace/inside.rb")
      context.write!("workspace-other/file.rb")
      graph = Rubydex::Graph.configure_for_workspace(context.absolute_path_to("workspace"))

      diagnostics = Rubydex::Linter::Runner.new(graph, custom_rules: [OutsideWorkspaceRule], config: linter_config).run

      assert_empty(diagnostics)
    end
  end

  def test_runner_filters_diagnostics_under_dependency_paths
    with_context do |context|
      context.write!("workspace/inside.rb")
      graph = Rubydex::Graph.configure_for_workspace(context.absolute_path_to("workspace"))
      dependency_path = context.absolute_path_to("workspace/vendor/bundle")
      Gem.stubs(:path).returns([dependency_path])

      diagnostics = Rubydex::Linter::Runner.new(graph, custom_rules: [DependencyPathRule], config: linter_config).run

      assert_empty(diagnostics)
    end
  end

  def test_runner_filters_native_diagnostics_under_dependency_paths
    with_context do |context|
      context.write!("workspace/inside.rb")
      graph = Rubydex::Graph.configure_for_workspace(context.absolute_path_to("workspace"))
      dependency_path = context.absolute_path_to("workspace/.dev/gem")
      graph.index_source(context.uri_to("workspace/.dev/gem/broken.rb"), "class Broken", "ruby")
      Gem.stubs(:path).returns([dependency_path])

      diagnostics = Rubydex::Linter::Runner.new(graph, custom_rules: [SilentRule], config: linter_config).run

      assert_empty(diagnostics)
    end
  end

  def test_runner_filters_a_diagnostic_when_its_primary_location_matches_a_rule_exclude
    with_context do |context|
      context.write!("workspace/inside.rb")
      graph = Rubydex::Graph.configure_for_workspace(context.absolute_path_to("workspace"))
      config = configured_linter_config("ExcludedPrimaryRule", exclude_patterns: ["components/legacy/**"])

      diagnostics = Rubydex::Linter::Runner.new(graph, custom_rules: [ExcludedPrimaryRule], config:).run

      assert_empty(diagnostics)
    end
  end

  def test_runner_keeps_a_diagnostic_when_only_related_information_matches_a_rule_exclude
    with_context do |context|
      context.write!("workspace/inside.rb")
      graph = Rubydex::Graph.configure_for_workspace(context.absolute_path_to("workspace"))
      config = configured_linter_config(
        "ExcludedRelatedInformationRule",
        exclude_patterns: ["components/legacy/**"],
      )

      diagnostics = Rubydex::Linter::Runner.new(graph, custom_rules: [ExcludedRelatedInformationRule], config:).run

      assert_equal(["Included primary location."], diagnostics.map(&:message))
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

      diagnostics = Rubydex::Linter::Runner.new(graph, custom_rules: [SilentRule], config: linter_config).run

      expected_uri = context.uri_to("workspace/link/broken.rb")
      assert_equal([expected_uri, expected_uri], diagnostics.map { |diagnostic| diagnostic.location.uri })
    end
  end

  private

  #: (?Hash[String, bool] rules) -> Rubydex::LinterConfig
  def linter_config(rules = {})
    Rubydex::LinterConfig.new(
      rules.to_h { |name, enabled| [name, Rubydex::RuleConfig.new(name, enabled)] },
    )
  end

  #: (String, ?enabled: bool, ?exclude_patterns: Array[String]) -> Rubydex::LinterConfig
  def configured_linter_config(rule_name, enabled: true, exclude_patterns: [])
    Rubydex::LinterConfig.new(
      rule_name => Rubydex::RuleConfig.new(rule_name, enabled, exclude_patterns),
    )
  end
end
