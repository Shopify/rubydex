# frozen_string_literal: true

require "test_helper"
require "helpers/context"
require "mocha/minitest"
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

  class DependencyPathRule < Rubydex::Linter::Rule
    def severity = Rubydex::Severity::Information

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

  class ExcludedPrimaryRule < Rubydex::Linter::Rule
    def severity = Rubydex::Severity::Information

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

  def test_rule_lists_built_in_rule_names
    assert_equal(
      [
        "parse-error",
        "parse-warning",
        "dynamic-constant-reference",
        "dynamic-singleton-definition",
        "dynamic-ancestor",
        "top-level-mixin-self",
        "invalid-constant-visibility",
        "invalid-method-visibility",
        "undefined-method-visibility-target",
        "undefined-constant-visibility-target",
      ],
      Rubydex::Linter::Rule.built_in_rules_names,
    )
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

  def test_configured_severity_overrides_the_rule_severity
    config = configured_linter_config("WarningRule", severity: Rubydex::Severity::Error)
    result = Rubydex::Linter::Runner.new(Rubydex::Graph.new, rules: [WarningRule], config:).run

    assert_equal(Rubydex::Severity::Error, result.diagnostics.fetch(0).severity)
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
    assert(result.diagnostics.all? { |diagnostic| diagnostic.severity == Rubydex::Severity::Error })
    refute_predicate(result, :success?)
  end

  def test_graph_does_not_add_disabled_diagnostics
    with_context do |context|
      context.write!("workspace/warning.rb", "unused = true")
      context.write!("workspace/error.rb", "class Broken")
      context.write!("workspace/rubydex.toml", <<~TOML)
        [linter.rules.parse-warning]
        enabled = false
      TOML
      config = Rubydex::Config.load(context.absolute_path_to("workspace"))
      graph = Rubydex::Graph.new
      graph.load_config(config)
      graph.index_all(context.glob("workspace/**/*.rb"))

      assert_equal(["parse-error", "parse-error"], graph.diagnostics.map(&:rule))

      result = Rubydex::Linter::Runner.new(graph, rules: [], config: config.linter).run

      assert_equal(["parse-error", "parse-error"], result.diagnostics.map(&:rule))
    end
  end

  def test_graph_does_not_add_diagnostics_from_excluded_paths
    with_context do |context|
      context.write!("workspace/components/legacy/example.rb", "unused = true")
      context.write!("workspace/components/current/example.rb", "unused = true")
      context.write!("workspace/rubydex.toml", <<~TOML)
        [linter.rules.parse-warning]
        exclude = ["components/legacy/**"]
      TOML
      config = Rubydex::Config.load(context.absolute_path_to("workspace"))
      graph = Rubydex::Graph.new
      graph.load_config(config)
      graph.index_all(context.glob("workspace/**/*.rb"))

      expected_uri = context.uri_to("workspace/components/current/example.rb")
      assert_equal([expected_uri], graph.diagnostics.map { |diagnostic| diagnostic.location.uri })

      result = Rubydex::Linter::Runner.new(graph, rules: [], config: config.linter).run

      assert_equal([expected_uri], result.diagnostics.map { |diagnostic| diagnostic.location.uri })
    end
  end

  def test_graph_applies_configured_diagnostic_severity
    with_context do |context|
      context.write!("workspace/example.rb", "unused = true")
      context.write!("workspace/rubydex.toml", <<~TOML)
        [linter.rules.parse-warning]
        severity = "error"
      TOML
      config = Rubydex::Config.load(context.absolute_path_to("workspace"))
      graph = Rubydex::Graph.new
      graph.load_config(config)
      graph.index_all(context.glob("workspace/**/*.rb"))

      diagnostic = graph.diagnostics.fetch(0)
      assert_equal("parse-warning", diagnostic.rule)
      assert_equal(Rubydex::Severity::Error, diagnostic.severity)

      result = Rubydex::Linter::Runner.new(graph, rules: [], config: config.linter).run
      refute_predicate(result, :success?)
    end
  end

  def test_runner_accepts_no_rules
    result = Rubydex::Linter::Runner.new(Rubydex::Graph.new, rules: [], config: linter_config).run

    assert_empty(result.diagnostics)
    assert_predicate(result, :success?)
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

  def test_runner_filters_diagnostics_under_dependency_paths
    with_context do |context|
      context.write!("workspace/inside.rb")
      graph = Rubydex::Graph.configure_for_workspace(context.absolute_path_to("workspace"))
      dependency_path = context.absolute_path_to("workspace/vendor/bundle")
      Gem.stubs(:path).returns([dependency_path])

      result = Rubydex::Linter::Runner.new(graph, rules: [DependencyPathRule], config: linter_config).run

      assert_empty(result.diagnostics)
    end
  end

  def test_runner_filters_graph_diagnostics_under_dependency_paths
    with_context do |context|
      context.write!("workspace/vendor/bundle/gems/broken.rb", "class Broken")
      graph = Rubydex::Graph.configure_for_workspace(context.absolute_path_to("workspace"))
      graph.index_all(context.glob("workspace/**/*.rb"))
      dependency_path = context.absolute_path_to("workspace/vendor/bundle")
      Gem.stubs(:path).returns([dependency_path])

      assert_equal(["parse-error", "parse-error"], graph.diagnostics.map(&:rule))

      result = Rubydex::Linter::Runner.new(graph, rules: [], config: linter_config).run

      assert_empty(result.diagnostics)
    end
  end

  def test_rule_does_not_add_a_diagnostic_from_an_excluded_path
    with_context do |context|
      context.write!("workspace/inside.rb")
      graph = Rubydex::Graph.configure_for_workspace(context.absolute_path_to("workspace"))
      config = configured_linter_config("ExcludedPrimaryRule", exclude_patterns: ["components/legacy/**"])
      rule = ExcludedPrimaryRule.new(graph, config:)

      rule.lint

      assert_empty(rule.diagnostics)
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

      result = Rubydex::Linter::Runner.new(graph, rules: [ExcludedRelatedInformationRule], config:).run

      assert_equal(["Included primary location."], result.diagnostics.map(&:message))
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

  #: (
  #|   String,
  #|   ?enabled: bool,
  #|   ?exclude_patterns: Array[String],
  #|   ?severity: singleton(Rubydex::Severity::Base)?,
  #| ) -> Rubydex::LinterConfig
  def configured_linter_config(rule_name, enabled: true, exclude_patterns: [], severity: nil)
    Rubydex::LinterConfig.new(
      rule_name => Rubydex::RuleConfig.new(rule_name, enabled, exclude_patterns, severity),
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
