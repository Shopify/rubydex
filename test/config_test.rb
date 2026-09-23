# frozen_string_literal: true

require "test_helper"
require "helpers/context"

class ConfigTest < Minitest::Test
  include Test::Helpers::WithContext

  def test_load_returns_an_empty_configuration_for_a_workspace_without_a_config_file
    with_context do |context|
      config = Rubydex::Config.load(context.absolute_path)
      assert_equal(context.absolute_path, config.workspace_path)
      assert_empty(config.linter.rules)
      assert_empty(config.dead_code.exclude_patterns)
    end
  end

  def test_load_raises_when_the_workspace_does_not_exist
    with_context do |context|
      error = assert_raises(Rubydex::ConfigError) do
        Rubydex::Config.load(context.absolute_path_to("typo"))
      end

      assert_match(/typo/, error.message)
    end
  end

  def test_load_raises_on_malformed_toml
    with_context do |context|
      context.write!("rubydex.toml", "exclude = [\n")

      error = assert_raises(Rubydex::ConfigError) do
        Rubydex::Config.load(context.absolute_path)
      end

      assert_match(/rubydex.toml/, error.message)
    end
  end

  def test_load_raises_on_an_unknown_top_level_setting
    with_context do |context|
      context.write!("rubydex.toml", "excludes = [\"vendor\"]\n")

      error = assert_raises(Rubydex::ConfigError) do
        Rubydex::Config.load(context.absolute_path)
      end

      assert_match(/excludes/, error.message)
    end
  end

  def test_load_raises_on_an_unknown_graph_setting
    with_context do |context|
      context.write!("rubydex.toml", "[graph]\nexcludes = [\"vendor\"]\n")

      error = assert_raises(Rubydex::ConfigError) do
        Rubydex::Config.load(context.absolute_path)
      end

      assert_match(/graph.excludes/, error.message)
    end
  end

  def test_load_raises_when_the_path_is_not_a_string
    assert_raises(TypeError) { Rubydex::Config.load(123) }
  end

  def test_dead_code_returns_frozen_exclusion_patterns
    with_context do |context|
      patterns = ["app/generated/**", "lib/caf\u00e9.rb"]
      context.write!("rubydex.toml", <<~TOML)
        [dead-code]
        exclude = ["app/generated/**", "lib/caf\u00e9.rb"]
      TOML

      dead_code = Rubydex::Config.load(context.absolute_path).dead_code

      assert_instance_of(Rubydex::DeadCodeConfig, dead_code)
      assert_equal(patterns, dead_code.exclude_patterns)
      assert_predicate(dead_code, :frozen?)
      assert_predicate(dead_code.exclude_patterns, :frozen?)
      dead_code.exclude_patterns.each do |pattern|
        assert_predicate(pattern, :frozen?)
        assert_equal(Encoding::UTF_8, pattern.encoding)
      end
    end
  end

  def test_dead_code_defaults_to_no_exclusions_when_section_is_empty
    with_context do |context|
      context.write!("rubydex.toml", "[dead-code]\n")

      assert_empty(Rubydex::Config.load(context.absolute_path).dead_code.exclude_patterns)
    end
  end

  def test_load_raises_on_an_unknown_dead_code_setting
    with_context do |context|
      context.write!("rubydex.toml", "[dead-code]\nexcludes = [\"generated/**\"]\n")

      error = assert_raises(Rubydex::ConfigError) do
        Rubydex::Config.load(context.absolute_path)
      end

      assert_match(/dead-code.excludes/, error.message)
    end
  end

  def test_load_raises_when_dead_code_exclude_is_not_an_array
    with_context do |context|
      context.write!("rubydex.toml", "[dead-code]\nexclude = \"generated/**\"\n")

      error = assert_raises(Rubydex::ConfigError) do
        Rubydex::Config.load(context.absolute_path)
      end

      assert_match(/dead-code/, error.message)
    end
  end

  def test_load_raises_when_dead_code_exclude_contains_a_non_string
    with_context do |context|
      context.write!("rubydex.toml", "[dead-code]\nexclude = [\"generated/**\", 1]\n")

      error = assert_raises(Rubydex::ConfigError) do
        Rubydex::Config.load(context.absolute_path)
      end

      assert_match(/dead-code/, error.message)
    end
  end

  def test_graph_dead_code_config_defaults_to_no_exclusions
    assert_empty(Rubydex::Graph.new.dead_code_config.exclude_patterns)
  end

  def test_graph_uses_its_loaded_dead_code_configuration_snapshot
    with_context do |context|
      context.write!("rubydex.toml", "[dead-code]\nexclude = [\"original/**\"]\n")
      config = Rubydex::Config.load(context.absolute_path)
      graph = Rubydex::Graph.new
      graph.load_config(config)
      snapshot = graph.dead_code_config

      context.write!("rubydex.toml", "[dead-code]\nexclude = [\"updated/**\"]\n")

      assert_equal(["original/**"], config.dead_code.exclude_patterns)
      assert_equal(["original/**"], graph.dead_code_config.exclude_patterns)

      graph.load_config(Rubydex::Config.load(context.absolute_path))

      assert_equal(["updated/**"], graph.dead_code_config.exclude_patterns)
      assert_equal(["original/**"], snapshot.exclude_patterns)

      context.write!("rubydex.toml", "")
      graph.load_config(Rubydex::Config.load(context.absolute_path))

      assert_empty(graph.dead_code_config.exclude_patterns)
    end
  end

  def test_configure_for_workspace_loads_dead_code_exclusions_on_frozen_graphs
    with_context do |context|
      context.write!("rubydex.toml", "[dead-code]\nexclude = [\"generated/**\"]\n")
      graph = Rubydex::Graph.configure_for_workspace(context.absolute_path).freeze
      dead_code = graph.dead_code_config

      assert_equal(["generated/**"], dead_code.exclude_patterns)
      assert_predicate(dead_code, :frozen?)
      assert_predicate(dead_code.exclude_patterns, :frozen?)

      context.write!("rubydex.toml", "[dead-code]\nexclude = [\"reloaded/**\"]\n")
      graph.load_config(Rubydex::Config.load(context.absolute_path))

      assert_equal(["reloaded/**"], graph.dead_code_config.exclude_patterns)
    end
  end

  def test_linter_returns_the_configured_rules
    with_context do |context|
      context.write!("rubydex.toml", <<~TOML)
        [linter.rules.Something]
        severity = "warning"
        exclude = ["components/legacy/**", "test/fixtures/**"]

        [linter.rules.Other]
        enabled = false
      TOML

      config = Rubydex::Config.load(context.absolute_path)
      rules = config.linter.rules

      assert_equal(["Other", "Something"], rules.keys.sort)
      assert_predicate(rules, :frozen?)
      assert_predicate(rules.fetch("Something"), :enabled?)
      assert_equal(
        ["components/legacy/**", "test/fixtures/**"],
        rules.fetch("Something").exclude_patterns,
      )
      assert_equal(Rubydex::Severity::Warning, rules.fetch("Something").severity)
      refute_predicate(rules.fetch("Other"), :enabled?)
      assert_empty(rules.fetch("Other").exclude_patterns)
      assert_nil(rules.fetch("Other").severity)
    end
  end

  def test_linter_maps_every_configured_severity
    with_context do |context|
      context.write!("rubydex.toml", <<~TOML)
        [linter.rules.ErrorRule]
        severity = "error"

        [linter.rules.WarningRule]
        severity = "warning"

        [linter.rules.InformationRule]
        severity = "information"

        [linter.rules.HintRule]
        severity = "hint"
      TOML

      rules = Rubydex::Config.load(context.absolute_path).linter.rules

      {
        "ErrorRule" => Rubydex::Severity::Error,
        "WarningRule" => Rubydex::Severity::Warning,
        "InformationRule" => Rubydex::Severity::Information,
        "HintRule" => Rubydex::Severity::Hint,
      }.each do |rule_name, severity|
        assert_equal(severity, rules.fetch(rule_name).severity)
      end
    end
  end
end
