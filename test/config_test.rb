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
