# frozen_string_literal: true

require "test_helper"
require "helpers/context"

class ConfigTest < Minitest::Test
  include Test::Helpers::WithContext

  def test_load_returns_an_empty_configuration_for_a_workspace_without_a_config_file
    with_context do |context|
      config = Rubydex::Config.load(context.absolute_path)
      assert_equal(context.absolute_path, config.workspace_path)
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
end
