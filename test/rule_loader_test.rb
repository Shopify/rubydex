# frozen_string_literal: true

require "test_helper"
require "helpers/context"
require "rubydex/linter"

class RuleLoaderTest < Minitest::Test
  include Test::Helpers::WithContext

  def test_load_wraps_rule_file_errors
    with_context do |context|
      rule_file = "rubydex_linter/rules/broken_rule.rb"
      context.write!(rule_file, "class BrokenRule <\n")

      error = with_bundle_gemfile(nil) do
        assert_raises(Rubydex::Linter::RuleLoadError) do
          Rubydex::Linter::RuleLoader.load(context.absolute_path)
        end
      end

      assert_match(/Unable to load linter rules from .*broken_rule\.rb/, error.message)
      assert_instance_of(SyntaxError, error.cause)
    end
  end

  private

  #: [R] (String?) { -> R } -> R
  def with_bundle_gemfile(value)
    previous = ENV["BUNDLE_GEMFILE"]
    ENV["BUNDLE_GEMFILE"] = value
    yield
  ensure
    ENV["BUNDLE_GEMFILE"] = previous
  end
end
