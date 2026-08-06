# frozen_string_literal: true

require "test_helper"
require "helpers/context"
require "mocha/minitest"
require "rubydex/linter"

module RuleLoaderTestFixtures; end

class RuleLoaderTest < Minitest::Test
  include Test::Helpers::WithContext

  def test_load_returns_project_and_bundled_rules_on_repeated_calls
    with_context do |context|
      project_rule = "rubydex_linter/rules/project_rule.rb"
      dependency_rule = "fake_gem/lib/rubydex_linter/rules/dependency_rule.rb"
      write_rule(context, project_rule, "ProjectRule")
      write_rule(context, dependency_rule, "DependencyRule")
      Gem.stubs(:find_latest_files).with(Rubydex::Linter::RuleLoader::RULE_GLOB).returns(
        [context.absolute_path_to(dependency_rule)],
      )

      with_bundle_gemfile(context.absolute_path_to("Gemfile")) do
        first_load = Rubydex::Linter::RuleLoader.load(context.absolute_path)
        second_load = Rubydex::Linter::RuleLoader.load(context.absolute_path)

        assert_equal(["DependencyRule", "ProjectRule"], first_load.map(&:rule_name).sort)
        assert_equal(first_load, second_load)
      end
    end
  end

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

  #: (Test::Helpers::Context, String, String) -> void
  def write_rule(context, path, class_name)
    context.write!(path, <<~RUBY)
      # frozen_string_literal: true

      module RuleLoaderTestFixtures
        class #{class_name} < Rubydex::Linter::Rule
          def severity = Rubydex::Severity::Error
          def lint; end
        end
      end
    RUBY
  end

  #: [R] (String?) { -> R } -> R
  def with_bundle_gemfile(value)
    previous = ENV["BUNDLE_GEMFILE"]
    ENV["BUNDLE_GEMFILE"] = value
    yield
  ensure
    ENV["BUNDLE_GEMFILE"] = previous
  end
end
