# frozen_string_literal: true

require "test_helper"
require "helpers/context"
require "mocha/minitest"
require "rubydex/linter"

module RuleLoaderTestFixtures
  class IntermediateRule < Rubydex::Linter::Rule
    def severity = Rubydex::Severity::Error
    def lint; end
  end
end

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

        rule_names = first_load.map(&:rule_name)
        assert_includes(rule_names, "DependencyRule")
        assert_includes(rule_names, "ProjectRule")
        assert_includes(rule_names, "RuleStructure")
        assert_equal(first_load, second_load)
      end
    end
  end

  def test_load_returns_built_in_rules_without_bundler
    with_context do |context|
      Gem.expects(:find_latest_files).never

      rules = with_bundle_gemfile(nil) do
        Rubydex::Linter::RuleLoader.load(context.absolute_path)
      end

      assert_includes(rules, Rubydex::Linter::Rules::RuleStructure)
    end
  end

  def test_load_returns_indirect_rule_subclasses
    with_context do |context|
      write_rule(
        context,
        "rubydex_linter/rules/indirect_rule.rb",
        "IndirectRule",
        superclass: "RuleLoaderTestFixtures::IntermediateRule",
      )

      rules = with_bundle_gemfile(nil) do
        Rubydex::Linter::RuleLoader.load(context.absolute_path)
      end

      assert_includes(rules, RuleLoaderTestFixtures::IndirectRule)
      refute_includes(rules, RuleLoaderTestFixtures::IntermediateRule)
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

  #: (Test::Helpers::Context, String, String, ?superclass: String) -> void
  def write_rule(context, path, class_name, superclass: "Rubydex::Linter::Rule")
    context.write!(path, <<~RUBY)
      # frozen_string_literal: true

      module RuleLoaderTestFixtures
        class #{class_name} < #{superclass}
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
