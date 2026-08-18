# frozen_string_literal: true

require "test_helper"
require "helpers/context"
require "mocha/minitest"
require "rubydex/linter"

class RuleLoaderTest < Minitest::Test
  include Test::Helpers::WithContext

  def test_paths_include_built_in_and_project_rules_without_bundler
    with_context do |context|
      project_rule_path = "workspace/rubydex_linter/rules/project_rule.rb"
      write_linter_rule(context, "RuleLoaderTestProjectPathRule", path: project_rule_path)

      paths = with_bundle_gemfile(nil) do
        Rubydex::Linter::RuleLoader.paths(context.absolute_path_to("workspace"))
      end

      assert_includes(paths, File.expand_path("../lib/rubydex_linter/rules/rule_structure.rb", __dir__))
      assert_includes(paths, context.absolute_path_to(project_rule_path))
    end
  end

  def test_paths_include_rules_from_bundled_dependencies
    with_context do |context|
      dependency_rule_path = "fake_gem/lib/rubydex_linter/rules/dependency_rule.rb"
      write_linter_rule(context, "RuleLoaderTestDependencyPathRule", path: dependency_rule_path)
      Gem.expects(:find_latest_files)
        .with("rubydex_linter/rules/**/*.rb")
        .returns([context.absolute_path_to(dependency_rule_path)])

      paths = with_bundle_gemfile(context.absolute_path_to("Gemfile")) do
        Rubydex::Linter::RuleLoader.paths(context.absolute_path_to("workspace"))
      end

      assert_includes(paths, context.absolute_path_to(dependency_rule_path))
    end
  end

  def test_paths_remove_built_in_rules_rediscovered_through_bundler
    with_context do |context|
      built_in_rule_path = File.expand_path("../lib/rubydex_linter/rules/rule_structure.rb", __dir__)
      Gem.expects(:find_latest_files)
        .with("rubydex_linter/rules/**/*.rb")
        .returns([built_in_rule_path])

      paths = with_bundle_gemfile(context.absolute_path_to("Gemfile")) do
        Rubydex::Linter::RuleLoader.paths(context.absolute_path)
      end

      assert_equal(1, paths.count(built_in_rule_path))
    end
  end

  def test_built_in_rules_are_available_without_bundler
    with_context do |context|
      with_bundle_gemfile(nil) do
        Rubydex::Linter::RuleLoader.load(context.absolute_path)
      end

      assert_includes(Rubydex::Linter::CustomRule.subclasses, Rubydex::Linter::Rules::RuleStructure)
    end
  end

  def test_loads_project_rules
    with_context do |context|
      write_linter_rule(context, "RuleLoaderTestProjectRule")

      with_bundle_gemfile(nil) do
        Rubydex::Linter::RuleLoader.load(context.absolute_path)
      end

      assert_includes(Rubydex::Linter::CustomRule.subclasses.map(&:rule_name), "RuleLoaderTestProjectRule")
    end
  end

  def test_loads_rules_from_bundled_dependencies
    with_context do |context|
      rule_path = "fake_gem/lib/rubydex_linter/rules/dependency_rule.rb"
      write_linter_rule(context, "RuleLoaderTestDependencyRule", path: rule_path)
      Gem.expects(:find_latest_files)
        .with("rubydex_linter/rules/**/*.rb")
        .returns([context.absolute_path_to(rule_path)])

      with_bundle_gemfile(context.absolute_path_to("Gemfile")) do
        Rubydex::Linter::RuleLoader.load(context.absolute_path_to("workspace"))
      end

      assert_includes(Rubydex::Linter::CustomRule.subclasses.map(&:rule_name), "RuleLoaderTestDependencyRule")
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

  #: (Test::Helpers::Context, String, ?path: String) -> void
  def write_linter_rule(context, class_name, path: "rubydex_linter/rules/rule.rb")
    context.write!(path, <<~RUBY)
      class Rubydex::Linter::Rules::#{class_name} < Rubydex::Linter::CustomRule; end
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
