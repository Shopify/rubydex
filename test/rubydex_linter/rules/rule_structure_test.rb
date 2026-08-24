# frozen_string_literal: true

require "test_helper"
require "rubydex/linter/rule_test_case"
require "rubydex_linter/rules/rule_structure"

module Rubydex
  module Linter
    module Rules
      class RuleStructureTest < RuleTestCase
        BASE_RULE_SOURCE = <<~RUBY
          module Rubydex
            module Linter
              class CustomRule; end
              module Rules; end
            end
          end
        RUBY

        def setup
          super
          add_shared_source("lib/rubydex/linter/custom_rule.rb" => BASE_RULE_SOURCE)
        end

        def test_allows_one_rule_class_in_each_supported_rule_directory
          assert_no_diagnostics(
            "rubydex_linter/rules/project_rule.rb" => rule_source("ProjectRule"),
            "lib/rubydex_linter/rules/gem_rule.rb" => rule_source("GemRule"),
          )
        end

        def test_allows_rule_classes_in_nested_gem_rule_directories
          assert_no_diagnostics(
            "gems/example/lib/rubydex_linter/rules/nested_gem_rule.rb" => rule_source("NestedGemRule"),
          )
        end

        def test_allows_indirect_rule_subclasses_and_helper_classes
          assert_no_diagnostics(
            "rubydex_linter/rules/base_rule.rb" => rule_source("BaseRule"),
            "rubydex_linter/rules/indirect_rule.rb" => <<~RUBY,
              class Rubydex::Linter::Rules::Helper; end
              class Rubydex::Linter::Rules::IndirectRule < Rubydex::Linter::Rules::BaseRule; end
            RUBY
          )
        end

        def test_reports_multiple_rule_classes_in_one_file_when_one_is_reopened
          diagnostics = assert_diagnostics(
            "rubydex_linter/rules/first_rule.rb" => rule_source("FirstRule"),
            "rubydex_linter/rules/two_rules.rb" => <<~RUBY,
              class Rubydex::Linter::Rules::FirstRule; end
              ^{} Each rule file must define only one linter rule; found 2.
                                            ^^^^^^^^^ `Rubydex::Linter::Rules::FirstRule` is defined here.
              class Rubydex::Linter::Rules::SecondRule < Rubydex::Linter::CustomRule; end
                                            ^^^^^^^^^^ `Rubydex::Linter::Rules::SecondRule` is defined here.
            RUBY
          )

          assert_equal(1, diagnostics.length)
          assert_equal(
            [
              "`Rubydex::Linter::Rules::FirstRule` is defined here.",
              "`Rubydex::Linter::Rules::SecondRule` is defined here.",
            ],
            diagnostics.fetch(0).related_information.map(&:message).sort,
          )
        end

        def test_reports_a_rule_class_outside_the_rules_namespace
          assert_diagnostics(
            "rubydex_linter/rules/wrong_namespace.rb" => <<~RUBY,
              module ConsumerRules
                class WrongNamespace < Rubydex::Linter::CustomRule; end
                      ^^^^^^^^^^^^^^ `ConsumerRules::WrongNamespace` must be defined under `Rubydex::Linter::Rules`.
              end
            RUBY
          )
        end

        def test_reports_a_rule_class_outside_a_rule_directory
          assert_diagnostics(
            "app/rules/wrong_place.rb" => <<~RUBY,
              class Rubydex::Linter::Rules::WrongPlace < Rubydex::Linter::CustomRule; end
                                            ^^^^^^^^^^ `Rubydex::Linter::Rules::WrongPlace` must be defined under `rubydex_linter/rules/` or `lib/rubydex_linter/rules/`.
            RUBY
          )
        end

        def test_ignores_rule_fixture_classes_under_test_directories
          assert_no_diagnostics(
            "test/fixtures/fixture_rule.rb" => <<~RUBY,
              module RuleStructureTestFixtures
                class FixtureRule < Rubydex::Linter::CustomRule; end
              end
            RUBY
          )
        end

        private

        #: (String) -> String
        def rule_source(class_name)
          "class Rubydex::Linter::Rules::#{class_name} < Rubydex::Linter::CustomRule; end\n"
        end
      end
    end
  end
end
