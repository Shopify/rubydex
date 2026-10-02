# frozen_string_literal: true

require "test_helper"
require "rubydex/linter/rule_test_case"
require "rubydex_linter/rules/prohibited_method_override"

module Rubydex
  module Linter
    module Rules
      class ProhibitedMethodOverrideTest < RuleTestCase
        def test_defaults_prohibit_both_singleton_definition_forms_but_allow_instance_methods
          assert_diagnostics(<<~RUBY)
            class Example
              def extend; end

              def self.extend; end
                       ^^^^^^ Prohibited override of `Kernel#extend()`
            end

            class Other
              class << self
                def extend; end
                    ^^^^^^ Prohibited override of `Kernel#extend()`
              end
            end
          RUBY
        end

        def test_instance_level_prohibits_direct_instance_definitions_only
          configure("Kernel#extend()" => "instance")

          assert_diagnostics(<<~RUBY)
            class Example
              def extend; end
                  ^^^^^^ Prohibited override of `Kernel#extend()`

              def self.extend; end
            end
          RUBY
        end

        def test_any_level_prohibits_direct_instance_and_singleton_definitions
          configure("Kernel#extend()" => "any")

          assert_diagnostics(<<~RUBY)
            class Example
              def extend; end
                  ^^^^^^ Prohibited override of `Kernel#extend()`

              def self.extend; end
                       ^^^^^^ Prohibited override of `Kernel#extend()`
            end
          RUBY
        end

        def test_instance_level_prohibits_include_and_prepend_but_allows_extend
          configure("Kernel#extend()" => "instance")

          assert_diagnostics(<<~RUBY)
            module Overrides
              def extend; end
                  ^^^^^^ `Included` mixes `Kernel#extend()` from `Overrides`
                  ^^^^^^ `Prepended` mixes `Kernel#extend()` from `Overrides`
            end

            class Included
              include Overrides
                      ^^^^^^^^^ Prohibited override of `Kernel#extend()`
            end

            class Prepended
              prepend Overrides
                      ^^^^^^^^^ Prohibited override of `Kernel#extend()`
            end

            class Extended
              extend Overrides
            end
          RUBY
        end

        def test_singleton_level_prohibits_extend_and_mixins_in_the_singleton_body
          assert_diagnostics(<<~RUBY)
            module Overrides
              def extend; end
                  ^^^^^^ `Extended` mixes `Kernel#extend()` from `Overrides`
                  ^^^^^^ `Included::<Included>` mixes `Kernel#extend()` from `Overrides`
                  ^^^^^^ `Prepended::<Prepended>` mixes `Kernel#extend()` from `Overrides`
            end

            class Extended
              extend Overrides
                     ^^^^^^^^^ Prohibited override of `Kernel#extend()`
            end

            class Included
              class << self
                include Overrides
                        ^^^^^^^^^ Prohibited override of `Kernel#extend()`
              end
            end

            class Prepended
              class << self
                prepend Overrides
                        ^^^^^^^^^ Prohibited override of `Kernel#extend()`
              end
            end

            class InstanceMethods
              include Overrides
            end
          RUBY
        end

        def test_any_level_prohibits_instance_and_singleton_mixins
          configure("Kernel#extend()" => "any")

          assert_diagnostics(<<~RUBY)
            module Overrides
              def extend; end
                  ^^^^^^ `Extended` mixes `Kernel#extend()` from `Overrides`
                  ^^^^^^ `Included` mixes `Kernel#extend()` from `Overrides`
            end

            class Included
              include Overrides
                      ^^^^^^^^^ Prohibited override of `Kernel#extend()`
            end

            class Extended
              extend Overrides
                     ^^^^^^^^^ Prohibited override of `Kernel#extend()`
            end
          RUBY
        end

        def test_only_prohibits_overrides_of_the_configured_owner
          configure("Parent#perform()" => "any")

          assert_diagnostics(<<~RUBY)
            class Parent
              def perform; end
            end

            class Child < Parent
              def perform; end
                  ^^^^^^^ Prohibited override of `Parent#perform()`
            end

            class Unrelated
              def perform; end
            end

            module Overrides
              def perform; end
            end

            class Other
              include Overrides
            end
          RUBY
        end

        def test_allows_definitions_after_the_protected_owner_in_the_ancestor_chain
          configure("Protected#perform()" => "any")

          assert_no_diagnostics(<<~RUBY)
            module Protected
              def perform; end
            end

            module Overrides
              def perform; end
            end

            class Example
              # Doesn't actually override `Protected#perform()` because it appears later in the ancestor chain
              include Overrides
              prepend Protected

              def perform; end
            end
          RUBY
        end

        def test_custom_restrictions_merge_with_defaults_and_can_disable_one_default
          configure("Kernel#extend()" => false, "Kernel#perform()" => "instance")

          assert_diagnostics(<<~RUBY)
            module Kernel
              def perform; end
            end

            class Example
              def self.extend; end

              def self.private; end
                       ^^^^^^^ Prohibited override of `Module#private()`

              def perform; end
                  ^^^^^^^ Prohibited override of `Kernel#perform()`
            end
          RUBY
        end

        private

        def configure(options)
          @rule_config = LinterConfig.new(
            rule_class.rule_name => RuleConfig.new(rule_class.rule_name, true, [], nil, options),
          )
        end
      end
    end
  end
end
