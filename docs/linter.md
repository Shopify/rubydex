# Rubydex linter

Rubydex ships with a built-in linter and infrastructure to write custom rules. It includes the built-in rules that are part of the core analysis by default, such as parse errors, dynamic constant references, and others that will be available in the future like undefined constant, cyclic ancestor, superclass mismatch and so on. The goal is to provide a unified interface for linting the code from both a base analysis and custom rule perspective.

## Usage

### Commands

The linter includes the following commands:

```bash
# Lint the codebase.
bundle exec rdx lint

# Explain a given rule. This works for custom rules made available by dependencies or by
# the current workspace. The command prints the documentation written on top of a rule definition,
# which should provide longer explanations than a diagnostic message.
bundle exec rdx lint explain MyRule
```

### Configuration

All Rubydex tools are always configured in `rubydex.toml` within their specific sections. Linter configurations allow users to disable rules, change their severity or exclude certain paths.

```toml
[linter.rules.MyCustomRule]
enabled = true
exclude = ["path_to_skip/**"]
severity = "warning" # Valid values: "hint" | "information" | "warning" | "error"
```

### Editor support

Linting results are automatically surfaced by the [Ruby LSP](https://github.com/Shopify/ruby-lsp). The only caveat is that only the beta version of the Ruby LSP is currently supported due to the Rubydex requirement. We are working on stabilizing a release of the Ruby LSP to remove this requirement.

## Creating custom rules

Rubydex ships with the base infrastructure allowing developers to create their own rules, which can exist inside of a given project or even published as a gem. Rules are loaded automatically by Rubydex as long as they follow the expected path patterns:

- gems: `lib/rubydex_linter/rules/**/*`
- project: `rubydex_linter/rules/**/*`


**IMPORTANT**: currently, Rubydex's analysis is limited to declarations. We are working on analyzing code flow and type inference, but that is not yet ready. Therefore, custom linting rules can only reason about declaration mistakes at the moment.

Custom rules inherit from `Rubydex::Linter::CustomRule` and must define the `lint` method. All rules have access to a fully populated `Rubydex::Graph`, which can be inspected for finding possible mistakes, in addition to convenience helpers.

Consider the following example, which is a rule to prohibit re-opening Rails models:
```ruby
module Rubydex
  module Linter
    module Rules
      # Prohibits Rails models from being re-opened (more than one definition).
      #
      # Note: rule class names become the config key.
      class ProhibitModelReopen < CustomRule
        class << self
          # The default severity of this rule, which can be overridden by the user's configuration.
          #
          # @override
          #: () -> singleton(Severity::Base)
          def default_severity
            Severity::Error
          end
        end

        # The inspection that this rule performs to find possible mistakes.
        #
        # @override
        #: () -> void
        def lint
          # All descendants of ActiveRecord::Base are models
          models = child_classes("ActiveRecord::Base")

          models.each do |model|
            # Every place that this model was defined
            definitions = model.definitions

            # If there's more than one definition, produce a diagnostic
            unless definitions.one?
              first, *others = definitions.to_a

              add_diagnostic(
                "`#{model.name}` is defined more than once.",
                first.name_location,
                related_information: others.map do |other_def|
                  RelatedInformation.new(
                    "`#{model.name}` is also defined here.",
                    other_def.name_location,
                  )
                end,
              )
            end
          end
        end
      end
    end
  end
end
```

**Note**: there's currently no support for auto correct.

#### Best practices

- Avoid over-notifying violations. One problem in the code should equal one diagnostic. If there are associated pieces of information that contribute to the problem, use related information to provide rich context
- Avoid over-querying the graph. This is the main source of performance issues with custom rules. Favor highly specific queries that fetch the information required directly, rather than looping through the entire graph. For example:


```ruby
# bad: loops through the entire graph to find out which declarations inherit from Foo
graph.declarations.select { |d| d.has_ancestor?("Foo") }

# good: directly inspects the desired descendants. No loop
graph["Foo"].descendants

# bad: loops through the entire graph to find all methods named `is_a?` declared on any type
graph.declarations.select { |d| d.is_a?(Rubydex::Method) && d.name.end_with?("#is_a?()") }

# good: uses the search API to find all matches directly
graph.search("#is_a?()")
```

- Rules should not read information from disk. The design of the linter is to operate on a graph object. Trying to read from disk from the implementation of a rule not only introduces IO in the middle of the analysis, but it may operate on a stale version of the codebase for scenarios like language servers
- Fewer generic rules are better than many highly specific ones. For example, instead of writing 2 separate rules to check if models inherit from `ApplicationModel` and controllers from `ApplicationController`, consider creating a `RequiredParentClass` rule that can be applied generically and configured

### Testing a rule

Rubydex provides the `RuleTestCase` parent class to conveniently test rules with a more visual approach.

```ruby
require "rubydex/linter/rule_test_case"

module Rubydex
  module Linter
    module Rules
      class ProhibitModelReopenTest < RuleTestCase
        def setup
          super

          # Add a shared piece of code that will be automatically included in
          # the graph for all examples
          source = <<~RUBY
            module ActiveRecord
              class Base; end
            end
          RUBY
          add_shared_source("fake/active_record.rb" => source)
        end

        def test_no_violations_with_single_definition
          assert_no_diagnostics("app/models/post.rb" => <<~RUBY)
            class Post < ActiveRecord::Base
            end
          RUBY
        end

        def test_catches_model_reopens
          # Expected violations use ^^^^ to point to the exact spot in the code
          # where the diagnostic will be produced. Assertions can include both
          # the diagnostic and its related information, so that developers can visually
          # see the experience that will be delivered to users.
          assert_diagnostics(
            "app/models/post.rb" => <<~RUBY,
              class Post < ActiveRecord::Base
                    ^^^^ `Post` is defined more than once.
              end
            RUBY
            "app/models/other_post.rb" => <<~RUBY,
              class Post < ActiveRecord::Base
                    ^^^^ `Post` is also defined here.
              end
            RUBY
          )
        end
      end
    end
  end
end
```
