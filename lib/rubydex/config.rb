# frozen_string_literal: true

module Rubydex
  # The linter's settings, read from the `[linter]` section of the configuration file.
  class LinterConfig
    # The configured rules, keyed by rule name. Only rules the configuration file mentions appear here, so a rule that
    # was never configured is absent rather than present with its defaults.
    #
    #: Hash[String, RuleConfig]
    attr_reader :rules

    #: (Hash[String, RuleConfig]) -> void
    def initialize(rules)
      @rules = rules.freeze
      freeze
    end

    #: (singleton(Linter::Rule) rule_class) -> bool
    def rule_enabled?(rule_class)
      rule = @rules[rule_class.rule_name]
      !rule || rule.enabled?
    end
  end

  # The settings of a single linter rule, read from a `[linter.rules.RuleName]` table.
  class RuleConfig
    #: String
    attr_reader :name

    #: (String, bool) -> void
    def initialize(name, enabled)
      @name = name
      @enabled = enabled
    end

    #: () -> bool
    def enabled?
      @enabled
    end
  end
end
