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

    #: (singleton(Linter::Rule) rule_class) -> Array[String]
    def excludes_for(rule_class)
      @rules[rule_class.rule_name]&.exclude_patterns || []
    end

    #: (singleton(Linter::Rule) rule_class) -> singleton(Severity::Base)
    def severity_for(rule_class)
      @rules[rule_class.rule_name]&.severity || rule_class.default_severity
    end
  end

  # The settings of a single linter rule, read from a `[linter.rules.RuleName]` table.
  class RuleConfig
    #: String
    attr_reader :name

    #: Array[String]
    attr_reader :exclude_patterns

    #: singleton(Severity::Base)?
    attr_reader :severity

    #: (String, bool, ?Array[String], ?singleton(Severity::Base)?) -> void
    def initialize(name, enabled, exclude_patterns = [], severity = nil)
      @name = name
      @enabled = enabled
      @exclude_patterns = exclude_patterns
      @severity = severity
    end

    #: () -> bool
    def enabled?
      @enabled
    end
  end
end
