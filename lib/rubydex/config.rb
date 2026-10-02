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

    #: (singleton(Rule) rule_class) -> bool
    def rule_enabled?(rule_class)
      rule = @rules[rule_class.rule_name]
      !rule || rule.enabled?
    end

    #: (singleton(Rule) rule_class) -> Array[String]
    def excludes_for(rule_class)
      @rules[rule_class.rule_name]&.exclude_patterns || []
    end

    #: (singleton(Rule) rule_class) -> singleton(Severity::Base)?
    def severity_for(rule_class)
      @rules[rule_class.rule_name]&.severity
    end

    #: (singleton(Rule) rule_class) -> Hash[String, RuleConfig::option_value]
    def options_for(rule_class)
      @rules[rule_class.rule_name]&.options || {}.freeze
    end
  end

  # The settings of a single linter rule, read from a `[linter.rules.RuleName]` table.
  class RuleConfig
    #: type option_value = String | Integer | Float | bool | Array[option_value]

    #: String
    attr_reader :name

    #: Array[String]
    attr_reader :exclude_patterns

    #: singleton(Severity::Base)?
    attr_reader :severity

    #: Hash[String, option_value]
    attr_reader :options

    #: (String, bool, ?Array[String], ?singleton(Severity::Base)?, ?Hash[String, option_value]) -> void
    def initialize(name, enabled, exclude_patterns = [], severity = nil, options = {})
      @name = name
      @enabled = enabled
      @exclude_patterns = exclude_patterns
      @severity = severity
      @options = options.freeze
    end

    #: () -> bool
    def enabled?
      @enabled
    end
  end
end
