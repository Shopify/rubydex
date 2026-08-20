# frozen_string_literal: true

module Rubydex
  # The identity of a rule. We have different mechanisms for collecting diagnostics, but they all share the same
  # fundamental identity: a unique name and a default severity that may be overridden by the user's configuration.
  #
  # By keeping this identity concept unified, we are able to treat rules consistently regardless of whether they were
  # collected by the graph during the analysis or by the linter afterwards.
  #
  # @abstract
  class Rule
    class << self
      #: () -> String
      def rule_name
        name #: as !nil
          .split("::").last #: as !nil
      end

      # @abstract
      #: () -> singleton(Severity::Base)
      def default_severity
        raise NotImplementedError, "Subclasses must implement the default_severity method"
      end

      # Returns the resolved severity of the rule based on the given configuration.
      #
      #: (LinterConfig) -> singleton(Severity::Base)
      def severity(config)
        config.severity_for(self) || default_severity
      end
    end
  end
end
