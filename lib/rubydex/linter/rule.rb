# frozen_string_literal: true

module Rubydex
  module Linter
    # Base class for semantic lint rules.
    # @abstract
    class Rule
      class << self
        #: () -> String
        def rule_name
          name.split("::").last
        end
      end

      #: Graph
      attr_reader :graph

      #: LinterConfig
      attr_reader :config

      #: Array[Diagnostic]
      attr_reader :diagnostics

      #: (Graph, config: LinterConfig) -> void
      def initialize(graph, config:)
        @graph = graph
        @config = config
        @diagnostics = [] #: Array[Diagnostic]
      end

      # @abstract
      #: () -> singleton(Severity::Base)
      def severity
        raise NotImplementedError, "Subclasses must implement the severity method"
      end

      # @abstract
      #: () -> void
      def lint
        raise NotImplementedError, "Subclasses must implement the lint method"
      end

      protected

      #: (
      #|   String,
      #|   Location,
      #|   ?related_information: Array[RelatedInformation],
      #| ) -> void
      def add_diagnostic(message, location, related_information: [])
        @diagnostics << Diagnostic.new(
          rule: self.class.rule_name,
          message: message,
          location: location,
          severity: severity,
          related_information: related_information,
        )
      end
    end
  end
end
