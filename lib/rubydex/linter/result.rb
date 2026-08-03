# frozen_string_literal: true

module Rubydex
  module Linter
    class Result
      #: Array[Diagnostic]
      attr_reader :diagnostics

      #: (Array[Diagnostic]) -> void
      def initialize(diagnostics)
        @diagnostics = diagnostics
      end

      #: () -> bool
      def success?
        @diagnostics.none? { |diagnostic| diagnostic.severity == Severity::Error }
      end
    end
  end
end
