# frozen_string_literal: true

module Rubydex
  # ABC complexity reports. Partially defined in C (native_analyze / native_diff).
  module Complexity
    class << self
      #: (Array[String] paths, ?format: String | Symbol, ?top: Integer, ?methods_only: bool, ?details: bool, ?group: bool) -> String
      def analyze(paths, format: :text, top: 25, methods_only: false, details: false, group: false)
        raise TypeError, "no implicit conversion of #{paths.class} into Array" unless paths.is_a?(Array)

        native_analyze(paths.map(&:to_s), format, top, methods_only, details, group)
      end

      #: (String baseline_json, String current_json, ?format: String | Symbol, ?top: Integer) -> String
      def diff(baseline_json, current_json, format: :text, top: 25)
        native_diff(baseline_json, current_json, format, top)
      end
    end
  end
end
