# frozen_string_literal: true

module Rubydex
  class Diagnostic
    #: String
    attr_reader :rule

    #: String
    attr_reader :message

    #: Location
    attr_reader :location

    #: singleton(Severity::Base)
    attr_reader :severity

    #: Array[RelatedInformation]
    attr_reader :related_information

    #: (
    #|   rule: String,
    #|   message: String,
    #|   location: Location,
    #|   severity: singleton(Severity::Base),
    #|   ?related_information: Array[RelatedInformation],
    #| ) -> void
    def initialize(rule:, message:, location:, severity:, related_information: [])
      @rule = rule
      @message = message
      @location = location
      @severity = severity
      @related_information = related_information
    end
  end
end
