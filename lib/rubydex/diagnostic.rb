# frozen_string_literal: true

module Rubydex
  class Diagnostic
    #: singleton(Rule)
    attr_reader :rule

    #: String
    attr_reader :message

    #: Location
    attr_reader :location

    #: Array[RelatedInformation]
    attr_reader :related_information

    #: (
    #|   rule: singleton(Rule),
    #|   message: String,
    #|   location: Location,
    #|   ?related_information: Array[RelatedInformation],
    #| ) -> void
    def initialize(rule:, message:, location:, related_information: [])
      @rule = rule
      @message = message
      @location = location
      @related_information = related_information
    end
  end
end
