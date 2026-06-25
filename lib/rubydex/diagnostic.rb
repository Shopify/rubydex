# frozen_string_literal: true

module Rubydex
  class Diagnostic
    #: String
    attr_reader :rule

    #: String
    attr_reader :message

    #: Location
    attr_reader :location

    #: (rule: String, message: String, location: Location) -> void
    def initialize(rule:, message:, location:)
      @rule = rule
      @message = message
      @location = location
    end
  end
end
