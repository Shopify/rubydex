# frozen_string_literal: true

module Rubydex
  class RelatedInformation
    #: String
    attr_reader :message

    #: Location
    attr_reader :location

    #: (String, Location) -> void
    def initialize(message, location)
      @message = message
      @location = location
    end
  end
end
