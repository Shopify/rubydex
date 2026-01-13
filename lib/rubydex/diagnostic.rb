# frozen_string_literal: true

module Rubydex
  class Diagnostic
    #: String
    attr_reader :message

    #: Integer
    attr_reader :code

    #: Location
    attr_reader :location

    #: (message: String, code: Integer, location: Location) -> void
    def initialize(message:, code:, location:)
      @message = message
      @code = code
      @location = location
    end
  end
end
