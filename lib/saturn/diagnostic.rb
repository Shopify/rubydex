# frozen_string_literal: true

module Saturn
  class Diagnostic
    #: String
    attr_reader :message

    #: Symbol
    attr_reader :severity

    #: Location
    attr_reader :location

    #: (message: String, severity: Symbol, location: Location) -> void
    def initialize(message:, severity:, location:)
      @message = message
      @severity = severity
      @location = location
    end
  end
end
