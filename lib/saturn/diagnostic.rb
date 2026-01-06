# frozen_string_literal: true

module Saturn
  class Diagnostic
    #: String
    attr_reader :message

    #: Symbol
    attr_reader :severity

    #: Integer
    attr_reader :code

    #: Location
    attr_reader :location

    #: (message: String, severity: Symbol, code: Integer, location: Location) -> void
    def initialize(message:, severity:, code:, location:)
      @message = message
      @severity = severity
      @code = code
      @location = location
    end
  end
end
