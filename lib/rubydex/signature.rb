# frozen_string_literal: true

module Rubydex
  class Signature
    class Parameter
      #: Symbol
      attr_reader :name

      #: Location
      attr_reader :location

      #: (Symbol, Location) -> void
      def initialize(name, location)
        @name = name
        @location = location
      end
    end

    #: Array[Parameter]
    attr_reader :parameters

    #: (Array[Parameter]) -> void
    def initialize(parameters)
      @parameters = parameters
    end
  end
end
