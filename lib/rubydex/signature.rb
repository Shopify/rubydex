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

    #: MethodDefinition
    attr_reader :method_definition

    #: (parameters: Array[Parameter], method_definition: MethodDefinition) -> void
    def initialize(parameters:, method_definition:)
      @parameters = parameters
      @method_definition = method_definition
    end
  end
end
