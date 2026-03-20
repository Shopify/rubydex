# frozen_string_literal: true

module Rubydex
  class Keyword
    #: String
    attr_reader :name

    #: String
    attr_reader :documentation

    #: (String name, String documentation) -> void
    def initialize(name, documentation)
      @name = name
      @documentation = documentation
    end
  end
end
