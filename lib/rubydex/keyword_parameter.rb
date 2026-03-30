# frozen_string_literal: true

module Rubydex
  class KeywordParameter
    #: String
    attr_reader :name

    #: (String name) -> void
    def initialize(name)
      @name = name
    end
  end
end
