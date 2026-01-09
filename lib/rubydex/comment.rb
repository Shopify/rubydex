# frozen_string_literal: true

module Rubydex
  class Comment
    #: String
    attr_reader :string

    #: Location
    attr_reader :location

    #: (?string: String, ?location: Location) -> void
    def initialize(string:, location:)
      @string = string
      @location = location
    end
  end
end
