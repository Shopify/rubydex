# frozen_string_literal: true

module Rubydex
  class Error
    #: String
    attr_reader :message

    #: (String) -> void
    def initialize(message)
      @message = message
    end
  end

  class IntegrityError < Error; end
end
