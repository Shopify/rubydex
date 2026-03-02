# frozen_string_literal: true

module Rubydex
  class Failure
    #: String
    attr_reader :message

    #: (String) -> void
    def initialize(message)
      @message = message
    end
  end

  class IntegrityFailure < Failure; end
end
