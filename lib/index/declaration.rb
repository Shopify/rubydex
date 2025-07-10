# frozen_string_literal: true

module Index
  # Represents the base declaration class for all indexed items. This class is partially defined in C to connect with
  # the Rust layer
  class Declaration
    attr_reader :start_offset, :end_offset

    def initialize(start_offset, end_offset)
      @start_offset = start_offset
      @end_offset = end_offset
    end
  end
end
