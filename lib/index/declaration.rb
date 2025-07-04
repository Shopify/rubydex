# frozen_string_literal: true

module Index
  class Declaration
    def initialize(start_offset, end_offset)
      @start_offset = start_offset
      @end_offset = end_offset
    end
  end
end
