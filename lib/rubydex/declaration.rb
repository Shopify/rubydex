# frozen_string_literal: true

module Rubydex
  class Declaration
    # @abstract
    #: () -> Enumerable[Reference]
    def references
      raise NotImplementedError, "Subclasses must implement #references"
    end
  end
end
