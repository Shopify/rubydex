# frozen_string_literal: true

module Rubydex
  class Mixin
    #: ConstantReference
    attr_reader :constant_reference

    #: (ConstantReference) -> void
    def initialize(constant_reference)
      @constant_reference = constant_reference
    end
  end

  # Represents `include SomeModule`
  class Include < Mixin; end

  # Represents `prepend SomeModule`
  class Prepend < Mixin; end

  # Represents `extend SomeModule`
  class Extend < Mixin; end
end
