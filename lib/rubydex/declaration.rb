# frozen_string_literal: true

module Rubydex
  module Visibility
    #: () -> bool
    def public? = visibility == :public

    #: () -> bool
    def private? = visibility == :private

    #: () -> bool
    def protected? = visibility == :protected
  end

  class Declaration
    # @abstract
    #: () -> Enumerable[Reference]
    def references
      raise NotImplementedError, "Subclasses must implement #references"
    end
  end

  class Class < Namespace
    include Visibility
  end

  class Module < Namespace
    include Visibility
  end

  class Constant < Declaration
    include Visibility
  end

  class ConstantAlias < Declaration
    include Visibility
  end

  class Method < Declaration
    include Visibility
  end
end
