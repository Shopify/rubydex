# frozen_string_literal: true

module Rubydex
  module Severity
    # @abstract
    class Base
      class << self
        # @abstract
        #: () -> Symbol
        def value
          raise NotImplementedError, "Subclasses must implement the value method"
        end
      end
    end

    class Error < Base
      class << self
        # @override
        #: () -> Symbol
        def value
          :error
        end
      end
    end

    class Warning < Base
      class << self
        # @override
        #: () -> Symbol
        def value
          :warning
        end
      end
    end

    class Information < Base
      class << self
        # @override
        #: () -> Symbol
        def value
          :information
        end
      end
    end

    class Hint < Base
      class << self
        # @override
        #: () -> Symbol
        def value
          :hint
        end
      end
    end

    VALUE_MAP = {
      Error.value => Error,
      Warning.value => Warning,
      Information.value => Information,
      Hint.value => Hint,
    }.freeze #: Hash[Symbol, singleton(Base)]

    class << self
      #: (Symbol) -> singleton(Base)
      def from_value(value)
        severity = VALUE_MAP[value]
        return severity if severity

        raise ArgumentError, "Invalid severity #{value.inspect}. Expected one of: #{VALUE_MAP.keys.join(", ")}"
      end
    end
  end
end
