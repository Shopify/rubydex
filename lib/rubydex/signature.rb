# frozen_string_literal: true

module Rubydex
  # Represents a single signature of a method definition.
  #
  # A method definition may have multiple signatures when defined with overloads in RBS.
  class Signature
    # Returns the parameters of the signature.
    #
    # Each parameter is a 3-element array of `[kind, name, location]`,
    # following the same format as `Method#parameters` with an additional location element.
    #
    # The kind symbol is one of:
    # - `:req` — required positional parameter (`a`) or post-rest positional parameter (`d` in `def foo(*c, d)`)
    # - `:opt` — optional positional parameter (`b = 1`)
    # - `:rest` — rest positional parameter (`*c`)
    # - `:keyreq` — required keyword parameter (`e:`)
    # - `:key` — optional keyword parameter (`f: 1`)
    # - `:keyrest` — rest keyword parameter (`**g`)
    # - `:block` — block parameter (`&h`)
    # - `:forward` — forward parameter (`...`), Rubydex-specific (Ruby expands this to `:rest`, `:keyrest`, `:block`)
    #
    #: Array[[Symbol, Symbol, Location]]
    attr_reader :parameters

    # Returns the method definition this signature belongs to.
    #
    #: MethodDefinition
    attr_reader :method_definition

    #: (parameters: Array[[Symbol, Symbol, Location]], method_definition: MethodDefinition) -> void
    def initialize(parameters:, method_definition:)
      @parameters = parameters
      @method_definition = method_definition
    end
  end
end
