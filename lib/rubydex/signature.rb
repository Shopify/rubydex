# frozen_string_literal: true

module Rubydex
  class Signature
    class Parameter
      #: Symbol
      attr_reader :name

      #: Location
      attr_reader :location

      #: (Symbol, Location) -> void
      def initialize(name, location)
        @name = name
        @location = location
      end
    end

    class PositionalParameter < Parameter; end
    class OptionalPositionalParameter < Parameter; end
    class RestPositionalParameter < Parameter; end
    class PostParameter < Parameter; end
    class KeywordParameter < Parameter; end
    class OptionalKeywordParameter < Parameter; end
    class RestKeywordParameter < Parameter; end
    class ForwardParameter < Parameter; end
    class BlockParameter < Parameter; end

    #: Array[Parameter]
    attr_reader :parameters

    #: (Array[Parameter]) -> void
    def initialize(parameters)
      @parameters = parameters
    end

    #: () -> [Array[PositionalParameter], Array[OptionalPositionalParameter], RestPositionalParameter?, Array[PostParameter], Array[KeywordParameter], Array[OptionalKeywordParameter], RestKeywordParameter?, ForwardParameter?, BlockParameter?]
    def deconstruct
      positionals = [] #: Array[PositionalParameter]
      optional_positionals = [] #: Array[OptionalPositionalParameter]
      rest_positional = nil #: RestPositionalParameter?
      posts = [] #: Array[PostParameter]
      keywords = [] #: Array[KeywordParameter]
      optional_keywords = [] #: Array[OptionalKeywordParameter]
      rest_keyword = nil #: RestKeywordParameter?
      forward = nil #: ForwardParameter?
      block = nil #: BlockParameter?

      parameters.each do |param|
        case param
        when PositionalParameter then positionals << param
        when OptionalPositionalParameter then optional_positionals << param
        when RestPositionalParameter then rest_positional = param
        when PostParameter then posts << param
        when KeywordParameter then keywords << param
        when OptionalKeywordParameter then optional_keywords << param
        when RestKeywordParameter then rest_keyword = param
        when ForwardParameter then forward = param
        when BlockParameter then block = param
        end
      end

      [positionals, optional_positionals, rest_positional, posts, keywords, optional_keywords, rest_keyword, forward, block]
    end

    DECONSTRUCT_KEYS = [
      :positional_parameters,
      :optional_positional_parameters,
      :rest_positional_parameter,
      :post_parameters,
      :keyword_parameters,
      :optional_keyword_parameters,
      :rest_keyword_parameter,
      :forward_parameter,
      :block_parameter,
    ].freeze #: Array[Symbol]
    private_constant :DECONSTRUCT_KEYS

    #: (Array[Symbol]?) -> Hash[Symbol, untyped]
    def deconstruct_keys(keys)
      keys = DECONSTRUCT_KEYS if keys.nil?

      positionals, optional_positionals, rest_positional, posts,
        keywords, optional_keywords, rest_keyword, forward, block = deconstruct

      result = {} #: Hash[Symbol, untyped]
      keys.each do |key|
        case key
        when :positional_parameters then result[key] = positionals
        when :optional_positional_parameters then result[key] = optional_positionals
        when :rest_positional_parameter then result[key] = rest_positional
        when :post_parameters then result[key] = posts
        when :keyword_parameters then result[key] = keywords
        when :optional_keyword_parameters then result[key] = optional_keywords
        when :rest_keyword_parameter then result[key] = rest_keyword
        when :forward_parameter then result[key] = forward
        when :block_parameter then result[key] = block
        end
      end
      result
    end

    #: () -> Array[PositionalParameter]
    def positional_parameters = deconstruct[0]

    #: () -> Array[OptionalPositionalParameter]
    def optional_positional_parameters = deconstruct[1]

    #: () -> RestPositionalParameter?
    def rest_positional_parameter = deconstruct[2]

    #: () -> Array[PostParameter]
    def post_parameters = deconstruct[3]

    #: () -> Array[KeywordParameter]
    def keyword_parameters = deconstruct[4]

    #: () -> Array[OptionalKeywordParameter]
    def optional_keyword_parameters = deconstruct[5]

    #: () -> RestKeywordParameter?
    def rest_keyword_parameter = deconstruct[6]

    #: () -> ForwardParameter?
    def forward_parameter = deconstruct[7]

    #: () -> BlockParameter?
    def block_parameter = deconstruct[8]
  end
end
