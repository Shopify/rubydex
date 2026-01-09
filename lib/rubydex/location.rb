# frozen_string_literal: true

module Rubydex
  class Location
    include Comparable

    #: String
    attr_reader :uri

    #: Integer
    attr_reader :start_line, :end_line, :start_column, :end_column

    #: (?uri: String, ?start_line: Integer, ?end_line: Integer, ?start_column: Integer, ?end_column: Integer) -> void
    def initialize(uri:, start_line:, end_line:, start_column:, end_column:)
      @uri = uri
      @start_line = start_line
      @end_line = end_line
      @start_column = start_column
      @end_column = end_column
    end

    #: () -> String
    def path
      uri = URI(@uri)
      raise Rubydex::Error, "URI is not a file:// URI: #{@uri}" unless uri.scheme == "file"

      path = uri.path
      # TODO: This has to go away once we have a proper URI abstraction
      path.delete_prefix!("/") if Gem.win_platform?
      path
    end

    #: (other: BasicObject) -> Integer
    def <=>(other)
      return -1 unless other.is_a?(Location)

      a = [@uri, @start_line, @start_column, @end_line, @end_column]
      b = [other.uri, other.start_line, other.start_column, other.end_line, other.end_column]
      a <=> b
    end

    #: -> String
    def to_s
      "#{path}:#{@start_line}:#{@start_column}-#{@end_line}:#{@end_column}"
    end
  end
end
