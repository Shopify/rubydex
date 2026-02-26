# frozen_string_literal: true

module Rubydex
  # A zero based internal location. Intended to be used for tool-to-tool communication, such as a language server
  # communicating with an editor.
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

    # Turns this zero based location into a one based location for display purposes.
    #
    #: () -> DisplayLocation
    def to_display
      DisplayLocation.new(
        uri: @uri,
        start_line: @start_line + 1,
        end_line: @end_line + 1,
        start_column: @start_column + 1,
        end_column: @end_column + 1,
      )
    end

    #: -> String
    def to_s
      "#{path}:#{@start_line + 1}:#{@start_column + 1}-#{@end_line + 1}:#{@end_column + 1}"
    end
  end

  # A one based location intended for display purposes. This is what should be used when displaying a location to users,
  # like in CLIs
  class DisplayLocation < Location
    # Returns itself
    #
    #: () -> DisplayLocation
    def to_display
      self
    end

    #: -> String
    def to_s
      "#{path}:#{@start_line}:#{@start_column}-#{@end_line}:#{@end_column}"
    end
  end
end
