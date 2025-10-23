# frozen_string_literal: true

module Saturn
  class Location
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
      raise Saturn::Error, "URI is not a file:// URI: #{@uri}" unless uri.scheme == "file"

      uri.path
    end

    #: -> String
    def to_s
      "#{path}:#{@start_line}:#{@start_column}-#{@end_line}:#{@end_column}"
    end
  end
end
