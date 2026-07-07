# frozen_string_literal: true

require "json"

module Rubydex
  module MCPServer
    module JSONRPC
      PARSE_ERROR = -32_700
      INVALID_REQUEST = -32_600
      METHOD_NOT_FOUND = -32_601
      INVALID_PARAMS = -32_602
      INTERNAL_ERROR = -32_603

      class << self
        #: (untyped, Integer, String, ?data: untyped) -> Hash
        def error_response(id, code, message, data: nil)
          {
            jsonrpc: "2.0",
            id: id,
            error: {
              code: code,
              message: message,
              data: data,
            }.compact,
          }
        end
      end
    end

    class Error
      #: (String, ?String, ?String) -> void
      def initialize(error, message = nil, suggestion = nil)
        @error = error
        @message = message
        @suggestion = suggestion
      end

      #: (*untyped) -> String
      def to_json(*args)
        payload = { error: @error }
        payload[:message] = @message if @message
        payload[:suggestion] = @suggestion if @suggestion
        payload.to_json(*args)
      end
    end

    class Tool
      @tools = []

      class Response
        #: (Array[Hash], ?bool) -> void
        def initialize(content, error: false)
          @content = content
          @error = error
        end

        attr_reader :content

        #: -> bool
        def error?
          @error
        end

        #: -> Hash
        def to_h
          {
            content: content,
            isError: error?,
          }
        end
      end

      class << self
        attr_reader :tools

        #: (Class) -> void
        def inherited(tool)
          Tool.tools << tool unless tool.name&.end_with?("::BaseTool")
          super
        end

        #: -> Hash[String, Class]
        def tools_by_name
          tools.to_h { |tool| [tool.tool_name, tool] }
        end

        #: (?String) -> String
        def tool_name(value = nil)
          @tool_name = value if value
          @tool_name || raise(NotImplementedError, "#{name} must define tool_name")
        end

        #: (?String) -> String
        def description(value = nil)
          @description = value if value
          @description || raise(NotImplementedError, "#{name} must define description")
        end

        #: (?Hash, ?Array[String]) -> Hash
        def input_schema(properties: nil, required: nil)
          if properties
            @input_schema = {
              type: "object",
              properties: properties,
            }
            @input_schema[:required] = required if required
          end

          @input_schema || { type: "object", properties: {} }
        end

        #: -> Hash
        def to_h
          {
            name: tool_name,
            description: description,
            inputSchema: input_schema,
          }
        end
      end
    end

    class StdioTransport
      #: -> void
      def initialize
        @input = $stdin
        @output = $stdout
        @input.binmode
        @input.sync = true
        @output.binmode
        @output.sync = true
      end

      #: { (Hash | Array?, Hash?) -> void } -> void
      def open
        @input.each_line do |line|
          yield JSON.parse(line, symbolize_names: true)
        rescue JSON::ParserError
          yield nil, JSONRPC.error_response(nil, JSONRPC::PARSE_ERROR, "Parse error", data: "Invalid JSON")
        end
      end

      #: (Hash | Array[Hash]?) -> void
      def write(response)
        return unless response

        @output.puts(JSON.generate(response))
      end

      #: -> void
      def close
        @output.flush
      end
    end
  end
end
