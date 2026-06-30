# frozen_string_literal: true

require "json"

module Rubydex
  module MCPServer
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
      UNSET = Object.new.freeze
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
          tools << tool
          @tools_by_name = nil
          super
        end

        #: -> Hash[String, Class]
        def tools_by_name
          @tools_by_name ||= tools.to_h { |tool| [tool.tool_name, tool] }
        end

        #: (?String) -> String
        def tool_name(value = UNSET)
          @tool_name = value unless value.equal?(UNSET)
          @tool_name || raise(NotImplementedError, "#{name} must define tool_name")
        end

        #: (?String) -> String
        def description(value = UNSET)
          @description = value unless value.equal?(UNSET)
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

    class Server
      PARSE_ERROR = -32_700
      INVALID_REQUEST = -32_600
      METHOD_NOT_FOUND = -32_601
      INVALID_PARAMS = -32_602
      INTERNAL_ERROR = -32_603

      #: (Hash | Array | untyped) -> Hash | Array[Hash]?
      def handle(request)
        if request.is_a?(Array)
          return error_response(nil, INVALID_REQUEST, "Invalid Request", data: "Request is an empty array") if request.empty?

          responses = request.filter_map { |entry| handle(entry) }
          return responses if responses.any?

          return
        end

        unless request.is_a?(Hash)
          return error_response(nil, INVALID_REQUEST, "Invalid Request", data: "Request must be a hash")
        end

        has_id = request.key?(:id)
        id = request[:id]
        method = request[:method]
        params = request[:params]

        unless request[:jsonrpc] == "2.0"
          return error_response(nil, INVALID_REQUEST, "Invalid Request", data: "JSON-RPC version must be 2.0")
        end

        unless !has_id || id.is_a?(Integer) || (id.is_a?(String) && id.match?(/\A[a-zA-Z0-9_-]+\z/))
          return error_response(nil, INVALID_REQUEST, "Invalid Request", data: "Request ID must be a string or integer")
        end

        unless method.is_a?(String) && !method.start_with?("rpc.")
          return error_response(nil, INVALID_REQUEST, "Invalid Request", data: 'Method name must be a string and not start with "rpc."')
        end

        unless params.nil? || params.is_a?(Hash)
          return error_response(id, INVALID_PARAMS, "Invalid params", data: "Method parameters must be an object or null")
        end

        result = case method
        when "initialize"
          {
            protocolVersion: "2025-03-26",
            capabilities: { tools: {} },
            serverInfo: {
              name: "rubydex_mcp",
              version: Rubydex::VERSION,
            },
            instructions: SERVER_INSTRUCTIONS,
          }
        when "tools/list"
          { tools: Tool.tools.map(&:to_h) }
        when "tools/call"
          call_tool(params || {})
        when "ping"
          {}
        when "notifications/initialized"
          return
        else
          return has_id ? error_response(id, METHOD_NOT_FOUND, "Method not found", data: method) : nil
        end

        has_id ? { jsonrpc: "2.0", id: id, result: result } : nil
      rescue KeyError => e
        has_id ? error_response(id, INVALID_PARAMS, "Invalid params", data: e.message) : nil
      rescue StandardError => e
        has_id ? error_response(id, INTERNAL_ERROR, "Internal error", data: e.message) : nil
      end

      private

      #: (Hash) -> Hash
      def call_tool(params)
        tool_name = params.fetch(:name)
        tool = Tool.tools_by_name.fetch(tool_name, nil)
        raise KeyError, "Tool not found: #{tool_name}" unless tool

        arguments = params[:arguments] || {}
        missing_arguments = Array(tool.input_schema[:required]) - arguments.keys.map(&:to_s)
        unless missing_arguments.empty?
          return Tool::Response.new(
            [{ type: "text", text: "Missing required arguments: #{missing_arguments.join(", ")}" }],
            error: true,
          ).to_h
        end

        response = tool.call(**arguments.transform_keys(&:to_sym), server: self)
        response.to_h
      end

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

    class StdioTransport
      #: (Server, ?IO, ?IO) -> void
      def initialize(server, input: $stdin, output: $stdout)
        @server = server
        @input = input
        @output = output
        @input.binmode
        @output.binmode
        @output.sync = true
      end

      #: -> void
      def open
        @input.each_line do |line|
          response = @server.handle(JSON.parse(line, symbolize_names: true))
          next unless response

          @output.puts(JSON.generate(response))
          @output.flush
        rescue JSON::ParserError
          @output.puts(JSON.generate(jsonrpc: "2.0", id: nil, error: { code: Server::PARSE_ERROR, message: "Parse error", data: "Invalid JSON" }))
          @output.flush
        end
      end
    end
  end
end
