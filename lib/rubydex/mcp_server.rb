# frozen_string_literal: true

require "json"

require "rubydex"
require "rubydex/mcp_server/protocol"

Dir[File.join(__dir__, "mcp_server", "tools", "*_tool.rb")].sort.each { |file| require file }

module Rubydex
  module MCPServer
    SERVER_INSTRUCTIONS = <<~TEXT
      Rubydex provides semantic Ruby code intelligence.

      Use these tools for Ruby source files (.rb, .rbi, .rbs) when you need structural information about declarations, locations, hierarchy, references, or codebase composition.

      Use text search instead for literal strings, comments, log messages, non-Ruby files, or content search rather than structural queries.

      Fully qualified name format: "Foo::Bar" for classes/modules/constants, "Foo::Bar#method_name" for instance methods.

      Pagination: tools that may return a high number of results include `total` for pagination. When `total` exceeds the number of returned items, use `offset` to fetch the next page.
    TEXT

    class Server
      WORKER_COUNT = 4

      #: (root_path: String, ?transport: StdioTransport) -> void
      def initialize(root_path:, transport: nil)
        @root_path = root_path
        @transport = transport
        @graph = Graph.new(workspace_path: @root_path)
        @graph.load_config
        @index_finished = false
        @incoming_queue = Thread::Queue.new
        @outgoing_queue = Thread::Queue.new
        @workers = []
        @outgoing_dispatcher = nil
      end

      attr_reader :root_path

      #: -> Thread
      def spawn_indexer
        Thread.new do
          @graph.index_workspace
          @graph.resolve
          @index_finished = true
        end
      end

      #: -> void
      def main_loop
        @workers = Array.new(WORKER_COUNT) { new_worker }
        @outgoing_dispatcher = Thread.new do
          while (response = @outgoing_queue.pop)
            @transport.write(response)
          end
        end

        @transport.open do |request, parse_error|
          if parse_error
            send_message(parse_error)
          else
            @incoming_queue << request
          end
        end
      ensure
        run_shutdown
        @transport.close
      end

      #: (Hash | Array | untyped) -> Hash | Array[Hash]?
      def handle(request)
        if request.is_a?(Array)
          return JSONRPC.error_response(nil, JSONRPC::INVALID_REQUEST, "Invalid Request", data: "Request is an empty array") if request.empty?

          responses = request.filter_map { |entry| handle(entry) }
          return responses if responses.any?

          return
        end

        unless request.is_a?(Hash)
          return JSONRPC.error_response(nil, JSONRPC::INVALID_REQUEST, "Invalid Request", data: "Request must be a hash")
        end

        has_id = request.key?(:id)
        id = request[:id]
        method = request[:method]
        params = request[:params]

        unless request[:jsonrpc] == "2.0"
          return JSONRPC.error_response(nil, JSONRPC::INVALID_REQUEST, "Invalid Request", data: "JSON-RPC version must be 2.0")
        end

        unless !has_id || id.is_a?(Integer) || (id.is_a?(String) && id.match?(/\A[a-zA-Z0-9_-]+\z/))
          return JSONRPC.error_response(nil, JSONRPC::INVALID_REQUEST, "Invalid Request", data: "Request ID must be a string or integer")
        end

        unless method.is_a?(String) && !method.start_with?("rpc.")
          return JSONRPC.error_response(nil, JSONRPC::INVALID_REQUEST, "Invalid Request", data: 'Method name must be a string and not start with "rpc."')
        end

        unless params.nil? || params.is_a?(Hash)
          return JSONRPC.error_response(id, JSONRPC::INVALID_PARAMS, "Invalid params", data: "Method parameters must be an object or null")
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
          return has_id ? JSONRPC.error_response(id, JSONRPC::METHOD_NOT_FOUND, "Method not found", data: method) : nil
        end

        has_id ? { jsonrpc: "2.0", id: id, result: result } : nil
      rescue KeyError => e
        has_id ? JSONRPC.error_response(id, JSONRPC::INVALID_PARAMS, "Invalid params", data: e.message) : nil
      rescue StandardError => e
        has_id ? JSONRPC.error_response(id, JSONRPC::INTERNAL_ERROR, "Internal error", data: e.message) : nil
      end

      #: -> Graph | Error
      def graph_or_error
        return @graph if @index_finished

        Error.new(
          "indexing",
          "Rubydex is still indexing the codebase",
          "The server is starting up. Please retry in a few seconds.",
        )
      end

      private

      #: -> void
      def run_shutdown
        @incoming_queue.close unless @incoming_queue.closed?
        @workers.each(&:join)
        @outgoing_queue.close unless @outgoing_queue.closed?
        @outgoing_dispatcher&.join
      end

      #: -> Thread
      def new_worker
        Thread.new do
          while (request = @incoming_queue.pop)
            send_message(handle(request))
          end
        end
      end

      #: (Hash | Array[Hash]?) -> void
      def send_message(response)
        return unless response
        return if @outgoing_queue.closed?

        @outgoing_queue << response
      end

      #: (Hash) -> Hash
      def call_tool(params)
        tool_name = params.fetch(:name)
        tool = Tool.tools_by_name.fetch(tool_name, nil)
        raise KeyError, "Tool not found: #{tool_name}" unless tool

        arguments = params[:arguments] || {}
        known_arguments = tool.input_schema.fetch(:properties).keys.map(&:to_s)
        unknown_arguments = arguments.keys.map(&:to_s) - known_arguments
        unless unknown_arguments.empty?
          return Tool::Response.new(
            [{ type: "text", text: "Unknown arguments: #{unknown_arguments.join(", ")}" }],
            error: true,
          ).to_h
        end

        missing_arguments = Array(tool.input_schema[:required]) - arguments.keys.map(&:to_s)
        unless missing_arguments.empty?
          return Tool::Response.new(
            [{ type: "text", text: "Missing required arguments: #{missing_arguments.join(", ")}" }],
            error: true,
          ).to_h
        end

        graph = graph_or_error
        return Tool::Response.new([{ type: "text", text: JSON.generate(graph) }]).to_h if graph.is_a?(Error)

        response = tool.new(graph).call(**arguments.transform_keys(&:to_sym))
        response.to_h
      end
    end

    class << self
      #: (?String) -> void
      def run(path = ".")
        root = File.realpath(path)
        server = Server.new(root_path: root, transport: StdioTransport.new)
        server.spawn_indexer

        server.main_loop
      end
    end
  end
end
