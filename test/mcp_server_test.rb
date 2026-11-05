# frozen_string_literal: true

require "test_helper"
require "helpers/context"
require "json"
require "mocha/minitest"
require "rubydex/mcp_server"
require "stringio"
require "timeout"
require "uri"

class MCPServerTest < Minitest::Test
  include Test::Helpers::WithContext

  def test_spawn_indexer_uses_workspace_indexing_entrypoint
    with_context do |context|
      context.write!("app.rb", "class LocalWorkspaceClass; end")

      server = Rubydex::MCPServer::Server.new(root_path: context.absolute_path)
      capture_io do
        server.spawn_indexer.join
      end
      graph = server.graph_or_error

      assert_kind_of(Rubydex::Graph, graph)
      assert_equal("LocalWorkspaceClass", graph["LocalWorkspaceClass"].name)

      rbs_kernel = graph["Kernel"]&.definitions&.find do |definition|
        path = URI(definition.location.uri).path
        path && File.extname(path) == ".rbs"
      end
      assert(rbs_kernel, "Expected MCP startup indexing to include core RBS definitions")

      response = server.handle(
        {
          jsonrpc: "2.0",
          id: 2,
          method: "tools/call",
          params: {
            name: "codebase_stats",
            arguments: {},
            _meta: { progressToken: "ready-request" },
          },
        },
      )

      assert_equal(false, response.fetch(:result).fetch(:isError))
      assert(server.instance_variable_get(:@outgoing_queue).empty?)
    end
  end

  def test_run_fails_when_root_path_cannot_be_canonicalized
    error = assert_raises(Errno::ENOENT) do
      Rubydex::MCPServer.run("missing-path")
    end

    assert_includes(error.message, "missing-path")
  end

  def test_codebase_stats_reports_indexing_until_graph_is_ready
    server = Rubydex::MCPServer::Server.new(root_path: Dir.pwd)

    send_request = {
      jsonrpc: "2.0",
      id: 1,
      method: "tools/call",
      params: {
        name: "codebase_stats",
        arguments: {},
      },
    }

    response = server.handle(send_request)
    result = response.fetch(:result)
    payload = JSON.parse(result.fetch(:content)[0].fetch(:text))

    assert_equal(false, result.fetch(:isError))
    assert_equal("indexing", payload.fetch("error"))
    assert_match(/still indexing/, payload.fetch("message"))
    assert_match(/retry/, payload.fetch("suggestion"))
  end

  def test_progress_enabled_tool_call_waits_for_indexing_and_reports_each_phase
    graph, gates = build_controlled_graph
    Rubydex::Graph.stubs(:configure_for_workspace).returns(graph)
    transport = Class.new do
      attr_reader :writes, :request_received

      def initialize(request)
        @request = request
        @writes = Queue.new
        @request_received = Queue.new
        @release = Queue.new
        @closed = false
      end

      def open
        yield @request
        @request_received << true
        @release.pop
      end

      def write(message)
        @writes << message
      end

      def close
        @closed = true
      end

      def release
        @release << true
      end

      def closed?
        @closed
      end
    end.new(progress_request(1, "indexing-request"))
    server = Rubydex::MCPServer::Server.new(root_path: Dir.pwd, transport: transport)
    indexer = server.spawn_indexer
    server_thread = Thread.new { server.main_loop }

    assert(Timeout.timeout(1) { gates.fetch(:indexing_started).pop })
    assert(transport.request_received.pop)

    indexing_progress = Timeout.timeout(2) { transport.writes.pop }
    assert_progress(indexing_progress, "indexing-request", 1, /index/i)

    gates.fetch(:continue_indexing) << true
    assert(gates.fetch(:resolving_started).pop)
    resolution_progress = Timeout.timeout(2) { transport.writes.pop }
    assert_progress(resolution_progress, "indexing-request", 2, /resolv/i)

    gates.fetch(:continue_resolving) << true
    response = Timeout.timeout(2) { transport.writes.pop }
    assert_equal(1, response.fetch(:id))
    result = response.fetch(:result)
    assert_equal(false, result.fetch(:isError))
    assert_equal(
      {
        "files" => 0,
        "declarations" => 0,
        "definitions" => 0,
        "constant_references" => 0,
        "method_references" => 0,
        "breakdown_by_kind" => {},
      },
      JSON.parse(result.fetch(:content).first.fetch(:text)),
    )
    assert(transport.writes.empty?)
    transport.release
    server_thread.join
    assert_predicate(transport, :closed?)
  ensure
    release_gates(gates)
    transport&.release
    server_thread&.join
    indexer&.join
  end

  def test_concurrent_progress_requests_keep_their_tokens
    graph, gates = build_controlled_graph
    Rubydex::Graph.stubs(:configure_for_workspace).returns(graph)
    server = Rubydex::MCPServer::Server.new(root_path: Dir.pwd)
    indexer = server.spawn_indexer
    assert(Timeout.timeout(1) { gates.fetch(:indexing_started).pop })

    calls = ["first-token", "second-token"].each_with_index.map do |token, id|
      Thread.new { server.handle(progress_request(id + 1, token)) }
    end
    progress = server.instance_variable_get(:@outgoing_queue)
    initial_updates = 2.times.map { Timeout.timeout(2) { progress.pop } }
    assert_equal(["first-token", "second-token"], initial_updates.map { |update| update.fetch(:params).fetch(:progressToken) }.sort)
    assert_equal([1, 1], initial_updates.map { |update| update.fetch(:params).fetch(:progress) })

    gates.fetch(:continue_indexing) << true
    assert(gates.fetch(:resolving_started).pop)
    resolution_updates = 2.times.map { Timeout.timeout(2) { progress.pop } }
    assert_equal(["first-token", "second-token"], resolution_updates.map { |update| update.fetch(:params).fetch(:progressToken) }.sort)
    assert_equal([2, 2], resolution_updates.map { |update| update.fetch(:params).fetch(:progress) })

    gates.fetch(:continue_resolving) << true
    assert_equal([false, false], calls.map { |call| call.value.fetch(:result).fetch(:isError) })
    assert(progress.empty?)
  ensure
    release_gates(gates)
    calls&.each(&:join)
    indexer&.join
  end

  def test_progress_enabled_tool_call_returns_when_indexing_fails
    graph = Object.new
    graph.define_singleton_method(:workspace_path) { Dir.pwd }
    graph.define_singleton_method(:index_workspace) { raise "indexing failed" }
    Rubydex::Graph.stubs(:configure_for_workspace).returns(graph)
    server = Rubydex::MCPServer::Server.new(root_path: Dir.pwd)

    indexer = nil
    response = nil
    _, stderr = capture_io do
      indexer = server.spawn_indexer
      response = Timeout.timeout(2) { server.handle(progress_request(1, "failed-request")) }
      indexer.join
    end

    payload = JSON.parse(response.fetch(:result).fetch(:content).first.fetch(:text))
    assert_equal("indexing_failed", payload.fetch("error"))
    assert_includes(stderr, "Rubydex MCP indexing failed")
  ensure
    indexer&.join
  end

  def test_ping_returns_empty_result
    server = Rubydex::MCPServer::Server.new(root_path: Dir.pwd)

    response = server.handle(
      {
        jsonrpc: "2.0",
        id: 1,
        method: "ping",
      },
    )

    assert_equal({}, response.fetch(:result))
  end

  def test_find_dead_code_candidates_reports_indexing_until_graph_is_ready
    server = Rubydex::MCPServer::Server.new(root_path: Dir.pwd)

    response = server.handle(
      {
        jsonrpc: "2.0",
        id: 1,
        method: "tools/call",
        params: { name: "find_dead_code_candidates" },
      },
    )

    result = response.fetch(:result)
    payload = JSON.parse(result.fetch(:content)[0].fetch(:text))

    assert_equal(false, result.fetch(:isError))
    assert_equal("indexing", payload.fetch("error"))
    assert_match(/still indexing/, payload.fetch("message"))
    assert_match(/retry/, payload.fetch("suggestion"))
  end

  def test_find_dead_code_candidates_dispatches_pagination_arguments
    with_context do |context|
      context.write!("app.rb", "class Charlie; end\nclass Bravo; end\nclass Alpha; end\n")
      graph = Rubydex::Graph.configure_for_workspace(context.absolute_path)
      assert_empty(graph.index_all([context.absolute_path]))
      graph.resolve

      server = Rubydex::MCPServer::Server.new(root_path: context.absolute_path)
      server.stubs(:graph_or_error).returns(graph)

      response = server.handle(
        {
          jsonrpc: "2.0",
          id: "dead-code-page",
          method: "tools/call",
          params: {
            name: "find_dead_code_candidates",
            arguments: { "limit" => 1, "offset" => 1 },
          },
        },
      )

      assert_equal("2.0", response.fetch(:jsonrpc))
      assert_equal("dead-code-page", response.fetch(:id))
      result = response.fetch(:result)
      assert_equal(false, result.fetch(:isError))
      assert_equal(1, result.fetch(:content).length)
      content = result.fetch(:content).fetch(0)
      assert_equal("text", content.fetch(:type))
      assert_equal(
        {
          "candidates" => [
            { "name" => "Bravo", "kind" => "Class", "locations" => [{ "path" => "app.rb", "line" => 2 }] },
          ],
          "total" => 3,
        },
        JSON.parse(content.fetch(:text)),
      )
    end
  end

  def test_unknown_method_returns_json_rpc_method_error
    server = Rubydex::MCPServer::Server.new(root_path: Dir.pwd)

    response = server.handle(
      {
        jsonrpc: "2.0",
        id: 1,
        method: "missing/method",
      },
    )

    error = response.fetch(:error)
    assert_equal(-32_601, error.fetch(:code))
    assert_equal("Method not found", error.fetch(:message))
    assert_equal("missing/method", error.fetch(:data))
  end

  def test_invalid_json_rpc_request_returns_invalid_request
    server = Rubydex::MCPServer::Server.new(root_path: Dir.pwd)

    response = server.handle(
      {
        jsonrpc: "1.0",
        id: 1,
        method: "ping",
      },
    )

    error = response.fetch(:error)
    assert_equal(-32_600, error.fetch(:code))
    assert_equal("Invalid Request", error.fetch(:message))
    assert_equal("JSON-RPC version must be 2.0", error.fetch(:data))
  end

  def test_explicit_null_id_returns_invalid_request
    server = Rubydex::MCPServer::Server.new(root_path: Dir.pwd)

    response = server.handle(
      {
        jsonrpc: "2.0",
        id: nil,
        method: "ping",
      },
    )

    error = response.fetch(:error)
    assert_equal(-32_600, error.fetch(:code))
    assert_equal("Invalid Request", error.fetch(:message))
    assert_equal("Request ID must be a string or integer", error.fetch(:data))
    assert_nil(response.fetch(:id))
  end

  def test_batch_request_always_returns_array
    server = Rubydex::MCPServer::Server.new(root_path: Dir.pwd)

    response = server.handle(
      [
        {
          jsonrpc: "2.0",
          id: 1,
          method: "ping",
        },
        {
          jsonrpc: "2.0",
          method: "notifications/initialized",
        },
      ],
    )

    assert_equal([{ jsonrpc: "2.0", id: 1, result: {} }], response)
  end

  def test_main_loop_handles_requests_concurrently
    transport = Class.new do
      attr_reader :writes

      def initialize
        @writes = Queue.new
        @closed = false
      end

      def open
        yield({ jsonrpc: "2.0", id: 1, method: "slow" })
        yield({ jsonrpc: "2.0", id: 2, method: "fast" })
      end

      def write(response)
        @writes << response
      end

      def close
        @closed = true
      end

      def closed?
        @closed
      end
    end.new
    server = Rubydex::MCPServer::Server.new(root_path: Dir.pwd, transport: transport)
    slow_request_started = Queue.new
    release_slow_request = Queue.new

    server.define_singleton_method(:handle) do |request|
      if request.fetch(:id) == 1
        slow_request_started << true
        release_slow_request.pop
      end

      { jsonrpc: "2.0", id: request.fetch(:id), result: {} }
    end

    server_thread = Thread.new { server.main_loop }
    slow_request_started.pop
    first_response = Timeout.timeout(1) { transport.writes.pop }

    assert_equal(2, first_response.fetch(:id))

    release_slow_request << true
    server_thread.join
    second_response = Timeout.timeout(1) { transport.writes.pop }

    assert_equal(1, second_response.fetch(:id))
    assert_predicate(transport, :closed?)
  end

  def test_main_loop_serializes_writes_through_output_queue
    transport = Class.new do
      attr_reader :errors, :writes

      def initialize
        @errors = Queue.new
        @writes = Queue.new
        @writing = false
      end

      def open
        10.times do |index|
          yield({ jsonrpc: "2.0", id: index, method: "ping" })
        end
      end

      def write(response)
        @errors << "concurrent write" if @writing
        @writing = true
        sleep(0.001)
        @writes << response
        @writing = false
      end

      def close
      end
    end.new
    server = Rubydex::MCPServer::Server.new(root_path: Dir.pwd, transport: transport)

    server.define_singleton_method(:handle) do |request|
      { jsonrpc: "2.0", id: request.fetch(:id), result: {} }
    end

    server.main_loop

    responses = 10.times.map { Timeout.timeout(1) { transport.writes.pop } }
    assert_empty(transport.errors.size.times.map { transport.errors.pop })
    assert_equal((0...10).to_a, responses.map { |response| response.fetch(:id) }.sort)
  end

  def test_main_loop_routes_parse_errors_through_output_queue
    output = StringIO.new
    transport = Rubydex::MCPServer::StdioTransport.new
    transport.instance_variable_set(:@input, StringIO.new("{bad json\n"))
    transport.instance_variable_set(:@output, output)
    server = Rubydex::MCPServer::Server.new(root_path: Dir.pwd, transport: transport)

    server.main_loop

    response = JSON.parse(output.string)
    error = response.fetch("error")
    assert_equal(-32_700, error.fetch("code"))
    assert_equal("Parse error", error.fetch("message"))
  end

  def test_unknown_tool_argument_returns_tool_error
    server = Rubydex::MCPServer::Server.new(root_path: Dir.pwd)

    response = server.handle(
      {
        jsonrpc: "2.0",
        id: 1,
        method: "tools/call",
        params: {
          name: "search_declarations",
          arguments: {
            query: "Dog",
            unexpected: true,
          },
        },
      },
    )

    result = response.fetch(:result)
    assert_equal(true, result.fetch(:isError))
    assert_equal("Unknown arguments: unexpected", result.fetch(:content)[0].fetch(:text))
  end

  def test_missing_required_tool_argument_returns_tool_error
    server = Rubydex::MCPServer::Server.new(root_path: Dir.pwd)

    response = server.handle(
      {
        jsonrpc: "2.0",
        id: 1,
        method: "tools/call",
        params: {
          name: "search_declarations",
          arguments: {},
        },
      },
    )

    result = response.fetch(:result)
    assert_equal(true, result.fetch(:isError))
    assert_equal("Missing required arguments: query", result.fetch(:content)[0].fetch(:text))
  end

  private

  def progress_request(id, token)
    {
      jsonrpc: "2.0",
      id: id,
      method: "tools/call",
      params: {
        name: "codebase_stats",
        arguments: {},
        _meta: { progressToken: token },
      },
    }
  end

  def assert_progress(notification, token, progress, message)
    assert_equal("2.0", notification.fetch(:jsonrpc))
    assert_equal("notifications/progress", notification.fetch(:method))
    assert_equal(token, notification.fetch(:params).fetch(:progressToken))
    assert_equal(progress, notification.fetch(:params).fetch(:progress))
    assert_match(message, notification.fetch(:params).fetch(:message))
    refute(notification.key?(:id))
  end

  def build_controlled_graph
    gates = {
      indexing_started: Queue.new,
      continue_indexing: Queue.new,
      resolving_started: Queue.new,
      continue_resolving: Queue.new,
    }
    graph = Object.new
    graph.define_singleton_method(:workspace_path) { Dir.pwd }
    graph.define_singleton_method(:index_workspace) do
      gates.fetch(:indexing_started) << true
      gates.fetch(:continue_indexing).pop
    end
    graph.define_singleton_method(:resolve) do
      gates.fetch(:resolving_started) << true
      gates.fetch(:continue_resolving).pop
    end
    graph.define_singleton_method(:declarations) { [] }
    graph.define_singleton_method(:documents) { [] }
    graph.define_singleton_method(:constant_references) { [] }
    graph.define_singleton_method(:method_references) { [] }
    [graph, gates]
  end

  def release_gates(gates)
    return unless gates

    [:continue_indexing, :continue_resolving].each do |gate_name|
      gate = gates.fetch(gate_name)
      gate << true if gate.empty?
    end
  end
end
