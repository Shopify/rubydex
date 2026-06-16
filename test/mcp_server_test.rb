# frozen_string_literal: true

require "test_helper"
require "helpers/context"
require "json"
require "rubydex/mcp_server"
require "open3"
require "rbconfig"
require "timeout"
require "uri"

class MCPServerTest < Minitest::Test
  include Test::Helpers::WithContext

  def test_spawn_indexer_uses_workspace_indexing_entrypoint
    with_context do |context|
      context.write!("app.rb", "class LocalWorkspaceClass; end")

      state = Rubydex::MCPServer::State.new(context.absolute_path)
      state.spawn_indexer.join
      graph = state.graph_or_error

      assert_kind_of(Rubydex::Graph, graph)
      assert_equal("LocalWorkspaceClass", graph["LocalWorkspaceClass"].name)

      rbs_kernel = graph["Kernel"].definitions.find do |definition|
        File.extname(URI(definition.location.uri).path) == ".rbs"
      end
      assert(rbs_kernel, "Expected MCP startup indexing to include core RBS definitions")
    end
  end

  def test_run_fails_when_root_path_cannot_be_canonicalized
    error = assert_raises(Errno::ENOENT) do
      Rubydex::MCPServer.run("missing-path")
    end

    assert_includes(error.message, "missing-path")
  end

  def test_codebase_stats_reports_indexing_until_graph_is_ready
    state = Rubydex::MCPServer::State.new(Dir.pwd)
    server = Rubydex::MCPServer::Server.new(server_context: state)

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
    payload = JSON.parse(response.fetch(:result).fetch(:content)[0].fetch(:text))

    assert_equal("indexing", payload.fetch("error"))
    assert_match(/still indexing/, payload.fetch("message"))
    assert_match(/retry/, payload.fetch("suggestion"))
  end
end

class MCPServerIntegrationTest < Minitest::Test
  include Test::Helpers::WithContext

  MAX_INDEXING_RETRIES = 200

  def test_executable_prints_help
    stdout, stderr, status = run_executable("--help")

    assert_predicate(status, :success?)
    assert_empty(stderr)
    assert_includes(stdout, "Rubydex MCP server for AI assistants using Ruby code intelligence")
    assert_includes(stdout, "Usage: rubydex_mcp [PATH]")
    assert_includes(stdout, "-V, --version")
    assert_includes(stdout, "Print version")
  end

  def test_executable_prints_version
    stdout, stderr, status = run_executable("--version")

    assert_predicate(status, :success?)
    assert_empty(stderr)
    assert_equal("rubydex_mcp #{Rubydex::VERSION}\n", stdout)
  end

  def test_executable_rejects_extra_arguments
    stdout, stderr, status = run_executable("foo", "bar")

    assert_equal(2, status.exitstatus)
    assert_empty(stdout)
    assert_includes(stderr, "error: unexpected argument 'bar' found")
    assert_includes(stderr, "Usage: rubydex_mcp [PATH]")
  end

  def test_executable_rejects_unknown_options
    stdout, stderr, status = run_executable("--unknown")

    assert_equal(2, status.exitstatus)
    assert_empty(stdout)
    assert_includes(stderr, "error: invalid option: --unknown")
    assert_includes(stderr, "Usage: rubydex_mcp [PATH]")
  end

  def test_mcp_server_e2e
    with_context do |context|
      context.write!("app.rb", <<~RUBY)
        class Animal
          def speak
            "..."
          end
        end

        class Dog < Animal
          def speak
            "Woof!"
          end
        end

        module Greetable
          def greet
            "Hello"
          end
        end

        module UniqueMarker
        end

        class Kennel
          def build
            Animal.new
          end
        end

      RUBY

      stderr_output = +""
      Open3.popen3(RbConfig.ruby, "-rbundler/setup", executable_path, context.absolute_path) do |stdin, stdout, stderr, wait_thr|
        stderr_reader = Thread.new { stderr_output << stderr.read }

        initialize_session(stdin, stdout)
        assert_tools_are_registered(stdin, stdout)

        request_id = 3
        stats, request_id = wait_for_indexing_to_complete(stdin, stdout, request_id)
        assert_operator(stats.fetch("files"), :>=, 2)
        assert_operator(stats.fetch("declarations"), :>, 0)

        request_id += 1
        search_response = call_tool(stdin, stdout, request_id, "search_declarations", { query: "Dog", match_mode: "exact", kind: "Class" })
        assert_has_name(search_response.fetch("results"), "Dog", "search results")
        assert_operator(search_response.fetch("total"), :>, 0)

        request_id += 1
        negative_offset_response = call_tool(stdin, stdout, request_id, "search_declarations", { query: "UniqueMarker", match_mode: "exact", offset: -1, limit: 1 })
        assert_has_name(negative_offset_response.fetch("results"), "UniqueMarker", "negative offset search results")

        request_id += 1
        declaration = call_tool(stdin, stdout, request_id, "get_declaration", { name: "Dog" })
        assert_equal("Dog", declaration.fetch("name"))
        assert_equal("Class", declaration.fetch("kind"))
        refute_empty(declaration.fetch("definitions"))
        assert_has_name(declaration.fetch("ancestors"), "Animal", "Dog ancestors")

        request_id += 1
        descendants = call_tool(stdin, stdout, request_id, "get_descendants", { name: "Animal" })
        assert_has_name(descendants.fetch("descendants"), "Dog", "Animal descendants")
        assert_operator(descendants.fetch("total"), :>, 0)

        request_id += 1
        references = call_tool(stdin, stdout, request_id, "find_constant_references", { name: "Animal" })
        refute_empty(references.fetch("references"))
        assert(references.fetch("references").all? { |entry| entry.key?("path") })
        assert_operator(references.fetch("total"), :>, 0)

        request_id += 1
        method_references = call_tool(stdin, stdout, request_id, "find_constant_references", { name: "Dog#speak()" })
        assert_equal("Dog#speak()", method_references.fetch("name"))
        assert_empty(method_references.fetch("references"))
        assert_equal(0, method_references.fetch("total"))

        request_id += 1
        file_declarations = call_tool(stdin, stdout, request_id, "get_file_declarations", { file_path: "app.rb" })
        assert(file_declarations.fetch("file").end_with?("app.rb"))
        declaration_entries = file_declarations.fetch("declarations")
        assert_has_name(declaration_entries, "Animal", "file declarations")
        assert_has_name(declaration_entries, "Dog", "file declarations")
        assert_has_name(declaration_entries, "Greetable", "file declarations")

        stdin.close
        Timeout.timeout(5) { wait_thr.value }
        stderr_reader.join
      rescue Timeout::Error
        Process.kill("TERM", wait_thr.pid)
        flunk("rubydex_mcp did not exit after stdin closed. stderr:\n#{stderr_output}")
      end
    end
  end

  private

  def run_executable(*arguments)
    Open3.capture3(
      RbConfig.ruby,
      "-rbundler/setup",
      executable_path,
      *arguments,
    )
  end

  def executable_path
    File.expand_path("../exe/rubydex_mcp", __dir__)
  end

  def send_message(stdin, message)
    stdin.puts(JSON.generate(message))
    stdin.flush
  end

  def send_request(stdin, id, method, params)
    send_message(
      stdin,
      {
        jsonrpc: "2.0",
        id: id,
        method: method,
        params: params,
      },
    )
  end

  def read_response(stdout)
    Timeout.timeout(5) do
      line = stdout.gets
      flunk("Expected JSON-RPC response, got EOF") unless line

      JSON.parse(line)
    end
  end

  def read_response_for_id(stdout, expected_id)
    response = read_response(stdout)
    assert_equal(expected_id, response.fetch("id"))
    response
  end

  def initialize_session(stdin, stdout)
    send_request(
      stdin,
      1,
      "initialize",
      {
        protocolVersion: "2025-03-26",
        capabilities: {},
        clientInfo: { name: "test-client", version: "0.1.0" },
      },
    )

    response = read_response_for_id(stdout, 1)
    assert_kind_of(Hash, response.fetch("result").fetch("capabilities").fetch("tools"))

    send_message(
      stdin,
      {
        jsonrpc: "2.0",
        method: "notifications/initialized",
      },
    )
  end

  def assert_tools_are_registered(stdin, stdout)
    send_request(stdin, 2, "tools/list", {})
    response = read_response_for_id(stdout, 2)
    tool_names = response.fetch("result").fetch("tools").map { |tool| tool.fetch("name") }

    assert_includes(tool_names, "search_declarations")
    assert_includes(tool_names, "get_declaration")
    assert_includes(tool_names, "get_descendants")
    assert_includes(tool_names, "find_constant_references")
    assert_includes(tool_names, "get_file_declarations")
    assert_includes(tool_names, "codebase_stats")
    assert_equal(6, tool_names.length)
  end

  def call_tool(stdin, stdout, request_id, tool_name, arguments)
    send_request(
      stdin,
      request_id,
      "tools/call",
      {
        name: tool_name,
        arguments: arguments,
      },
    )

    response = read_response_for_id(stdout, request_id)
    JSON.parse(response.fetch("result").fetch("content")[0].fetch("text"))
  end

  def wait_for_indexing_to_complete(stdin, stdout, request_id)
    MAX_INDEXING_RETRIES.times do
      parsed = call_tool(stdin, stdout, request_id, "codebase_stats", {})
      return [parsed, request_id] unless parsed.key?("error")

      assert_equal("indexing", parsed.fetch("error"))
      request_id += 1
      sleep(0.05)
    end

    flunk("Timed out waiting for indexing to complete")
  end

  def assert_has_name(entries, expected_name, context)
    names = entries.filter_map { |entry| entry["name"] }
    assert_includes(names, expected_name, "Expected #{context} to include #{expected_name}, got: #{names.inspect}")
  end
end
