# frozen_string_literal: true

require "test_helper"
require "helpers/context"
require "json"
require "open3"
require "rbconfig"
require "rubydex/mcp_server"
require "timeout"

class MCPServerIntegrationTest < Minitest::Test
  include Test::Helpers::WithContext

  MAX_INDEXING_RETRIES = 200

  def test_executable_prints_help
    stdout, _stderr, status = run_executable("--help")

    assert_predicate(status, :success?)
    assert_includes(stdout, "mcp [PATH]")
    assert_includes(stdout, "Run the MCP server for AI assistants")
  end

  def test_executable_prints_version
    stdout, _stderr, status = run_executable("--version")

    assert_predicate(status, :success?)
    assert_equal("v#{Rubydex::VERSION}\n", stdout)
  end

  def test_executable_rejects_extra_arguments
    stdout, stderr, status = run_executable("mcp", "foo", "bar")

    assert_equal(1, status.exitstatus)
    assert_empty(stdout)
    assert_includes(stderr, "unexpected argument: bar")
    assert_includes(stderr, "mcp [PATH]")
  end

  def test_executable_rejects_unknown_options
    stdout, stderr, status = run_executable("mcp", "--unknown")

    assert_equal(1, status.exitstatus)
    assert_empty(stdout)
    assert_includes(stderr, "invalid option: --unknown")
    assert_includes(stderr, "mcp [PATH]")
  end

  def test_mcp_server_can_be_required_directly
    stdout, stderr, status = Open3.capture3(
      RbConfig.ruby,
      "-rbundler/setup",
      "-Ilib",
      "-e",
      <<~RUBY,
        require "rubydex/mcp_server"

        puts Rubydex::VERSION
        puts Rubydex::Graph.name

        response = Rubydex::MCPServer::Server.new(root_path: Dir.pwd).handle(
          jsonrpc: "2.0",
          id: 1,
          method: "initialize",
        )
        puts response.fetch(:result).fetch(:serverInfo).fetch(:version)
      RUBY
    )

    assert_predicate(status, :success?, stderr)
    assert_equal("#{Rubydex::VERSION}\nRubydex::Graph\n#{Rubydex::VERSION}\n", stdout)
  end

  def test_mcp_server_e2e
    with_context do |context|
      context.write!("app.rb", "class Dog; end")

      stderr_output = +""
      Open3.popen3(RbConfig.ruby, "-rbundler/setup", executable_path, "mcp", context.absolute_path) do |stdin, stdout, stderr, wait_thr|
        stderr_reader = Thread.new do
          stderr_output << stderr.read
        rescue IOError
          # This thread only drains stderr. If teardown closes the pipe while read is blocked, there is
          # no more stderr to capture and the subprocess exit status is still checked through wait_thr.
        end

        initialize_session(stdin, stdout)
        assert_tools_are_registered(stdin, stdout)

        stats, request_id = wait_for_indexing_to_complete(stdin, stdout, 3)
        assert_operator(stats.fetch("files"), :>=, 1)
        assert_operator(stats.fetch("declarations"), :>, 0)

        search_response = call_tool(stdin, stdout, request_id + 1, "search_declarations", { query: "Dog", match_mode: "exact" })
        assert_equal(["Dog"], search_response.fetch("results").map { |result| result.fetch("name") })

        stdin.close
        Timeout.timeout(30) { wait_thr.value }
        stderr_reader.join
      rescue Timeout::Error
        Process.kill("TERM", wait_thr.pid)
        flunk("rdx mcp did not exit after stdin closed. stderr:\n#{stderr_output}")
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
    File.expand_path("../../exe/rdx", __dir__)
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

    assert_equal(
      [
        "codebase_stats",
        "find_constant_references",
        "get_declaration",
        "get_descendants",
        "get_file_declarations",
        "search_declarations",
      ],
      tool_names.sort,
    )
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
end
