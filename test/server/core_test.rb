# frozen_string_literal: true

require "test_helper"
require "rubydex/server"
require "socket"
require "tmpdir"

module Rubydex
  module Server
    # A resident server serves every client, so one bad request, unreadable file, or bug must not
    # stop it for the rest.
    class CoreTest < Minitest::Test
      def setup
        skip("server mode is not supported on this platform") unless Server.supported?

        @runtime_dir = Dir.mktmpdir("rdx-core-test")
        @workspace = Dir.mktmpdir("rdx-core-workspace")
        @previous_server_dir = ENV["RDX_SERVER_DIR"]
        ENV["RDX_SERVER_DIR"] = @runtime_dir
        @state = State.new(workspace_path: @workspace)
        @state.record!
      end

      def teardown
        ENV["RDX_SERVER_DIR"] = @previous_server_dir
        FileUtils.rm_rf(@runtime_dir)
        FileUtils.rm_rf(@workspace)
      end

      # An error in one command must not reach `run` and stop the server for every other client;
      # it must cost one connection.
      def test_an_unexpected_error_answers_the_client_and_keeps_the_server
        core = Core.new(@state)
        core.define_singleton_method(:dispatch) { |_request, _client| raise "boom" }

        response = exchange_with(core, { "command" => "query", "query" => "MATCH (n) RETURN n" })

        assert_equal(1, response["status"])
        assert_match(/internal error/, response["stderr"])
        assert_match(/boom/, response["stderr"])

        assert_match(/internal error: RuntimeError: boom/, @log)
        assert_match(/core_test\.rb/, @log, "the log must keep the backtrace")
      end

      def test_an_unauthorized_request_is_refused_and_never_dispatched
        core = Core.new(@state)
        dispatched = false
        core.define_singleton_method(:dispatch) { |_request, _client| dispatched = true }

        response = exchange_with(core, { "command" => "query" }, token: "not-the-token")

        assert_equal(1, response["status"])
        assert_match(/unauthorized/, response["stderr"])
        refute(dispatched, "an unauthorized request must never reach dispatch")
      end

      # The extension raises `TypeError` for a non-string, and no rescue caught it, so it reached
      # `run` and stopped the server.
      def test_a_request_without_a_query_string_is_a_client_error
        core = Core.new(@state)

        [nil, 12345, ["MATCH (n) RETURN n"], { "q" => 1 }].each do |query|
          response = core.send(:handle_query, { "command" => "query", "query" => query })

          assert_equal(1, response["status"], "expected #{query.inspect} to be refused")
          assert_match(/no query string/, response["stderr"])
        end
      end

      def test_a_request_with_a_malformed_query_format_is_a_client_error
        # A real graph ensures the request reaches the extension and raises the `TypeError` this
        # test prevents, not a missing-graph error.
        core = with_graph

        # `false` is in the list because `|| "table"` would turn it into the default.
        [12345, ["json"], { "format" => "json" }, false].each do |format|
          response = core.send(
            :handle_query,
            { "command" => "query", "query" => "MATCH (n) RETURN n", "query_format" => format },
          )

          assert_equal(1, response["status"], "expected #{format.inspect} to be refused")
          assert_match(/no query format string/, response["stderr"])
        end
      end

      def test_a_request_without_a_query_format_uses_the_default
        core = with_graph

        response = core.send(:handle_query, { "command" => "query", "query" => "MATCH (c:Class) RETURN c.name" })

        assert_equal(0, response["status"])
      end

      # Invalid Cypher is the caller's mistake, so it must return the parser's message, not an
      # internal error.
      def test_invalid_cypher_is_reported_as_a_user_error
        core = with_graph

        response = exchange_with(core, { "command" => "query", "query" => "NOT A QUERY" })

        assert_equal(1, response["status"])
        assert_match(/Cypher syntax error/, response["stderr"])
        refute_match(/internal error/, response["stderr"])
      end

      private

      # The graph is empty because these tests check request handling, not what the graph holds.
      #: -> Core
      def with_graph
        core = Core.new(@state)
        core.instance_variable_set(:@graph, Rubydex::Graph.configure_for_workspace(@state.workspace_path))
        core
      end

      #: (Core core, Hash[String, untyped] payload, ?token: String?) -> Hash[untyped, untyped]
      def exchange_with(core, payload, token: nil)
        server_side, client_side = UNIXSocket.pair

        client = Thread.new do
          client_side.gets
          Frame.write(client_side, payload.merge("token" => token || @state.token))
          Frame.read_response(client_side, total_timeout: 10.0)
        ensure
          client_side.close
        end

        @log, _err = capture_io { core.send(:handle, server_side) }
        client.value
      ensure
        client&.join
      end
    end
  end
end
