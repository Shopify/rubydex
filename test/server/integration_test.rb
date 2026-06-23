# frozen_string_literal: true

require "test_helper"
require "helpers/context"
require "rubydex/server"
require "open3"
require "rbconfig"
require "tmpdir"

module Rubydex
  module Server
    # Spawns a real resident server through the `rdx` executable and exercises the client round-trip.
    #
    # The server is launched as a fresh subprocess (never forked from this test process) so it does
    # not inherit the already-loaded native extension's thread pools — exactly how the real CLI
    # behaves. Guarded to platforms that support fork + UNIX sockets.
    class IntegrationTest < Minitest::Test
      include Test::Helpers::WithContext

      LIB_DIR = File.expand_path("../../lib", __dir__) #: String
      EXE = File.expand_path("../../exe/rdx", __dir__) #: String

      def setup
        skip("server mode unsupported on this platform") unless Server.supported?

        @runtime_dir = Dir.mktmpdir("rdx-server-integration")
        @previous_server_dir = ENV["RDX_SERVER_DIR"]
        ENV["RDX_SERVER_DIR"] = @runtime_dir
        @contexts = []
      end

      def teardown
        @contexts&.each do |context|
          rdx(["server", "stop"], context)
        rescue StandardError
          nil
        end
        ENV["RDX_SERVER_DIR"] = @previous_server_dir if @runtime_dir
        FileUtils.rm_rf(@runtime_dir) if @runtime_dir
      end

      def test_warm_query_matches_repeated_calls
        with_context do |context|
          track(context)
          context.write!("zoo.rb", <<~RUBY)
            class Animal; end
            class Dog < Animal; end
            class Cat < Animal; end
          RUBY

          query = "MATCH (c:Class)-[:INHERITS]->(p:Class) WHERE p.name = 'Animal' RETURN c.name ORDER BY c.name"
          output = query!(context, query)

          assert_match(/Cat/, output)
          assert_match(/Dog/, output)
          assert_match(/2 rows/, output)

          # Server is now resident; a second call returns the same result.
          assert(server_alive?(context))
          assert_equal(output, query!(context, query))
        end
      end

      def test_server_picks_up_file_changes
        with_context do |context|
          track(context)
          context.write!("zoo.rb", "class Animal; end\nclass Dog < Animal; end\n")

          query = "MATCH (c:Class)-[:INHERITS]->(p:Class) WHERE p.name = 'Animal' RETURN c.name ORDER BY c.name"
          refute_match(/Fox/, query!(context, query))

          sleep(0.01) # ensure a distinct mtime
          context.write!("zoo.rb", "class Animal; end\nclass Dog < Animal; end\nclass Fox < Animal; end\n")

          assert_match(/Fox/, query!(context, query))
        end
      end

      def test_server_drops_deleted_files
        with_context do |context|
          track(context)
          context.write!("animal.rb", "class Animal; end")
          context.write!("dog.rb", "class Dog < Animal; end")

          query = "MATCH (c:Class {name: 'Dog'}) RETURN c.name"
          assert_match(/Dog/, query!(context, query))

          File.delete(context.absolute_path_to("dog.rb"))

          refute_match(/Dog/, query!(context, query))
        end
      end

      def test_query_output_matches_inline
        with_context do |context|
          track(context)
          context.write!("zoo.rb", "class Animal; end\nclass Dog < Animal; end\n")

          query = "MATCH (c:Class {name: 'Dog'}) RETURN c.name"
          inline, _, inline_status = rdx(["query", query], context)
          warm = query!(context, query)

          assert_predicate(inline_status, :success?)
          assert_equal(inline, warm)
        end
      end

      def test_start_status_and_stop
        with_context do |context|
          track(context)
          context.write!("foo.rb", "class Foo; end")

          out, err, status = rdx(["server", "start"], context)
          assert_predicate(status, :success?, "start failed: #{err}")
          assert_match(/rdx server started/, out)
          assert(server_alive?(context))

          status_out, _, _ = rdx(["server", "status"], context)
          assert_match(/rdx server running/, status_out)
          assert_match(/pid:/, status_out)

          stop_out, _, _ = rdx(["server", "stop"], context)
          assert_match(/rdx server stopped/, stop_out)
          refute(server_alive?(context))

          status_out, _, _ = rdx(["server", "status"], context)
          assert_match(/not running/, status_out)
        end
      end

      def test_restart_replaces_the_server
        with_context do |context|
          track(context)
          context.write!("foo.rb", "class Foo; end")

          rdx(["server", "start"], context)
          first_pid = cache(context).server_pid
          refute_nil(first_pid)

          rdx(["server", "restart"], context)
          second_pid = cache(context).server_pid

          refute_equal(first_pid, second_pid)
          assert(server_alive?(context))
        end
      end

      private

      #: (Test::Helpers::Context context) -> void
      def track(context)
        @contexts << context
      end

      #: (Test::Helpers::Context context) -> Cache
      def cache(context)
        Cache.new(workspace_path: context.absolute_path)
      end

      #: (Test::Helpers::Context context) -> bool
      def server_alive?(context)
        cache(context).server_alive?
      end

      #: (Array[String] args, Test::Helpers::Context context) -> [String, String, Process::Status]
      def rdx(args, context)
        Open3.capture3(
          RbConfig.ruby,
          "-I",
          LIB_DIR,
          EXE,
          *args,
          chdir: context.absolute_path,
        )
      end

      #: (Test::Helpers::Context context, String query) -> String
      def query!(context, query)
        out, err, status = rdx(["query", query, "--server"], context)
        assert_predicate(status, :success?, "query failed: #{err}")
        out
      end
    end
  end
end
