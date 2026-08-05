# frozen_string_literal: true

require "test_helper"
require "helpers/context"
require "rubydex/server"
require "open3"
require "rbconfig"
require "tmpdir"

module Rubydex
  module Server
    # The server runs as a fresh subprocess, not a fork, so it does not inherit the loaded native
    # extension, as the real CLI does.
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

        # The runtime directory goes away on the next line, and the daemon's log is inside it.
        report_server_logs unless passed?

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

          query = "MATCH (c:Class)-[:HAS_PARENT]->(p:Class) WHERE p.name = 'Animal' RETURN c.name ORDER BY c.name"
          output = query!(context, query)

          assert_match(/Cat/, output)
          assert_match(/Dog/, output)
          assert_match(/2 rows/, output)

          assert(server_running?(context))
          assert_equal(output, query!(context, query))
        end
      end

      def test_boots_and_indexes_files_in_subdirectories
        with_context do |context|
          track(context)
          # Nested directories reach the workspace manifest's recursive walk, which can crash the
          # daemon at boot on a non-flat codebase.
          context.write!("app/models/animal.rb", "class Animal; end\n")
          context.write!("app/models/dog.rb", "class Dog < Animal; end\n")

          query = "MATCH (c:Class)-[:HAS_PARENT]->(p:Class) WHERE p.name = 'Animal' RETURN c.name"
          output = query!(context, query)

          assert_match(/Dog/, output)
          assert(server_running?(context))
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
          assert(server_running?(context))

          status_out, _, _ = rdx(["server", "status"], context)
          assert_match(/rdx server running/, status_out)
          assert_match(/pid:/, status_out)

          stop_out, _, _ = rdx(["server", "stop"], context)
          assert_match(/rdx server stopped/, stop_out)
          refute(server_running?(context))

          status_out, _, _ = rdx(["server", "status"], context)
          assert_match(/not running/, status_out)
        end
      end

      def test_restart_replaces_the_server
        with_context do |context|
          track(context)
          context.write!("foo.rb", "class Foo; end")

          rdx!(["server", "start"], context)
          first_pid = state(context).server_pid
          refute_nil(first_pid, "the started server recorded no pid")

          rdx!(["server", "restart"], context)
          second_pid = state(context).server_pid

          refute_equal(first_pid, second_pid)
          assert(server_running?(context))
        end
      end

      private

      # Never prints `state.json`: it holds the per-boot token, and CI logs are public.
      #: -> void
      def report_server_logs
        return unless @runtime_dir

        # Names and sizes only, so an absent log is distinguishable from an empty one.
        Dir.glob(File.join(@runtime_dir, "**", "*"), File::FNM_DOTMATCH).sort.each do |path|
          warn("    #{File.directory?(path) ? "dir " : "file"} #{File.size(path).to_s.rjust(7)}  #{path}")
        end

        Dir.glob(File.join(@runtime_dir, "**", "server.log")).sort.each do |path|
          warn("--- #{path} ---")
          warn(File.read(path))
        rescue StandardError => e
          warn("--- #{path} unreadable: #{e.class} ---")
        end
      end

      #: (Test::Helpers::Context context) -> void
      def track(context)
        @contexts << context
      end

      #: (Test::Helpers::Context context) -> State
      def state(context)
        State.new(workspace_path: context.absolute_path)
      end

      #: (Test::Helpers::Context context) -> bool
      def server_running?(context)
        state(context).server_running?
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

      # Reports what a failed command printed, because an unresponsive server reports on stdout and
      # a refusal on stderr.
      #: (Array[String] args, Test::Helpers::Context context) -> String
      def rdx!(args, context)
        out, err, status = rdx(args, context)
        said = { "stdout" => out, "stderr" => err }.filter_map do |name, text|
          "#{name}: #{text.strip}" unless text.strip.empty?
        end

        assert_predicate(status, :success?, ["`rdx #{args.join(" ")}` failed", *said].join("\n"))
        out
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
