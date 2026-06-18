# frozen_string_literal: true

require "test_helper"
require "rubydex/server"
require "tmpdir"

module Rubydex
  module Server
    class CacheTest < Minitest::Test
      def setup
        @runtime_dir = Dir.mktmpdir("rdx-server-test")
        @previous_server_dir = ENV["RDX_SERVER_DIR"]
        ENV["RDX_SERVER_DIR"] = @runtime_dir
      end

      def teardown
        ENV["RDX_SERVER_DIR"] = @previous_server_dir
        FileUtils.rm_rf(@runtime_dir)
      end

      def test_app_id_is_stable_for_the_same_workspace
        a = Cache.new(workspace_path: "/some/workspace")
        b = Cache.new(workspace_path: "/some/workspace")

        assert_equal(a.app_id, b.app_id)
      end

      def test_app_id_differs_between_workspaces
        a = Cache.new(workspace_path: "/workspace/a")
        b = Cache.new(workspace_path: "/workspace/b")

        refute_equal(a.app_id, b.app_id)
      end

      def test_expected_version_includes_gem_version
        cache = Cache.new(workspace_path: "/workspace")
        assert_match(/\A#{Regexp.escape(Rubydex::VERSION)}:/, cache.expected_version)
      end

      def test_ensure_dir_creates_directory_with_restrictive_permissions
        cache = Cache.new(workspace_path: "/workspace")
        cache.ensure_dir!

        assert(File.directory?(cache.dir))

        # POSIX permission bits aren't meaningful on Windows (where server mode is unsupported).
        skip("POSIX permissions are not enforced on this platform") if Gem.win_platform?

        mode = File.stat(cache.dir).mode & 0o777
        assert_equal(0o700, mode)
      end

      def test_token_is_generated_and_persisted_with_restrictive_permissions
        cache = Cache.new(workspace_path: "/workspace")
        token = cache.token

        refute_empty(token)
        assert(File.exist?(cache.token_path))

        # A fresh cache for the same workspace reads the same token back.
        assert_equal(token, Cache.new(workspace_path: "/workspace").token)

        # POSIX permission bits aren't meaningful on Windows (where server mode is unsupported).
        skip("POSIX permissions are not enforced on this platform") if Gem.win_platform?

        assert_equal(0o600, File.stat(cache.token_path).mode & 0o777)
      end

      def test_write_metadata_records_pid_and_version
        cache = Cache.new(workspace_path: "/workspace")
        cache.write_metadata!

        assert_equal(Process.pid, cache.server_pid)
        assert(cache.version_compatible?)
      end

      def test_version_compatible_is_false_without_version_file
        cache = Cache.new(workspace_path: "/workspace")
        refute(cache.version_compatible?)
      end

      def test_version_compatible_is_false_on_mismatch
        cache = Cache.new(workspace_path: "/workspace")
        cache.ensure_dir!
        File.write(cache.version_path, "9.9.9:deadbeef")

        refute(cache.version_compatible?)
      end

      def test_server_alive_is_false_without_pid_file
        cache = Cache.new(workspace_path: "/workspace")
        refute(cache.server_alive?)
      end

      def test_server_alive_is_false_for_dead_pid
        skip("fork is unavailable on this platform") unless Process.respond_to?(:fork)

        cache = Cache.new(workspace_path: "/workspace")
        cache.ensure_dir!
        # Spawn a child and reap it so we have a pid that is guaranteed to no longer be alive.
        dead_pid = fork { exit }
        Process.wait(dead_pid)
        File.write(cache.pid_path, dead_pid.to_s)

        refute(cache.server_alive?)
      end

      def test_clean_removes_runtime_files
        cache = Cache.new(workspace_path: "/workspace")
        cache.write_metadata!
        File.write(cache.socket_path, "")

        cache.clean!

        refute(File.exist?(cache.pid_path))
        refute(File.exist?(cache.version_path))
        refute(File.exist?(cache.socket_path))
      end

      def test_runtime_dir_honors_override
        cache = Cache.new(workspace_path: "/workspace")
        assert(cache.dir.start_with?(@runtime_dir))
      end
    end
  end
end
