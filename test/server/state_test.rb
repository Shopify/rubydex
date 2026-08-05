# frozen_string_literal: true

require "test_helper"
require "rubydex/server"
require "tmpdir"

module Rubydex
  module Server
    # Shared: the runtime suite proves the directory code refuses without the flag, and the
    # platform suite proves that `supported?` reports the same.
    module NofollowStub
      private

      # A value of `nil` represents a platform that lacks the flag.
      #: [T] (Integer? value) { -> T } -> T
      def with_nofollow(value)
        original = State::NOFOLLOW
        State.send(:remove_const, :NOFOLLOW)
        State.const_set(:NOFOLLOW, value)
        yield
      ensure
        State.send(:remove_const, :NOFOLLOW)
        State.const_set(:NOFOLLOW, original)
      end
    end

    class StateIdentityTest < Minitest::Test
      def test_app_id_is_stable_for_the_same_workspace
        a = State.new(workspace_path: "/some/workspace")
        b = State.new(workspace_path: "/some/workspace")

        assert_equal(a.app_id, b.app_id)
      end

      def test_app_id_differs_between_workspaces
        a = State.new(workspace_path: "/workspace/a")
        b = State.new(workspace_path: "/workspace/b")

        refute_equal(a.app_id, b.app_id)
      end

      # A protocol change must map the workspace to a different runtime directory, or a new client
      # reaches a server that speaks the older wire format.
      def test_app_id_differs_between_protocol_versions
        before = State.new(workspace_path: "/some/workspace").app_id
        after = with_protocol(Server::PROTOCOL + 1) do
          State.new(workspace_path: "/some/workspace").app_id
        end

        refute_equal(before, after)
      end

      def test_expected_version_includes_gem_version
        state = State.new(workspace_path: "/workspace")
        assert_match(/\A#{Regexp.escape(Rubydex::VERSION)}:/, state.expected_version)
      end

      private

      # `remove_const` prevents the Ruby warning about the constant reassignment.
      #: [T] (Integer value) { -> T } -> T
      def with_protocol(value)
        original = Server::PROTOCOL
        Server.send(:remove_const, :PROTOCOL)
        Server.const_set(:PROTOCOL, value)
        yield
      ensure
        Server.send(:remove_const, :PROTOCOL)
        Server.const_set(:PROTOCOL, original)
      end
    end

    class StateTest < Minitest::Test
      include NofollowStub

      def setup
        skip("server mode is not supported on this platform") unless Server.supported?

        @runtime_dir = File.realpath(Dir.mktmpdir("rdx-server-test"))
        @previous_server_dir = ENV["RDX_SERVER_DIR"]
        ENV["RDX_SERVER_DIR"] = @runtime_dir
      end

      def teardown
        ENV["RDX_SERVER_DIR"] = @previous_server_dir
        FileUtils.rm_rf(@runtime_dir)
      end

      def test_ensure_dir_creates_directory_with_restrictive_permissions
        state = State.new(workspace_path: "/workspace")
        state.ensure_dir!

        assert(File.directory?(state.dir))

        skip("POSIX permissions are not enforced on this platform") if Gem.win_platform?

        assert_equal(0o700, File.stat(state.dir).mode & 0o777)
      end

      def test_open_lock_creates_the_file_with_restrictive_permissions
        state = State.new(workspace_path: "/workspace")
        state.open_lock.close

        assert(File.exist?(state.lock_path))

        skip("POSIX permissions are not enforced on this platform") if Gem.win_platform?

        assert_equal(0o600, File.stat(state.lock_path).mode & 0o777)
      end

      def test_record_writes_the_state_file_with_restrictive_permissions
        state = State.new(workspace_path: "/workspace")
        state.record!

        assert(File.exist?(state.state_path))

        skip("POSIX permissions are not enforced on this platform") if Gem.win_platform?

        assert_equal(0o600, File.stat(state.state_path).mode & 0o777)
      end

      # A rename replaces the whole file, so no reader sees a partial record and no temporary file
      # survives the write.
      def test_record_leaves_no_temporary_file_behind
        state = State.new(workspace_path: "/workspace")
        state.record!

        assert_equal(["state.json"], Dir.children(state.dir).sort)
      end

      def test_record_stores_the_identity_of_this_process
        state = State.new(workspace_path: "/workspace")
        state.record!

        assert_equal(Process.pid, state.server_pid)
        assert(state.version_compatible?)
        refute_empty(state.token)
        assert_match(/\A\d{4}-\d{2}-\d{2}T/, state.started_at)
      end

      # The token authenticates a request, so a client that cached a previous server's token must
      # not talk to the new one.
      def test_record_issues_a_fresh_token_on_every_boot
        state = State.new(workspace_path: "/workspace")

        state.record!
        first = State.new(workspace_path: "/workspace").token
        state.record!
        second = State.new(workspace_path: "/workspace").token

        refute_empty(first)
        refute_equal(first, second)
      end

      def test_read_is_nil_without_a_state_file
        state = State.new(workspace_path: "/workspace")

        assert_nil(state.read)
        assert_nil(state.token)
        assert_nil(state.server_pid)
        assert_nil(state.started_at)
        refute(state.version_compatible?)
      end

      # `write_state` renames a temporary file, so this record came from somewhere else. `read`
      # must return `nil` for it, and must not raise.
      def test_read_tolerates_a_half_written_record
        state = State.new(workspace_path: "/workspace")
        state.ensure_dir!
        File.write(state.state_path, '{"pid": 12')

        assert_nil(state.read)
        assert_nil(state.server_pid)
      end

      def test_server_pid_rejects_an_implausible_value
        state = State.new(workspace_path: "/workspace")
        state.ensure_dir!

        [0, -1, "4711", nil].each do |value|
          File.write(state.state_path, JSON.dump({ "pid" => value }))
          assert_nil(state.server_pid, "expected #{value.inspect} to be rejected")
        end
      end

      def test_server_running_is_false_without_a_state_file
        state = State.new(workspace_path: "/workspace")

        refute(state.server_running?)
      end

      # The lock decides ownership, so a live recorded pid without a lock is not a running server.
      def test_server_running_ignores_a_live_but_unrelated_recorded_pid
        state = State.new(workspace_path: "/workspace")
        state.ensure_dir!
        File.write(state.state_path, JSON.dump({ "pid" => Process.pid, "version" => state.expected_version }))

        assert_equal(Process.pid, state.server_pid)
        refute(state.server_running?)
      end

      def test_server_running_follows_the_lock_across_processes
        skip("fork is unavailable on this platform") unless Process.respond_to?(:fork)

        state = State.new(workspace_path: "/workspace")

        with_locking_child(state) do
          assert(state.server_running?, "expected the lock held by the child to count as running")
        end

        refute(state.server_running?, "expected the lock to go free once the holder died")
      end

      def test_clean_removes_the_socket_but_keeps_the_lock_file
        state = State.new(workspace_path: "/workspace")
        state.open_lock.close
        File.write(state.socket_path, "")

        state.clean!

        refute(File.exist?(state.socket_path))
        # A lock belongs to an inode, so deleting this path would let two servers start at once.
        assert(File.exist?(state.lock_path))
      end

      def test_clean_leaves_the_socket_of_a_locked_workspace
        skip("fork is unavailable on this platform") unless Process.respond_to?(:fork)

        state = State.new(workspace_path: "/workspace")

        with_locking_child(state) do
          File.write(state.socket_path, "")

          state.clean!

          assert(File.exist?(state.socket_path))
        end
      end

      # On Windows an exclusive lock denies read access to the whole file, so the locked file stays
      # empty and the record lives outside it.
      def test_the_record_stays_readable_while_a_server_holds_the_lock
        skip("fork is unavailable on this platform") unless Process.respond_to?(:fork)

        state = State.new(workspace_path: "/workspace")

        with_locking_child(state) do |child_pid|
          refute_equal(state.lock_path, state.state_path)
          assert_equal(0, File.size(state.lock_path), "the locked file must never carry data")

          assert_equal(child_pid, state.server_pid)
          assert(state.version_compatible?)
          refute_nil(state.token)
        end
      end

      # The runtime files go in a subdirectory this code creates, so `RDX_SERVER_DIR` keeps the
      # permissions its owner set.
      def test_ensure_dir_leaves_the_override_directory_alone
        skip("POSIX permissions are not enforced on this platform") if Gem.win_platform?

        File.chmod(0o755, @runtime_dir)
        state = State.new(workspace_path: "/workspace")
        state.ensure_dir!

        assert_equal(
          0o755,
          File.stat(@runtime_dir).mode & 0o777,
          "the directory named by RDX_SERVER_DIR must keep its own permissions",
        )
        assert_equal(0o700, File.stat(state.dir).mode & 0o777)
        assert_equal(0o700, File.stat(File.dirname(state.dir)).mode & 0o777)
      end

      def test_the_runtime_directory_nests_under_the_override
        state = State.new(workspace_path: "/workspace")
        uid = Process.respond_to?(:uid) ? Process.uid : "nobody"

        assert_equal(File.join(@runtime_dir, "rubydex-#{uid}", state.app_id), state.dir)
      end

      # `chmod` follows symlinks and the runtime root lives in a world-writable temp dir, so a
      # planted link is refused before it changes the target's permissions.
      def test_ensure_dir_refuses_a_symlinked_runtime_root
        skip("symlinks are unavailable on this platform") if Gem.win_platform?

        target = Dir.mktmpdir("rdx-symlink-target")
        File.chmod(0o755, target)
        uid = Process.respond_to?(:uid) ? Process.uid : "nobody"
        File.symlink(target, File.join(@runtime_dir, "rubydex-#{uid}"))

        state = State.new(workspace_path: "/workspace")
        error = assert_raises(Error) { state.ensure_dir! }

        assert_match(/is a symlink/, error.message)
        assert_equal(
          0o755,
          File.stat(target).mode & 0o777,
          "an unrelated directory must not have its permissions changed",
        )
      ensure
        FileUtils.rm_rf(target) if target
      end

      def test_runtime_dir_honors_override
        state = State.new(workspace_path: "/workspace")
        assert(state.dir.start_with?(@runtime_dir))
      end

      def test_a_runtime_root_open_to_others_is_refused_before_locking
        state = State.new(workspace_path: "/workspace")
        state.ensure_dir!
        File.chmod(0o755, File.dirname(state.dir))

        error = assert_raises(Error) { state.server_running? }

        assert_match(/is not private to this user/, error.message)
      end

      # The lock and state file reject a symlink only on their own last component, so the runtime
      # root is checked before the client trusts its socket or token.
      def test_a_symlinked_runtime_root_is_refused_before_reading
        state = State.new(workspace_path: "/workspace")
        state.ensure_dir!
        root = File.dirname(state.dir)
        elsewhere = File.join(@runtime_dir, "elsewhere")
        FileUtils.mv(root, elsewhere)
        File.symlink(elsewhere, root)

        assert_raises(Error) { state.token }
        assert_raises(Error) { state.server_running? }
      end

      def test_liveness_is_false_before_anything_is_created
        refute_predicate(State.new(workspace_path: "/workspace"), :server_running?)
      end

      def test_a_base_writable_by_others_is_refused
        open_base = File.join(@runtime_dir, "shared")
        FileUtils.mkdir_p(open_base)
        File.chmod(0o777, open_base)
        ENV["RDX_SERVER_DIR"] = open_base

        error = assert_raises(Error) { State.new(workspace_path: "/workspace").ensure_dir! }

        assert_match(/writable by other users and is not sticky/, error.message)
      end

      def test_a_sticky_base_writable_by_others_is_accepted
        sticky_base = File.join(@runtime_dir, "sticky")
        FileUtils.mkdir_p(sticky_base)
        File.chmod(0o1777, sticky_base)
        ENV["RDX_SERVER_DIR"] = sticky_base

        state = State.new(workspace_path: "/workspace")
        state.ensure_dir!

        assert_equal(0o700, File.stat(state.dir).mode & 0o777)
      end

      def test_a_base_that_does_not_exist_is_refused_rather_than_created
        missing = File.join(@runtime_dir, "absent")
        ENV["RDX_SERVER_DIR"] = missing

        error = assert_raises(Error) { State.new(workspace_path: "/workspace").ensure_dir! }

        assert_match(/cannot hold the server runtime directory/, error.message)
        refute_path_exists(missing)
      end

      # The base is resolved first because the link entry itself, not its target, is what an owner
      # can swap.
      def test_the_base_is_resolved_before_it_is_checked
        open_parent = File.join(@runtime_dir, "open")
        real_base = File.join(open_parent, "real")
        FileUtils.mkdir_p(real_base)
        File.chmod(0o777, open_parent)
        link = File.join(@runtime_dir, "link")
        File.symlink(real_base, link)
        ENV["RDX_SERVER_DIR"] = link

        error = assert_raises(Error) { State.new(workspace_path: "/workspace").ensure_dir! }

        assert_match(%r{#{Regexp.escape(File.realpath(open_parent))} is writable by other users}, error.message)
      end

      def test_a_planted_symlink_never_becomes_the_state_file
        state = State.new(workspace_path: "/workspace")
        state.ensure_dir!
        victim = File.join(@runtime_dir, "victim")
        File.write(victim, "precious")
        File.symlink(victim, "#{state.state_path}.#{Process.pid}")

        assert_raises(Error) { state.record! }

        assert_equal("precious", File.read(victim))
      end

      def test_a_planted_symlink_never_becomes_the_lock
        state = State.new(workspace_path: "/workspace")
        state.ensure_dir!
        victim = File.join(@runtime_dir, "victim")
        File.write(victim, "precious")
        File.symlink(victim, state.lock_path)

        assert_raises(Error) { state.server_running? }

        assert_equal("precious", File.read(victim))
      end

      def test_a_platform_without_nofollow_refuses_to_create_the_directory
        state = State.new(workspace_path: "/workspace")

        error = assert_raises(Error) do
          with_nofollow(nil) { state.ensure_dir! }
        end

        assert_match(/cannot open a path without following symlinks/, error.message)
      end

      private

      # The child is killed so the kernel releases the lock, not the child.
      #: [T] (State state) { (Integer child_pid) -> T } -> T
      def with_locking_child(state)
        reader, writer = IO.pipe

        pid = fork do
          reader.close
          file = state.open_lock
          file.flock(File::LOCK_EX)
          state.record!
          writer.puts("locked")
          writer.close
          sleep(60)
        end

        writer.close
        reader.gets # Waits until the child holds the lock and writes its record.
        yield(pid)
      ensure
        reader&.close
        if pid
          begin
            Process.kill("KILL", pid)
            Process.wait(pid)
          rescue Errno::ESRCH, Errno::ECHILD
            nil
          end
        end
      end
    end

    # Runs on every platform, including the ones that cannot serve. The suites above skip there, so
    # without this nothing would check what an unsupported platform reports.
    class PlatformSupportTest < Minitest::Test
      include NofollowStub

      def test_a_platform_without_nofollow_does_not_support_server_mode
        with_nofollow(nil) { refute_predicate(Server, :supported?) }
      end

      def test_windows_does_not_support_server_mode
        skip("this platform is not Windows") unless Gem.win_platform?

        refute_predicate(Server, :supported?)
      end

      # Every path that runs only when `supported?` is true relies on `nofollow!` never raising.
      def test_a_supported_platform_always_has_the_nofollow_flag
        skip("server mode is not supported on this platform") unless Server.supported?

        refute_nil(State::NOFOLLOW)
      end
    end
  end
end
