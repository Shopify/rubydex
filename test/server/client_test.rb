# frozen_string_literal: true

require "test_helper"
require "rubydex/server"
require "socket"
require "stringio"
require "timeout"
require "tmpdir"

module Rubydex
  module Server
    # A child process takes the place of a real server, so each test controls when the lock, the
    # record, and the socket appear.
    class ClientTest < Minitest::Test
      def setup
        skip("server mode is not supported on this platform") unless Server.supported?

        @runtime_dir = Dir.mktmpdir("rdx-client-test")
        @previous_server_dir = ENV["RDX_SERVER_DIR"]
        ENV["RDX_SERVER_DIR"] = @runtime_dir
        @children = [] #: Array[Integer]
      end

      def teardown
        @children.each do |pid|
          Process.kill("KILL", pid)
          Process.wait(pid)
        rescue Errno::ESRCH, Errno::ECHILD
          nil
        end
        ENV["RDX_SERVER_DIR"] = @previous_server_dir
        FileUtils.rm_rf(@runtime_dir)
      end

      # A lock held during startup marks a booting server, so a second client must wait, not
      # fail.
      def test_start_waits_for_a_server_that_is_still_booting
        skip("fork is unavailable on this platform") unless Process.respond_to?(:fork)

        state = State.new(workspace_path: "/workspace")
        spawn_owner(state, bind_after: 0.4)

        started = monotonic
        Client.start(state)
        waited = monotonic - started

        assert(File.socket?(state.socket_path), "expected the owner's socket to exist")
        assert_operator(waited, :>=, 0.3, "expected the client to wait for the slow boot")
        assert_operator(waited, :<, 30.0, "expected the client to return as soon as the boot finished")
      end

      def test_start_fails_fast_when_a_booting_server_dies
        skip("fork is unavailable on this platform") unless Process.respond_to?(:fork)

        state = State.new(workspace_path: "/workspace")
        spawn_owner(state, bind: false, die_after: 0.3)

        started = monotonic
        error = assert_raises(Error) { Client.start(state) }
        waited = monotonic - started

        assert_match(/exited during startup/, error.message)
        assert_operator(waited, :<, 30.0, "expected a fail-fast, not the full boot timeout")
      end

      # The readiness rule must reject a dead server's socket and compatible record, or a client
      # connects to a dead socket.
      def test_a_stale_socket_beside_a_compatible_record_is_not_ready
        skip("fork is unavailable on this platform") unless Process.respond_to?(:fork)

        state = State.new(workspace_path: "/workspace")
        pid = spawn_owner(state)
        wait_for { File.socket?(state.socket_path) }
        kill_owner(pid)
        wait_for { !state.server_running? }

        assert(File.socket?(state.socket_path))
        assert(state.version_compatible?)
        refute(state.server_running?)

        error = assert_raises(Error) do
          Client.send(:wait_until_ready, state, previous: nil)
        end

        assert_match(/exited during startup/, error.message)
      end

      # The server is single-threaded, so one stuck query keeps it inside `handle` and it never
      # reaches `accept` again.
      def test_status_reports_a_server_that_never_answers
        skip("fork is unavailable on this platform") unless Process.respond_to?(:fork)

        state = State.new(workspace_path: "/workspace")
        pid = spawn_owner(state, accept: false)
        wait_for { File.socket?(state.socket_path) }

        out = StringIO.new
        started = monotonic
        status = nil #: Integer?
        calls = watching_client_mutations do
          Timeout.timeout(20) { status = Commands.status(state, stdout: out, timeout: 0.3) }
        end
        waited = monotonic - started

        assert_equal(1, status)
        assert_match(/did not answer within/, out.string)
        assert_match(/recorded pid:\s+#{pid}\b/, out.string)
        assert_match(/recorded start:\s+\d{4}-\d{2}-\d{2}T/, out.string)
        assert_operator(waited, :<, 5.0, "status must not wait beyond its budget")
        # A long query looks the same as a stuck one from outside, so the report must say so or
        # the reader kills a live process.
        assert_match(/a long query looks the same/, out.string)
        assert_match(/only if it stays silent/, out.string)
        # The log holds the boot and any failure, not the current request, so the report must not
        # send the reader there for it.
        assert_match(/not the request in flight/, out.string)
        # A new server cannot take the lock from a live one, so `status` must not restart.
        assert_empty(calls, "status must not start, stop or restart a server")
      end

      # The server accepts and sends the version line, then answers nothing; the handshake
      # succeeds, so the response budget bounds this wait.
      def test_status_reports_a_server_that_greets_and_then_goes_silent
        skip("fork is unavailable on this platform") unless Process.respond_to?(:fork)

        state = State.new(workspace_path: "/workspace")
        pid = spawn_owner(state, greet: true)
        wait_for { File.socket?(state.socket_path) }

        out = StringIO.new
        started = monotonic
        status = nil #: Integer?
        calls = watching_client_mutations do
          Timeout.timeout(20) { status = Commands.status(state, stdout: out, timeout: 0.3) }
        end
        waited = monotonic - started

        assert_equal(1, status)
        assert_match(/did not answer within/, out.string)
        assert_match(/recorded pid:\s+#{pid}\b/, out.string)
        assert_operator(waited, :<, 5.0, "the response budget must end this wait")
        assert_empty(calls, "status must not start, stop or restart a server")
      end

      # The owner holds the lock but has not bound its socket, so `status` must read the record
      # and not block on a socket that does not exist.
      def test_status_reports_an_owner_without_a_socket
        skip("fork is unavailable on this platform") unless Process.respond_to?(:fork)

        state = State.new(workspace_path: "/workspace")
        pid = spawn_owner(state, bind: false)

        out = StringIO.new
        calls = watching_client_mutations { @status = Commands.status(state, stdout: out, timeout: 0.3) }

        assert_equal(1, @status)
        assert_match(/has not created its socket/, out.string)
        assert_match(/recorded pid:\s+#{pid}\b/, out.string)
        assert_empty(calls)
      end

      def test_status_reports_a_workspace_that_no_server_owns
        state = State.new(workspace_path: "/workspace")

        out = StringIO.new
        status = Commands.status(state, stdout: out, timeout: 0.3)

        assert_equal(0, status)
        assert_match(/not running/, out.string)
      end

      # `server_running?` acquires the lock to test it. A child that locks after the fork can find
      # the lock held by its own parent, and then it exits without serving.
      def test_start_holds_the_lock_before_it_forks
        state = State.new(workspace_path: "/workspace")
        held = nil #: bool?

        with_stubbed_fork(-> { held = state.server_running? }) do
          Client.start(state)
        end

        assert(held, "the lock must already be held when the fork happens")
      end

      private

      # `fork` is a private `Kernel` method, so `Minitest#stub` cannot reach it.
      #: [T] (^() -> void observer) { -> T } -> T
      def with_stubbed_fork(observer)
        singleton = Client.singleton_class
        singleton.send(:alias_method, :original_wait_until_ready, :wait_until_ready)
        singleton.send(:define_method, :wait_until_ready) { |*, **| nil }
        singleton.send(:define_method, :fork) { |&_block| observer.call && nil }
        yield
      ensure
        singleton.send(:remove_method, :fork)
        singleton.send(:remove_method, :wait_until_ready)
        singleton.send(:alias_method, :wait_until_ready, :original_wait_until_ready)
        singleton.send(:remove_method, :original_wait_until_ready)
      end

      # `status` must not start, stop, or restart a server.
      #: [T] { -> T } -> Array[Symbol]
      def watching_client_mutations
        names = [:ensure_server, :start, :restart, :stop]
        originals = names.to_h { |name| [name, Client.method(name)] }
        calls = [] #: Array[Symbol]

        names.each do |name|
          Client.define_singleton_method(name) do |*_args, **_kwargs|
            calls << name
            nil
          end
        end

        yield
        calls
      ensure
        originals&.each { |name, method| Client.define_singleton_method(name, method) }
      end

      #: -> Float
      def monotonic
        Process.clock_gettime(Process::CLOCK_MONOTONIC)
      end

      # Each keyword simulates a distinct real server state.
      #: (State state, ?bind: bool, ?bind_after: Float, ?accept: bool, ?greet: bool, ?die_after: Float?) -> Integer
      def spawn_owner(state, bind: true, bind_after: 0.0, accept: true, greet: false, die_after: nil)
        reader, writer = IO.pipe

        pid = fork do
          reader.close
          lock = state.open_lock
          lock.flock(File::LOCK_EX)
          state.record!
          writer.puts("owned")
          writer.close

          if die_after
            sleep(die_after)
            exit!(1)
          end

          if bind
            sleep(bind_after) if bind_after.positive?
            # Bind by basename so a long runtime path does not overflow `sockaddr_un`.
            server = Dir.chdir(File.dirname(state.socket_path)) do
              UNIXServer.new(File.basename(state.socket_path))
            end
            if accept
              client = server.accept
              client.puts(state.expected_version) if greet
            end
          end

          sleep(60)
        end

        @children << pid
        writer.close
        reader.gets # Waits until the child holds the lock and writes its record.
        reader.close
        pid
      end

      #: (Integer pid) -> void
      def kill_owner(pid)
        Process.kill("KILL", pid)
        Process.wait(pid)
      rescue Errno::ESRCH, Errno::ECHILD
        nil
      end

      # A test must not race the child it spawned.
      #: (?Float timeout) { -> boolish } -> void
      def wait_for(timeout = 5.0)
        deadline = monotonic + timeout
        sleep(0.02) until yield || monotonic > deadline
        flunk("the condition never became true") unless yield
      end
    end
  end
end
