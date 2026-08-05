# frozen_string_literal: true

require "socket"

module Rubydex
  module Server
    # The short-lived client side. It requires no native extension when a server answers, which
    # is the source of the time saving.
    module Client
      HANDSHAKE_TIMEOUT = 10.0

      # A new server indexes the whole workspace first, so this limit is far larger.
      BOOT_TIMEOUT = 120.0

      STOP_TIMEOUT = 5.0

      class << self
        #: (State state, Hash[Symbol, untyped] options, ?stdout: IO, ?stderr: IO) -> Integer
        def query(state, options, stdout: $stdout, stderr: $stderr)
          request(
            state,
            {
              "command" => "query",
              "query" => options[:query],
              "query_format" => options[:query_format] || "table",
            },
            stdout: stdout,
            stderr: stderr,
          )
        end

        # The server answers only when the query completes, so this read has no overall deadline.
        #: (State state, Hash[String, untyped] payload, ?stdout: IO, ?stderr: IO) -> Integer
        def request(state, payload, stdout: $stdout, stderr: $stderr)
          socket = connection(state)

          begin
            exchange(state, socket, payload, stdout: stdout, stderr: stderr)
          rescue Frame::ReadTimeout, Frame::Malformed => e
            # The server failed, so the client reports the failure and does not raise.
            stderr.puts("rdx server: #{e.message}")
            1
          end
        end

        # Talks to a server that already exists, and changes nothing. `status` uses this to diagnose
        # a server that does not answer, so one budget covers every step. Returns `nil` on no answer.
        #: (State state, Hash[String, untyped] payload, ?stdout: IO, ?stderr: IO, ?timeout: Float) -> Integer?
        def probe(state, payload, stdout: $stdout, stderr: $stderr, timeout: Frame::REQUEST_TIMEOUT)
          deadline = Process.clock_gettime(Process::CLOCK_MONOTONIC) + timeout

          socket = connect(state, timeout: remaining(deadline))
          return unless socket

          exchange(state, socket, payload, stdout: stdout, stderr: stderr, total_timeout: remaining(deadline))
        rescue Frame::ReadTimeout, Frame::Malformed, Errno::EPIPE, Errno::ECONNRESET
          nil
        end

        #: (State state) -> UNIXSocket
        def connection(state)
          ensure_server(state)

          socket = connect(state)
          return socket if socket

          # A restart repairs a stale socket, but not a live owner that does not answer: a new
          # server cannot take the lock from it.
          unless state.server_running?
            restart(state)
            socket = connect(state)
            return socket if socket
          end

          if state.server_running?
            raise Error, "the rdx server for #{state.workspace_path} is not answering; run `rdx server status`"
          end

          raise Error, "could not connect to the rdx server at #{state.socket_path}"
        end

        #: (State state) -> void
        def ensure_server(state)
          if state.server_running? && File.socket?(state.socket_path)
            return if state.version_compatible?

            stop(state)
          end

          state.clean!
          start(state)
        end

        # A held lock means another process has already started a server here, so this waits for it.
        #
        # Take the lock before `fork`. `server_running?` acquires the lock to test it, so the
        # parent's readiness check can otherwise acquire it before the child does.
        #: (State state) -> void
        def start(state)
          previous = state.read

          return wait_until_ready(state, previous: previous) if state.server_running?

          state.ensure_dir!
          lock = state.open_lock

          # Another process acquired the lock after the check above.
          unless lock.flock(File::LOCK_EX | File::LOCK_NB)
            lock.close
            return wait_until_ready(state, previous: previous)
          end

          begin
            pid = fork { run_server(state, lock) }
          rescue StandardError
            lock.flock(File::LOCK_UN)
            lock.close
            raise
          end

          Process.detach(pid) if pid
          # The child inherits the open file description, so the lock outlives this handle.
          lock.close
          wait_until_ready(state, previous: previous)
        end

        # Returns false when the old server keeps its lock, because a new one cannot start then.
        #: (State state) -> bool
        def restart(state)
          return false unless stop(state)

          state.clean!
          start(state)
          true
        end

        # An authenticated request is the only stop. `Process.daemon` forks a second time, so a
        # recorded pid can belong to an unrelated process, and no caller may signal it.
        #: (State state) -> bool
        def stop(state)
          return true unless state.server_running?

          socket = connect(state)

          if socket
            begin
              exchange(
                state,
                socket,
                { "command" => "stop" },
                stdout: $stdout,
                stderr: $stderr,
                total_timeout: Frame::REQUEST_TIMEOUT,
              )
            rescue Errno::EPIPE, Errno::ECONNRESET, Frame::ReadTimeout, Frame::Malformed
              # A server that dies during its answer has stopped. The lock proves that, below.
              nil
            end
          end

          stopped = wait_until_stopped(state)
          state.clean! if stopped
          stopped
        end

        private

        # One exchange with a server that already answers. The three callers differ only in how they
        # reached it, and in what a failure means, so every failure raises and each caller decides.
        #: (State state, UNIXSocket socket, Hash[String, untyped] payload, stdout: IO, stderr: IO, ?total_timeout: Float?) -> Integer
        def exchange(state, socket, payload, stdout:, stderr:, total_timeout: nil)
          Frame.write(socket, payload.merge(base_payload(state)))

          response = Frame.read_response(socket, total_timeout: total_timeout)
          raise Frame::Malformed, "the server closed the connection without answering" unless response

          stdout.print(response["stdout"]) if response["stdout"]
          stderr.print(response["stderr"]) if response["stderr"]

          status = response["status"]
          raise Frame::Malformed, "the response carried no status" unless status.is_a?(Integer)

          status
        ensure
          socket.close unless socket.closed?
        end

        # The daemon inherits the held lock across both forks.
        #: (State state, File lock) -> void
        def run_server(state, lock)
          Process.daemon(true)
          Core.new(state, lock: lock).run
        end

        #: (State state, ?timeout: Float) -> UNIXSocket?
        def connect(state, timeout: HANDSHAKE_TIMEOUT)
          # A relative socket name keeps the path below the `sockaddr_un` limit.
          socket = Dir.chdir(File.dirname(state.socket_path)) do
            UNIXSocket.new(File.basename(state.socket_path))
          end
          version = Frame.read_line(socket, timeout: timeout).chomp

          if version == state.expected_version
            socket
          else
            socket.close
            nil
          end
        rescue Errno::ENOENT,
               Errno::ECONNREFUSED,
               Errno::ECONNRESET,
               Frame::ReadTimeout,
               Frame::Malformed
          begin
            socket&.close
          rescue IOError
            nil
          end
          nil
        end

        # `previous` is the record from before the spawn. A held lock, or a changed record, proves
        # that a boot began. A free lock after that proves the server died, so this stops early.
        #: (State state, previous: Hash[String, untyped]?) -> void
        def wait_until_ready(state, previous:)
          deadline = Process.clock_gettime(Process::CLOCK_MONOTONIC) + BOOT_TIMEOUT
          seen_lock = false

          loop do
            running = state.server_running?
            seen_lock ||= running

            # The lock is part of the test: a crashed server leaves a socket and a valid record.
            break if running && File.socket?(state.socket_path) && state.version_compatible?

            if (seen_lock || state.read != previous) && !running
              raise Error, "the rdx server exited during startup; see #{server_log_hint(state)} for details"
            end

            raise Error, "timed out waiting for the rdx server to start (see #{server_log_hint(state)})" if Process.clock_gettime(Process::CLOCK_MONOTONIC) > deadline

            sleep(0.05)
          end
        end

        # The kernel frees the lock when the process exits, however it exited.
        #: (State state) -> bool
        def wait_until_stopped(state)
          deadline = Process.clock_gettime(Process::CLOCK_MONOTONIC) + STOP_TIMEOUT

          while state.server_running?
            return false if Process.clock_gettime(Process::CLOCK_MONOTONIC) > deadline

            sleep(0.02)
          end

          true
        end

        # Floored, so a read never gets a zero or negative timeout.
        #: (Float deadline) -> Float
        def remaining(deadline)
          left = deadline - Process.clock_gettime(Process::CLOCK_MONOTONIC)
          [left, 0.05].max
        end

        # Where the detached server writes its output, so a user can read a boot failure.
        #: (State state) -> String
        def server_log_hint(state)
          override = ENV["RDX_SERVER_LOG"]
          override && !override.empty? ? override : state.log_path
        end

        #: (State state) -> Hash[String, untyped]
        def base_payload(state)
          {
            "protocol" => PROTOCOL,
            "token" => state.token,
            "cwd" => Dir.pwd,
            "argv" => ARGV,
            "env" => {},
          }
        end
      end
    end
  end
end
