# frozen_string_literal: true

require "socket"

module Rubydex
  module Server
    # The short-lived client side. Ensures a compatible server is running for the workspace, then
    # talks to it over the UNIX socket. Kept deliberately lightweight: it never `require`s the full
    # `rubydex` extension on the happy path, so the server actually saves time.
    module Client
      class << self
        # Runs a `--query` against the server, printing its output and returning the exit status.
        #: (Cache cache, Hash[Symbol, untyped] options, ?stdout: IO, ?stderr: IO) -> Integer
        def query(cache, options, stdout: $stdout, stderr: $stderr)
          request(
            cache,
            {
              "command" => "query",
              "query" => options[:query],
              "query_format" => options[:query_format] || "table",
            },
            stdout: stdout,
            stderr: stderr,
          )
        end

        # Sends a single request and streams the framed response. Restarts the server once if the
        # initial connection fails (stale socket / incompatible version).
        #: (Cache cache, Hash[String, untyped] payload, ?stdout: IO, ?stderr: IO) -> Integer
        def request(cache, payload, stdout: $stdout, stderr: $stderr)
          socket = connection(cache)
          Request.write(socket, payload.merge(base_payload(cache)))

          response = Request.read(socket)
          socket.close

          unless response
            stderr.puts("rdx server: empty response")
            return 1
          end

          stdout.print(response["stdout"]) if response["stdout"]
          stderr.print(response["stderr"]) if response["stderr"]
          response["status"] || 0
        end

        # Ensures a server is running, then connects with the version handshake. Restarts once on a
        # failed connect.
        #: (Cache cache) -> UNIXSocket
        def connection(cache)
          ensure_server(cache, detach: true)

          socket = connect(cache)
          unless socket
            restart(cache, detach: true)
            socket = connect(cache)
          end

          raise Error, "could not connect to the rdx server at #{cache.socket_path}" unless socket

          socket
        end

        # Starts a server if none is running, or restarts it if the running one is incompatible.
        #: (Cache cache, ?detach: bool) -> void
        def ensure_server(cache, detach: true)
          if cache.server_alive? && File.socket?(cache.socket_path)
            return if cache.version_compatible?

            stop(cache)
          end

          cache.clean!
          start(cache, detach: detach)
        end

        # Forks + daemonizes a server process (unless `detach: false`) and waits until it is ready.
        #: (Cache cache, ?detach: bool) -> void
        def start(cache, detach: true)
          cache.ensure_dir!

          if detach
            pid = fork { run_server(cache, detach: true) }
            Process.detach(pid) if pid
            wait_until_ready(cache)
          else
            run_server(cache, detach: false)
          end
        end

        #: (Cache cache, ?detach: bool) -> void
        def restart(cache, detach: true)
          stop(cache)
          cache.clean!
          start(cache, detach: detach)
        end

        # Stops the running server (gracefully if possible, otherwise via SIGTERM) and cleans up.
        #
        # The pid is captured up front and waited on directly, because the server removes its own pid
        # file during shutdown while it still holds the start flock. Re-reading the pid file would
        # make the process look dead too early, letting a restart spawn a new daemon that then fails
        # to grab the still-held flock.
        #: (Cache cache) -> void
        def stop(cache)
          pid = cache.server_pid

          if pid && process_alive?(pid)
            graceful_stop(cache, pid)

            if process_alive?(pid)
              terminate(pid)
              wait_for_exit(pid)
            end
          end

          cache.clean!
        end

        private

        # Runs the server in the current process. Guards against concurrent starts with a flock: if
        # another process already holds the lock, this returns without starting a second server.
        #: (Cache cache, detach: bool) -> void
        def run_server(cache, detach:)
          lock = File.open(cache.lock_path, File::RDWR | File::CREAT, 0o600)

          unless lock.flock(File::LOCK_EX | File::LOCK_NB)
            lock.close
            return
          end

          Process.daemon(true) if detach
          Core.new(cache, lock: lock, detach: detach).run
        end

        #: (Cache cache) -> UNIXSocket?
        def connect(cache)
          # Mirror the server's relative bind so a long runtime directory doesn't blow the
          # `sockaddr_un` path limit. The cwd is restored when the block returns.
          socket = Dir.chdir(File.dirname(cache.socket_path)) do
            UNIXSocket.new(File.basename(cache.socket_path))
          end
          version = Request.read_line_with_timeout(socket, HANDSHAKE_TIMEOUT).chomp

          if version == cache.expected_version
            socket
          else
            socket.close
            nil
          end
        rescue Errno::ENOENT, Errno::ECONNREFUSED, Errno::ECONNRESET, ServerReadTimeout
          begin
            socket&.close
          rescue IOError
            nil
          end
          nil
        end

        #: (Cache cache, Integer pid) -> void
        def graceful_stop(cache, pid)
          socket = connect(cache)
          return unless socket

          Request.write(socket, base_payload(cache).merge("command" => "stop"))
          Request.read(socket)
          socket.close
          wait_for_exit(pid)
        rescue Errno::EPIPE, Errno::ECONNRESET
          nil
        end

        #: (Integer pid) -> void
        def terminate(pid)
          Process.kill("TERM", pid)
        rescue Errno::ESRCH
          nil
        end

        #: (Integer pid) -> bool
        def process_alive?(pid)
          Process.kill(0, pid)
          true
        rescue Errno::ESRCH, Errno::EPERM
          false
        end

        #: (Cache cache) -> void
        def wait_until_ready(cache)
          deadline = monotonic + BOOT_TIMEOUT

          until File.socket?(cache.socket_path) && cache.version_compatible?
            raise Error, "timed out waiting for the rdx server to start" if monotonic > deadline

            sleep(0.05)
          end
        end

        #: (Integer pid) -> bool
        def wait_for_exit(pid)
          deadline = monotonic + 5.0

          while process_alive?(pid)
            return false if monotonic > deadline

            sleep(0.02)
          end

          true
        end

        #: (Cache cache) -> Hash[String, untyped]
        def base_payload(cache)
          {
            "protocol" => PROTOCOL,
            "token" => cache.token,
            "cwd" => Dir.pwd,
            "argv" => ARGV,
            "env" => {},
          }
        end

        #: -> Float
        def monotonic
          Process.clock_gettime(Process::CLOCK_MONOTONIC)
        end
      end
    end
  end
end
