# frozen_string_literal: true

module Rubydex
  module Server
    # Implementations for the server-control CLI flags (`--start-server`, `--stop-server`,
    # `--restart-server`, `--server-status`). Each returns a process exit status.
    module Commands
      class << self
        #: (Cache cache, ?detach: bool, ?stdout: IO) -> Integer
        def start(cache, detach: true, stdout: $stdout)
          if cache.server_alive? && cache.version_compatible? && File.socket?(cache.socket_path)
            stdout.puts("rdx server already running (pid=#{cache.server_pid}, socket=#{cache.socket_path})")
            return 0
          end

          Client.ensure_server(cache, detach: detach)
          # With --no-detach the server runs in the foreground and only returns here once stopped.
          stdout.puts("rdx server started (pid=#{cache.server_pid}, socket=#{cache.socket_path})")
          0
        end

        #: (Cache cache, ?stdout: IO) -> Integer
        def stop(cache, stdout: $stdout)
          unless cache.server_alive?
            stdout.puts("rdx server: no server running for #{cache.workspace_path}")
            cache.clean!
            return 0
          end

          Client.stop(cache)
          stdout.puts("rdx server stopped")
          0
        end

        #: (Cache cache, ?detach: bool, ?stdout: IO) -> Integer
        def restart(cache, detach: true, stdout: $stdout)
          Client.restart(cache, detach: detach)
          stdout.puts("rdx server restarted (pid=#{cache.server_pid}, socket=#{cache.socket_path})")
          0
        end

        #: (Cache cache, ?stdout: IO, ?stderr: IO) -> Integer
        def status(cache, stdout: $stdout, stderr: $stderr)
          unless cache.server_alive? && File.socket?(cache.socket_path)
            stdout.puts("rdx server: not running for #{cache.workspace_path}")
            return 0
          end

          Client.request(cache, { "command" => "status" }, stdout: stdout, stderr: stderr)
        end
      end
    end
  end
end
