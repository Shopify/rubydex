# frozen_string_literal: true

module Rubydex
  module Server
    module Commands
      class << self
        #: (State state, ?stdout: IO) -> Integer
        def start(state, stdout: $stdout)
          if state.server_running? && state.version_compatible? && File.socket?(state.socket_path)
            stdout.puts("rdx server already running")
            return 0
          end

          Client.ensure_server(state)
          stdout.puts("rdx server started")
          0
        end

        #: (State state, ?stdout: IO) -> Integer
        def stop(state, stdout: $stdout)
          unless state.server_running?
            stdout.puts("rdx server: no server running for #{state.workspace_path}")
            state.clean!
            return 0
          end

          if Client.stop(state)
            stdout.puts("rdx server stopped")
            return 0
          end

          stdout.puts(unresponsive_report(state, "it did not stop within #{Client::STOP_TIMEOUT}s"))
          1
        end

        #: (State state, ?stdout: IO) -> Integer
        def restart(state, stdout: $stdout)
          unless Client.restart(state)
            stdout.puts(unresponsive_report(state, "it did not stop within #{Client::STOP_TIMEOUT}s"))
            return 1
          end

          stdout.puts("rdx server restarted")
          0
        end

        # `timeout` exists so a test can exercise the unanswered path without the real wait.
        #: (State state, ?stdout: IO, ?stderr: IO, ?timeout: Float) -> Integer
        def status(state, stdout: $stdout, stderr: $stderr, timeout: Frame::REQUEST_TIMEOUT)
          unless state.server_running?
            stdout.puts("rdx server: not running for #{state.workspace_path}")
            return 0
          end

          unless File.socket?(state.socket_path)
            stdout.puts(unresponsive_report(state, "it has not created its socket"))
            return 1
          end

          # `probe` must not change the server or spend a fresh timeout on the handshake and the
          # answer, because it diagnoses a server that will not answer.
          status = Client.probe(
            state,
            { "command" => "status" },
            stdout: stdout,
            stderr: stderr,
            timeout: timeout,
          )
          return status if status

          stdout.puts(unresponsive_report(state, "it did not answer within #{timeout}s"))
          1
        end

        private

        # The report states an observation, not a diagnosis: the server answers one request at a
        # time, so a long query looks the same as a stuck process.
        #: (State state, String reason) -> String
        def unresponsive_report(state, reason)
          <<~REPORT
            rdx server: a server holds #{state.workspace_path}, but #{reason}
              recorded pid:   #{state.server_pid || "unknown"}
              recorded start: #{state.started_at || "unknown"}
              socket:         #{state.socket_path}
              log:            #{state.log_path}
            The server answers one request at a time, so a long query looks the same from here. The
            log records the boot and any failure, but not the request in flight, so it cannot tell
            you which of the two this is. rdx never signals a recorded pid: check the log for errors,
            and stop that process yourself only if it stays silent for longer than the work should
            take.
          REPORT
        end
      end
    end
  end
end
