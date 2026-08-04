# frozen_string_literal: true

require "rubydex/cli/command"

module Rubydex
  module CLI
    class Command
      class Server < Command
        command "server"
        arguments "<action>"
        summary "Manage the resident server (start, stop, restart, status)"

        ACTIONS = ["start", "stop", "restart", "status"].freeze #: Array[String]

        USAGE = <<~TEXT #: String
          Usage: rdx server <action> [options]

          Actions:
            start     Start the server for this workspace
            stop      Stop the running server for this workspace
            restart   Restart the server for this workspace
            status    Print the status of the server for this workspace
        TEXT

        #: -> void
        def run
          # Options are parsed before the action is shifted, so a flag is not read as the action.
          # `OptionParser#parse!` permutes, so the action may come before or after a flag.
          parse_options!(options: true, banner: USAGE)

          action = argv.shift
          abort_with_actions("unknown server action: #{action.inspect}") unless ACTIONS.include?(action)

          require "rubydex/server"

          unless Rubydex::Server.supported?
            abort("rdx server mode is not supported on this platform " \
              "(requires fork, UNIX sockets and O_NOFOLLOW)")
          end

          exit(dispatch_action(action))
        end

        private

        #: (String action) -> Integer
        def dispatch_action(action)
          state = Rubydex::Server::State.new(workspace_path: Dir.pwd)

          case action
          when "start" then Rubydex::Server::Commands.start(state)
          when "stop" then Rubydex::Server::Commands.stop(state)
          when "restart" then Rubydex::Server::Commands.restart(state)
          else Rubydex::Server::Commands.status(state)
          end
        end

        # Uses the action list, not the top-level command list, because the error is about an action
        # of this command.
        #: (String message) -> void
        def abort_with_actions(message)
          warn(message)
          warn("")
          warn(USAGE)
          exit(1)
        end
      end
    end
  end
end
