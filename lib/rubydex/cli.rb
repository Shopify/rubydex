# frozen_string_literal: true

module Rubydex
  # Command-line entry point for the `rdx` executable. It handles the top-level flags, resolves the
  # subcommand and delegates to it, which keeps `exe/rdx` a thin shim.
  #
  # Subcommands live one per file in `lib/rubydex/cli/command/` and declare the name they answer to
  # (see {CLI::Command}). Both dispatch and the `rdx help` command list are driven by that
  # declaration, so adding a subcommand means adding a file — there is no dispatch table or usage
  # text to keep in step.
  module CLI
    HEADER = "Usage: rdx <command> [options]" #: String
    FOOTER = "Run `rdx <command> --help` for command-specific options." #: String

    # `help` is served by the top-level flags rather than by a Command subclass, so its entry is
    # listed here to keep it in the generated command list.
    HELP_ENTRY = ["help", "Show this help message"].freeze #: [String, String]

    class << self
      # Entry point used by `exe/rdx`.
      #: (?Array[String] argv) -> void
      def start(argv = ARGV)
        return if handle_top_level_flags(argv)

        # Everything past this point needs the native extension.
        require "rubydex"

        dispatch(argv.shift, argv)
      rescue StandardError => e
        # A command loads `rubydex/server` only when it needs it, so the constant can be absent.
        # A direct reference here would raise `NameError` and hide the real error.
        raise unless defined?(Rubydex::Server::Error) && e.is_a?(Rubydex::Server::Error)

        # A server that does not start or answer is a runtime condition, not a defect in rdx.
        warn("rdx server: #{e.message}")
        exit(1)
      end

      # Reports `message`, then the usage text, and exits non-zero. Public because the subcommands
      # report their own argument errors through it.
      #: (String message) -> void
      def abort_with_usage(message)
        warn(message)
        warn("")
        warn(usage)
        exit(1)
      end

      private

      # The top-level usage text, built from the declared commands.
      #: -> String
      def usage
        require "rubydex/cli/command"

        entries = Command.all.map { |command| [command.usage_form, command.summary.to_s] }
        entries << HELP_ENTRY.dup

        width = entries.map { |form, _| form.length }.max + 3
        lines = entries.flat_map { |form, summary| describe(form, summary, width) }

        "#{[HEADER, "", "Commands:", *lines, "", FOOTER].join("\n")}\n"
      end

      # Renders one entry of the command list, aligning any continuation lines of `summary` under
      # the first.
      #: (String form, String summary, Integer width) -> Array[String]
      def describe(form, summary, width)
        first, *rest = summary.split("\n")

        ["  #{form.ljust(width)}#{first}", *rest.map { |line| "#{" " * (width + 2)}#{line}" }]
      end

      # Handles `--version` / `--help` / a bare invocation, returning whether the invocation was
      # handled so `start` returns exactly once rather than exiting from inside every branch.
      #: (Array[String] argv) -> bool
      def handle_top_level_flags(argv)
        case argv.first
        when "--version", "version"
          require "rubydex/version"
          puts("v#{Rubydex::VERSION}")
          true
        when nil, "-h", "--help", "help"
          puts(usage)
          true
        else
          false
        end
      end

      #: (String? name, Array[String] argv) -> void
      def dispatch(name, argv)
        require "rubydex/cli/command"

        command_class = Command.find(name)
        abort_with_usage("unknown command: #{name}") unless command_class

        command_class.new(argv).run
      end
    end
  end
end
