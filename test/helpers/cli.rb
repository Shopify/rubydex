# frozen_string_literal: true

require "rubydex/cli"

module Test
  module Helpers
    # What one command wrote, and the status it exited with.
    class CLIResult
      #: String
      attr_reader :out

      #: String
      attr_reader :err

      #: Integer
      attr_reader :status

      #: (String command, String out, String err, Integer status) -> void
      def initialize(command, out, err, status)
        @command = command
        @out = out
        @err = err
        @status = status
      end

      #: -> bool
      def success?
        @status.zero?
      end

      # Reported by every assertion below, so a failure shows the whole invocation instead of the
      # one stream the assertion happened to look at. Both streams are reproduced line for line, so
      # what a reader sees here is what the command wrote.
      #: -> String
      def to_s
        [
          "$ #{@command}\n",
          banner("STDOUT"),
          body(@out),
          banner("STDERR"),
          body(@err),
          banner("STATUS: #{@status}"),
        ].join
      end

      private

      RULE = "##########" #: String

      #: (String title) -> String
      def banner(title)
        "#{RULE} #{title} #{RULE}\n"
      end

      # Keeps every line of a stream, and starts the next banner on a line of its own.
      #: (String text) -> String
      def body(text)
        return "<empty>\n" if text.empty?

        text.end_with?("\n") ? text : "#{text}\n"
      end
    end

    # Drives the `rdx` CLI in the test process, and asserts on what it wrote.
    module WithCLI
      # Runs the CLI with `args` and captures both streams.
      #
      # `exit` and `abort` raise `SystemExit`, which is caught here so a command that aborts reports
      # its status instead of taking the test process down. A command that returns normally
      # reports 0.
      #
      # `chdir` sets the working directory of the invocation. The commands resolve the workspace
      # they index from `Dir.pwd`, so a test that needs a workspace of its own passes one.
      #: (*String args, ?chdir: String?) -> CLIResult
      def rdx(*args, chdir: nil)
        command = ["rdx", *args].join(" ")
        status = 0

        out, err = capture_io do
          in_directory(chdir) do
            Rubydex::CLI.start(args)
          rescue SystemExit => e
            status = e.status
          end
        end

        CLIResult.new(command, out, err, status)
      end

      #: (CLIResult result) -> void
      def assert_success_status(result)
        assert(result.success?, "Expected `rdx` to exit with 0.\n#{result}")
      end

      #: (CLIResult result) -> void
      def refute_success_status(result)
        refute(result.success?, "Expected `rdx` to exit with a non-zero status.\n#{result}")
      end

      #: (CLIResult result) -> void
      def assert_empty_stdout(result)
        assert_empty(result.out, result.to_s)
      end

      #: (String text, CLIResult result) -> void
      def assert_stdout_equals(text, result)
        assert_equal(text, result.out, result.to_s)
      end

      #: (CLIResult result, String snippet) -> void
      def assert_stdout_includes(result, snippet)
        assert_includes(result.out, snippet, result.to_s)
      end

      #: (CLIResult result, String snippet) -> void
      def refute_stdout_includes(result, snippet)
        refute_includes(result.out, snippet, result.to_s)
      end

      #: (CLIResult result, String snippet) -> void
      def assert_stderr_includes(result, snippet)
        assert_includes(result.err, snippet, result.to_s)
      end

      #: (CLIResult result, String snippet) -> void
      def refute_stderr_includes(result, snippet)
        refute_includes(result.err, snippet, result.to_s)
      end

      # Returns the `MatchData`, so a caller can assert on where the match landed.
      #: (CLIResult result, Regexp pattern) -> MatchData
      def assert_stdout_includes_pattern(result, pattern)
        assert_match(pattern, result.out, result.to_s)
      end

      private

      #: (String? path) { -> void } -> void
      def in_directory(path, &block)
        path ? Dir.chdir(path, &block) : block.call
      end
    end
  end
end
