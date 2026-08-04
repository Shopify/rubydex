# frozen_string_literal: true

require "test_helper"
require "helpers/cli"
require "helpers/context"
require "json"
require "mocha/minitest"
require "rubydex/cli"

# `cli.rb` loads the command files lazily, so that `rdx --version` pulls in nothing it does not
# need. Load them here so the tests can refer to the command classes by constant regardless of the
# order the tests run in.
require "rubydex/cli/command"
Rubydex::CLI::Command.all

# Exercises the `rdx` CLI. Every command runs in this process through `rdx`, which reports the exit
# status and the stdout/stderr split that callers rely on (progress on stderr, results on stdout).
# `exe/rdx` only calls `Rubydex::CLI.start`, so nothing here shells out.
class CLITest < Minitest::Test
  include Test::Helpers::WithCLI
  include Test::Helpers::WithContext

  def test_commands_are_discovered_from_subclasses
    commands = Rubydex::CLI::Command.all

    assert_includes(commands, Rubydex::CLI::Command::Query)
    assert_includes(commands, Rubydex::CLI::Command::Console)
    assert_includes(commands, Rubydex::CLI::Command::Mcp)

    # The declared name is what the class reports, and drives its usage line.
    assert_equal("query", Rubydex::CLI::Command::Query.command_name)
    assert_equal("query <CYPHER>", Rubydex::CLI::Command::Query.usage_form)
    assert_equal("console", Rubydex::CLI::Command::Console.usage_form)
  end

  def test_commands_are_listed_alphabetically
    # Asserted against the rendered help text rather than against the command list, since the order
    # that matters is the one the reader sees. The anonymous commands the other tests declare are
    # listed here too, so the assertions compare the offsets of the real ones to each other.
    result = rdx("help")

    assert_success_status(result)

    # `assert_stdout_includes_pattern` returns the MatchData, so every entry is proven present
    # before the offsets are compared: a missing one fails on its own assertion rather than on a
    # comparison against nil. We collect the beginning offset of the first match (index 0) for each
    # command so that we can compare their order below.
    console, mcp, query, help = ["console", "mcp", "query", "help"].map do |name|
      assert_stdout_includes_pattern(result, /^  #{name}\b/).begin(0)
    end

    assert_operator(console, :<, mcp)
    assert_operator(mcp, :<, query)
    # `help` is listed last rather than in alphabetical position.
    assert_operator(query, :<, help)
  end

  def test_dispatch_uses_the_declared_name_not_the_class_or_file_name
    # An anonymous command whose declared name matches no class or file name: if dispatch derived
    # the name from either, this could not be reached.
    #
    # Anonymous commands stay visible to `Command.subclasses` for the rest of the process, so the
    # assertions here and in the other discovery tests check for inclusion rather than exact sets.
    Class.new(Rubydex::CLI::Command) do
      command "totally-unrelated"
      summary "A command that exists only for this test"

      def run
        print("dispatched with #{argv.inspect}")
      end
    end

    result = rdx("totally-unrelated", "an-argument")

    assert_stdout_equals('dispatched with ["an-argument"]', result)
  end

  def test_declaring_a_name_twice_is_rejected
    Class.new(Rubydex::CLI::Command) { command "conflicting" }

    error = assert_raises(ArgumentError) do
      Class.new(Rubydex::CLI::Command) { command "conflicting" }
    end

    assert_match(/`conflicting` is already declared/, error.message)
  end

  def test_usage_is_generated_from_the_declared_commands
    result = rdx("help")

    assert_success_status(result)

    [
      Rubydex::CLI::Command::Query,
      Rubydex::CLI::Command::Console,
      Rubydex::CLI::Command::Mcp,
    ].each do |command|
      assert_stdout_includes_pattern(result, /^  #{Regexp.escape(command.usage_form)}\s{2,}\S/)
    end

    # `help` is not a subcommand class, but must still be listed.
    assert_stdout_includes_pattern(result, /^  help\s{2,}Show this help message$/)
  end

  def test_version_is_reported_for_both_spellings
    ["--version", "version"].each do |flag|
      result = rdx(flag)

      assert_success_status(result)
      assert_stdout_includes_pattern(result, /\Av\d+\.\d+\.\d+/)
    end
  end

  def test_usage_is_printed_for_help_and_bare_invocation
    [[], ["-h"], ["--help"], ["help"]].each do |args|
      result = rdx(*args)

      assert_success_status(result)
      assert_stdout_includes(result, "Usage: rdx <command> [options]")
      assert_stdout_includes(result, "query <CYPHER>")
    end
  end

  def test_unknown_command_reports_usage_on_stderr
    result = rdx("frobnicate")

    refute_success_status(result)
    assert_stderr_includes(result, "unknown command: frobnicate")
    assert_stderr_includes(result, "Usage: rdx <command> [options]")
    # Usage for a failed invocation must not pollute stdout.
    assert_empty_stdout(result)
  end

  def test_schema_is_described_without_a_workspace
    result = rdx("query", "--schema")

    assert_success_status(result)
    assert_stdout_includes(result, "HAS_PARENT")
  end

  def test_schema_honors_the_format_option
    result = rdx("query", "--schema", "--format", "json")

    assert_success_status(result)
    parsed = JSON.parse(result.out)

    assert(parsed["relationships"].any? { |relationship| relationship["type"] == "HAS_PARENT" }, result.to_s)
  end

  def test_schema_warns_when_a_query_is_also_given
    result = rdx("query", "--schema", "MATCH (c:Class) RETURN c.name")

    assert_success_status(result)
    assert_stderr_includes(result, "ignoring query argument")
  end

  def test_query_without_an_argument_reports_usage
    result = rdx("query")

    refute_success_status(result)
    assert_stderr_includes(result, "`query` requires a Cypher query argument")
  end

  def test_malformed_query_fails_before_indexing
    result = rdx("query", "MATCH (c RETURN c")

    refute_success_status(result)
    assert_stderr_includes(result, "Cypher syntax error")
    # The failure must come from parsing, not from a graph build.
    refute_stderr_includes(result, "Indexing workspace")
    assert_empty_stdout(result)
  end

  def test_query_runs_against_the_workspace_graph
    with_context do |context|
      context.write!("zoo.rb", "class Animal; end\nclass Dog < Animal; end\n")

      result = rdx("query", "MATCH (c:Class {name: 'Dog'}) RETURN c.name", chdir: context.absolute_path)

      assert_success_status(result)
      assert_stdout_includes(result, "Dog")
      assert_stdout_includes(result, "1 row")
      # Progress is reported on stderr so stdout stays pipeable.
      assert_stderr_includes(result, "Indexing workspace")
      refute_stdout_includes(result, "Indexing workspace")
    end
  end

  def test_query_supports_json_output
    with_context do |context|
      context.write!("zoo.rb", "class Dog; end\n")

      cypher = "MATCH (c:Class {name: 'Dog'}) RETURN c.name"
      result = rdx("query", cypher, "--format", "json", chdir: context.absolute_path)

      assert_success_status(result)
      assert_equal([{ "c.name" => "Dog" }], JSON.parse(result.out), result.to_s)
    end
  end

  def test_command_help_is_available_per_subcommand
    ["query", "console", "mcp"].each do |command|
      result = rdx(command, "--help")

      assert_success_status(result)
      assert_stdout_includes(result, "Usage: rdx #{command}")
    end
  end

  def test_every_command_reports_an_invalid_option_with_the_usage
    ["query", "console", "mcp"].each do |command|
      result = rdx(command, "--bogus-flag")

      refute_success_status(result)
      assert_stderr_includes(result, "invalid option: --bogus-flag")
      assert_stderr_includes(result, "Usage: rdx <command> [options]")
      # A bad option must not produce a Ruby backtrace.
      refute_stderr_includes(result, "OptionParser::InvalidOption")
      assert_empty_stdout(result)
    end
  end

  def test_an_invalid_format_value_reports_the_usage
    result = rdx("query", "--schema", "--format", "yaml")

    refute_success_status(result)
    assert_stderr_includes(result, "invalid argument: --format yaml")
    refute_stderr_includes(result, "OptionParser::InvalidArgument")
  end

  def test_mcp_rejects_extra_arguments
    result = rdx("mcp", "one", "two")

    refute_success_status(result)
    assert_stderr_includes(result, "unexpected argument: two")
  end

  # `irb` is not a runtime dependency, so its absence is reported rather than raised. The graph is
  # stubbed out: this is about the `require`, and indexing a workspace would prove nothing here.
  def test_console_reports_a_missing_irb
    console_raising_on_require(load_error("irb"))

    result = rdx("console")

    refute_success_status(result)
    assert_stderr_includes(result, "Interactive mode requires `irb` to be in the bundle")
  end

  # IRB loads `reline`, which needs `fiddle` on Windows. Reporting that as a missing `irb` sent two
  # reviewers of this PR after the wrong cause, so the original failure has to survive.
  def test_console_surfaces_a_load_error_raised_from_inside_irb
    console_raising_on_require(load_error("fiddle/import"))

    error = assert_raises(LoadError) { rdx("console") }

    assert_equal("fiddle/import", error.path)
  end

  private

  #: (LoadError error) -> void
  def console_raising_on_require(error)
    console = Rubydex::CLI::Command::Console.any_instance
    console.stubs(:build_graph).returns(:unused)
    console.stubs(:require).with("irb").raises(error)
  end

  # `LoadError#path` names the feature that could not be loaded, and has no public writer.
  #: (String path) -> LoadError
  def load_error(path)
    error = LoadError.new("cannot load such file -- #{path}")
    error.instance_variable_set(:@path, path)
    error
  end
end
