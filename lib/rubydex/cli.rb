# frozen_string_literal: true

require "optparse"

module Rubydex
  # Command-line entry point for the `rdx` executable. Parses the top-level command, dispatches to
  # the matching subcommand handler and keeps `exe/rdx` itself a thin shim.
  #
  # Heavyweight requires (`rubydex`, `rubydex/server`) are deferred into the handlers so that paths
  # which don't need the native extension (e.g. `--help`, or talking to a resident server) stay
  # cheap to start.
  class CLI
    USAGE = <<~TEXT
      Usage: rdx <command> [options]

      Commands:
        query <CYPHER>   Run a Cypher query against the workspace graph and print the result.
                         Use `query --schema` to describe the queryable schema without indexing.
        console          Open an interactive session with a populated graph for the current workspace
        server <action>  Manage the resident server (start, stop, restart, status)
        mcp [PATH]       Run the MCP server for AI assistants (workspace defaults to the current dir)
        help             Show this help message

      Run `rdx <command> --help` for command-specific options.
    TEXT

    SERVER_USAGE = <<~TEXT
      Usage: rdx server <action> [options]

      Actions:
        start     Start the server for this workspace
        stop      Stop the running server for this workspace
        restart   Restart the server for this workspace
        status    Print the status of the server for this workspace
    TEXT

    class << self
      # Convenience entry point used by `exe/rdx`.
      #: (?Array[String] argv) -> void
      def start(argv = ARGV)
        new(argv).run
      end
    end

    #: (Array[String] argv) -> void
    def initialize(argv = ARGV)
      @argv = argv
    end

    #: -> void
    def run
      # Top-level --version / --help / bare invocation, handled before command dispatch. Each branch
      # reports whether it handled the invocation so we return exactly once here rather than from
      # inside every branch.
      handled =
        case @argv.first
        when "--version", "version"
          require "rubydex/version"
          puts "v#{Rubydex::VERSION}"
          true
        when nil, "-h", "--help", "help"
          puts USAGE
          true
        else
          false
        end

      return if handled

      dispatch(@argv.shift)
    end

    private

    #: (String? command) -> void
    def dispatch(command)
      case command
      when "query" then run_query
      when "console" then run_console
      when "server" then run_server
      when "mcp" then run_mcp
      else abort_with_usage("unknown command: #{command}")
      end
    end

    #: -> void
    def run_query
      options = parse_query_options

      query = @argv.shift

      if options[:schema]
        warn("warning: ignoring query argument because `--schema` was given") if query
        require "rubydex"
        # The schema is static, so describe it without building a graph.
        print(Rubydex::Query.schema(options[:format]))
        return
      end

      abort_with_usage("`query` requires a Cypher query argument (or pass `--schema`)") if query.nil? || query.empty?

      require "rubydex/server"

      # Server mode is opt-in via `--server` and falls back to inline execution when it's disabled or
      # unsupported on this platform.
      use_server = options[:server] && !Rubydex::Server.disabled? && Rubydex::Server.supported?

      if use_server
        # The resident server parses and runs the query, so the client stays lightweight (no native
        # extension load) and only forwards the query string.
        cache = Rubydex::Server::Cache.new(workspace_path: Dir.pwd)
        exit(Rubydex::Server::Client.query(cache, { query: query, query_format: options[:format] }))
      else
        require "rubydex"
        # Parse the query up front so a malformed query fails fast, before the expensive indexing.
        parsed = begin
          Rubydex::Query.parse(query)
        rescue ArgumentError => e
          abort(e.message)
        end

        # Progress goes to stderr so stdout carries only the query result (e.g. for piping JSON).
        graph = build_graph($stderr)
        begin
          print(parsed.render(graph, options[:format]))
        rescue ArgumentError => e
          abort(e.message)
        end
      end
    end

    #: -> void
    def run_console
      OptionParser.new do |parser|
        parser.banner = "Usage: rdx console"
        parser.on("-h", "--help", "Show this help") do
          puts parser
          exit
        end
      end.parse!(@argv)

      require "rubydex"
      graph = build_graph($stdout)

      begin
        require "irb"
        IRB.setup(nil)
        IRB.conf[:IRB_NAME] = "rubydex"
        workspace = IRB::WorkSpace.new(binding)
        IRB::Irb.new(workspace).run(IRB.conf)
      rescue LoadError
        abort("Interactive mode requires `irb` to be in the bundle")
      end
    end

    #: -> void
    def run_mcp
      parser = OptionParser.new do |p|
        p.banner = "Usage: rdx mcp [PATH]"
        p.on("-h", "--help", "Show this help") do
          puts p
          exit
        end
      end
      begin
        parser.parse!(@argv)
      rescue OptionParser::ParseError => e
        abort_with_usage(e.message)
      end

      path = @argv.shift || Dir.pwd
      abort_with_usage("unexpected argument: #{@argv.first}") unless @argv.empty?

      # The MCP server manages its own graph lifecycle for the given workspace.
      require "rubydex/mcp_server"
      Rubydex::MCPServer.run(path)
    end

    #: -> void
    def run_server
      action = @argv.shift
      detach = true
      OptionParser.new do |parser|
        parser.banner = SERVER_USAGE
        parser.on("--no-detach", "Run the server in the foreground (for debugging / containers)") { detach = false }
        parser.on("-h", "--help", "Show this help") do
          puts parser
          exit
        end
      end.parse!(@argv)

      require "rubydex/server"

      unless Rubydex::Server.supported?
        abort("rdx server mode is not supported on this platform (requires fork + UNIX sockets)")
      end

      cache = Rubydex::Server::Cache.new(workspace_path: Dir.pwd)
      status = case action
      when "start" then Rubydex::Server::Commands.start(cache, detach: detach)
      when "stop" then Rubydex::Server::Commands.stop(cache)
      when "restart" then Rubydex::Server::Commands.restart(cache, detach: detach)
      when "status" then Rubydex::Server::Commands.status(cache)
      else abort_with_usage("unknown server action: #{action.inspect}", SERVER_USAGE)
      end

      exit(status)
    end

    # Parses options for the `query` command: schema mode, output format, and whether to use the
    # resident server.
    #: -> Hash[Symbol, untyped]
    def parse_query_options
      options = { format: "table", schema: false, server: false }
      OptionParser.new do |parser|
        parser.banner = "Usage: rdx query <CYPHER> [options]"
        parser.on("--schema", "Describe the queryable schema instead of running a query") { options[:schema] = true }
        parser.on("--format FORMAT", ["table", "json"], "Output format (table or json)") { |value| options[:format] = value }
        parser.on("--[no-]server", "Use the resident server (opt-in; off by default)") { |value| options[:server] = value }
        parser.on("-h", "--help", "Show this help") do
          puts parser
          exit
        end
      end.parse!(@argv)
      options
    end

    # Builds the workspace graph, sending progress messages to `progress_io`.
    #: (IO progress_io) -> Rubydex::Graph
    def build_graph(progress_io)
      graph = Rubydex::Graph.new
      graph.load_config
      with_timer(progress_io, "Indexing workspace...") { graph.index_workspace }
      with_timer(progress_io, "Resolving graph...") { graph.resolve }
      graph
    end

    #: (IO io, String message) { -> void } -> void
    def with_timer(io, message)
      io.print(message)
      start = Process.clock_gettime(Process::CLOCK_MONOTONIC, :float_millisecond)
      yield
      duration = Process.clock_gettime(Process::CLOCK_MONOTONIC, :float_millisecond) - start
      io.puts(" finished in #{duration.round(2)}ms")
    end

    #: (String message, ?String usage) -> void
    def abort_with_usage(message, usage = USAGE)
      warn(message)
      warn("")
      warn(usage)
      exit(1)
    end
  end
end
