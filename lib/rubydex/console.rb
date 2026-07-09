# frozen_string_literal: true

require "irb"

module Rubydex
  # Interactive console backing `rdx console`.
  #
  # On top of a normal IRB session (with `graph` in scope so you can call Ruby directly, e.g.
  # `graph["Foo"]`), it offers two complementary ways to run Cypher against the same graph:
  #
  #   # `run("...")` — a method that returns rich Ruby objects for programmatic use: an Array of
  #   # Hashes keyed by RETURN column, where node columns are live Declaration/Definition/Document
  #   # handles. Because it's a plain method, the result can be assigned and navigated.
  #   rubydex(main):001> result = run("MATCH (c:Class)-[:HAS_PARENT]->(p) RETURN c, p")
  #   rubydex(main):002> result.first["c"].name
  #
  #   # `query <CYPHER>` — a command that takes the rest of the line verbatim (no quotes / valid
  #   # Ruby needed) and prints a formatted table for a quick look.
  #   rubydex(main):003> query MATCH (n:Class|Module) RETURN n.name ORDER BY n.name
  #
  #   # `schema` — prints the queryable schema.
  #   rubydex(main):004> schema
  #
  # So `run` is the programmatic entry point and `query` is the interactive quick-look; they are
  # deliberately separate rather than one delegating to the other. This is the clean, idiomatic
  # "query mode": rather than swapping the REPL's evaluator, Cypher lines are simply prefixed with
  # `query`.
  #
  # The `query`/`schema` commands rely on IRB's command-registration API (IRB >= 1.13). On older IRB
  # the console still works as a plain Ruby session (including `run`); only the command shortcuts are
  # unavailable.
  module Console
    class << self
      # The graph that `run` and the `query`/`schema` commands operate on. Set by {.start}.
      attr_accessor :graph

      # Starts an interactive session. `graph` is exposed as a local variable (for the Ruby side of
      # the console), and `run`/`query`/`schema` operate on it via {.graph}.
      def start(graph)
        self.graph = graph

        IRB.setup(nil)
        IRB.conf[:IRB_NAME] = "rubydex"
        IRB::Irb.new(IRB::WorkSpace.new(session_binding(graph))).run(IRB.conf)
      end

      # Runs a Cypher query against {.graph} and returns the rows as rich Ruby objects: an Array of
      # Hashes keyed by RETURN column name. Scalars become String/Integer/true/false/nil, lists
      # become Arrays, map projections become Hashes, and node columns become live
      # `Declaration`/`Definition`/`Document` handles. Raises `ArgumentError` on a syntax or
      # execution error. Callable in the console as `run("MATCH ...")`.
      def run(cypher)
        Rubydex::Query.parse(cypher.to_s).run(graph)
      end

      # Runs a Cypher query against {.graph} and returns the formatted output as a String (`:table`
      # or `:json`) — the quick-look counterpart of {.run}, used by the `query` command. Raises
      # `ArgumentError` on a syntax or execution error.
      def render(cypher, format: :table)
        Rubydex::Query.parse(cypher.to_s).render(graph, format)
      end

      # Returns the queryable Cypher schema description as a formatted String.
      def describe_schema(format: :table)
        Rubydex::Query.schema(format)
      end

      private

      # A binding that exposes `graph` as a local variable for the Ruby side of the console.
      def session_binding(graph)
        binding
      end
    end
  end
end

# Register the Cypher commands only when the installed IRB exposes the command API (>= 1.13).
# Otherwise stop loading here: the console still works as a plain Ruby session, just without the
# `query`/`schema` shortcuts.
begin
  require "irb/command"
  return unless defined?(IRB::Command::Base) && IRB::Command.respond_to?(:register)
rescue LoadError
  return
end

module Rubydex
  module Console
    # `query <CYPHER>` — runs a Cypher query against the console graph and prints a formatted table
    # for a quick look. For a result you can assign and navigate, use `run("...")` (see {Console.run}).
    class QueryCommand < IRB::Command::Base
      category "Rubydex"
      description "Run a Cypher query against the graph: query <CYPHER>"

      def execute(arg)
        cypher = arg.to_s.strip
        if cypher.empty?
          warn("Usage: query <CYPHER>")
          return
        end

        puts(Console.render(cypher))
        nil
      rescue ArgumentError => e
        warn(e.message)
        nil
      end
    end

    # `schema` — prints the queryable Cypher schema (labels, relationships, properties).
    class SchemaCommand < IRB::Command::Base
      category "Rubydex"
      description "Describe the queryable Cypher schema"

      def execute(_arg)
        puts(Console.describe_schema)
        nil
      end
    end
  end
end

IRB::Command.register(:query, Rubydex::Console::QueryCommand)
IRB::Command.register(:schema, Rubydex::Console::SchemaCommand)
