# frozen_string_literal: true

require "irb"

module Rubydex
  # Interactive console backing `rdx console`.
  #
  # On top of a normal IRB session (with `graph` in scope so you can call Ruby directly, e.g.
  # `graph["Foo"]`), it registers commands that run Cypher against the same graph without needing
  # valid Ruby syntax or quoting:
  #
  #   rubydex(main):001> query MATCH (n:Class|Module) RETURN n.name ORDER BY n.name
  #   rubydex(main):002> schema
  #
  # The `query` command takes the rest of the line verbatim, so the Cypher does not have to parse as
  # Ruby. This is the clean, idiomatic realization of a "query mode": instead of swapping the REPL's
  # evaluator, Cypher lines are simply prefixed with `query`.
  #
  # The Cypher commands rely on IRB's command-registration API (IRB >= 1.13). On older IRB the
  # console still works as a plain Ruby session; only the `query`/`schema` shortcuts are unavailable.
  module Console
    class << self
      # The graph that the `query`/`schema` console commands operate on. Set by {.start}.
      attr_accessor :graph

      # Starts an interactive session. `graph` is exposed both as a local variable (for the Ruby
      # side of the console) and to the Cypher commands via {.graph}.
      def start(graph)
        self.graph = graph

        IRB.setup(nil)
        IRB.conf[:IRB_NAME] = "rubydex"
        IRB::Irb.new(IRB::WorkSpace.new(session_binding(graph))).run(IRB.conf)
      end

      # Runs a Cypher query against {.graph} and returns the formatted output, or a human-readable
      # error message on failure. Extracted from the command so it can be exercised without IRB.
      def run_query(cypher, format: :table)
        cypher = cypher.to_s.strip
        return "Usage: query <CYPHER>" if cypher.empty?

        Rubydex::Query.parse(cypher).render(graph, format)
      rescue ArgumentError => e
        e.message
      end

      # Returns the queryable Cypher schema description.
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
    # `query <CYPHER>` — runs a Cypher query against the console graph and prints the result.
    class QueryCommand < IRB::Command::Base
      category "Rubydex"
      description "Run a Cypher query against the graph: query <CYPHER>"

      def execute(arg)
        puts(Console.run_query(arg))
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
