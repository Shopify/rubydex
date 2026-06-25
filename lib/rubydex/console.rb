# frozen_string_literal: true

# The console is built on IRB's command and helper-method registration APIs, both introduced in IRB
# 1.13. Requiring an older or absent IRB raises `LoadError` (`Gem::LoadError` is one), which
# `exe/rdx` turns into a friendly message rather than a backtrace.
gem "irb", ">= 1.13"
require "irb"
require "irb/command"
require "irb/helper_method"

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
  # The `run` helper and the `query`/`schema` commands are registered through IRB's own extension
  # APIs, so the session itself is a stock IRB workspace (see {.start}).
  module Console
    class << self
      # The graph that `run` and the `query`/`schema` commands operate on. Set by {.start}.
      attr_accessor :graph

      # Starts an interactive session. `graph` is exposed at the prompt, and `run` plus the
      # `query`/`schema` commands operate on it via {.graph}.
      def start(graph)
        self.graph = graph

        IRB.setup(nil)
        IRB.conf[:IRB_NAME] = "rubydex"
        IRB::Irb.new(workspace_for(graph)).run(IRB.conf)
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

      # The session workspace. This is deliberately IRB's own default workspace rather than a
      # hand-rolled binding, because `IRB::WorkSpace.new` derives its binding from a copy of
      # `IRB::TOPLEVEL_BINDING`, which buys three things a custom binding gets wrong:
      #
      #   * `self` is `main` and the cref is `Object`, so `class Foo; end` at the prompt defines
      #     `::Foo`. A `binding` captured inside `class << self` would define
      #     `#<Class:Rubydex::Console>::Foo` instead.
      #   * no caller locals leak in. Deriving from the CLI's `TOPLEVEL_BINDING` would inherit every
      #     top-level local of `exe/rdx` — including `query` and `schema`, and a local shadows the
      #     IRB command of the same name (IRB resolves locals before dispatching commands).
      #   * `_`, `help` and the rest of IRB's own conveniences behave as they do in stock `irb`.
      #
      # Only `graph` is injected; `run` reaches the prompt as a registered IRB helper method.
      def workspace_for(graph)
        workspace = IRB::WorkSpace.new
        workspace.local_variable_set(:graph, graph)
        workspace
      end
    end

    # `run(<CYPHER>)` — an IRB helper method, so it is callable bare at the prompt while still
    # returning a value that can be assigned and navigated.
    class RunHelper < IRB::HelperMethod::Base
      description "Run a Cypher query and return the rows as Ruby objects: run(<CYPHER>)"

      def execute(cypher)
        Console.run(cypher)
      end
    end

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

IRB::HelperMethod.register(:run, Rubydex::Console::RunHelper)
IRB::Command.register(:query, Rubydex::Console::QueryCommand)
IRB::Command.register(:schema, Rubydex::Console::SchemaCommand)
