# frozen_string_literal: true

require "rubydex/cli/command"

module Rubydex
  module CLI
    # `rdx query <CYPHER>` — runs a Cypher query against the workspace graph and prints the result.
    # `--schema` describes the queryable schema instead, which needs no graph.
    class Command
      class Query < Command
        command "query"
        arguments "<CYPHER>"
        summary <<~TEXT
          Run a Cypher query against the workspace graph and print the result.
          Use `query --schema` to describe the queryable schema (labels,
          relationships, properties) without indexing the workspace.
        TEXT

        #: -> void
        def run
          schema = false
          format = "table"

          parse_options!(options: true) do |parser|
            parser.on("--schema", "Describe the queryable schema instead of running a query") { schema = true }
            parser.on("--format FORMAT", ["table", "json"], "Output format (table or json)") do |value|
              format = value
            end
          end

          query = argv.shift

          if schema
            warn("warning: ignoring query argument because `--schema` was given") if query
            # The schema is static, so describe it without building a graph.
            print(Rubydex::Query.schema(format))
            return
          end

          if query.nil? || query.empty?
            abort_with_usage("`query` requires a Cypher query argument (or pass `--schema`)")
          end

          # Parse the query up front so a malformed query fails fast, before the expensive indexing.
          parsed = parse_query(query)

          # Progress goes to stderr so stdout carries only the result (e.g. for piping a query's JSON).
          graph = build_graph($stderr)

          render(parsed, graph, format)
        end

        private

        #: (String query) -> Rubydex::Query
        def parse_query(query)
          Rubydex::Query.parse(query)
        rescue ArgumentError => e
          abort(e.message)
        end

        #: (Rubydex::Query parsed, Rubydex::Graph graph, String format) -> void
        def render(parsed, graph, format)
          print(parsed.render(graph, format))
        rescue ArgumentError => e
          abort(e.message)
        end
      end
    end
  end
end
