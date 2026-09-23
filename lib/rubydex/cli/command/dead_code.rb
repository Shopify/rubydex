# frozen_string_literal: true

require "rubydex/cli/command"

module Rubydex
  module CLI
    # `rdx dead-code` — reports workspace declarations with no detected references.
    class Command
      class DeadCode < Command
        command "dead-code"
        summary <<~TEXT
          Find workspace classes, modules and constants with no detected references.
          Candidates require review; missing references do not prove safe deletion.
          Exclusions are read from [dead-code].exclude in rubydex.toml.
        TEXT

        #: -> void
        def run
          format = "table"
          path = nil #: String?
          fail_on_candidates = false

          parse_options!(options: true) do |parser|
            parser.separator("")
            parser.separator(self.class.summary)
            parser.separator("")
            parser.on("--format FORMAT", ["table", "json"], "Output format (table or json)") do |value|
              format = value
            end
            parser.on("--path GLOB", "Filter definitions by workspace-relative file or glob") { |value| path = value }
            parser.on("--fail-on-candidates", "Exit with status 1 when candidates are found") { fail_on_candidates = true }
          end

          abort_with_usage("unexpected argument: #{argv.first}") unless argv.empty?

          # Command discovery runs before the native extension is loaded, so load report support
          # only after parsing options and handling help.
          require "rubydex/dead_code"

          graph = build_graph($stderr, workspace_path: current_workspace_path)
          candidates = Rubydex::DeadCode.candidates(graph, path:)

          if format == "json"
            require "json"

            puts(JSON.generate(candidates:, total: candidates.length))
          else
            print_table(candidates)
          end

          exit(1) if fail_on_candidates && candidates.any?
        rescue Rubydex::ConfigError => error
          abort(error.message)
        end

        private

        #: (Array[Rubydex::DeadCode::candidate] candidates) -> void
        def print_table(candidates)
          if candidates.empty?
            puts("No dead-code candidates found.")
            return
          end

          require "terminal-table"

          rows = candidates.map do |candidate|
            locations = candidate[:locations].map do |location|
              "#{location[:path]}:#{location[:line]}:#{location[:column]}"
            end

            [candidate[:name], candidate[:kind], locations.join("\n")]
          end

          puts(Terminal::Table.new(title: "Dead-code candidates", headings: ["Name", "Kind", "Location"], rows:))
          puts("#{candidates.length} #{candidates.length == 1 ? "candidate" : "candidates"} found.")
        end
      end
    end
  end
end
