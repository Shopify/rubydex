# frozen_string_literal: true

require "rubydex/cli/command"

module Rubydex
  module CLI
    # `rdx dead_code` — lists dead code candidates in the current workspace.
    class Command
      class DeadCode < Command
        command "dead_code"
        summary "List dead code candidates in the current workspace"

        #: -> void
        def run
          parse_options!
          abort_with_usage("unexpected argument: #{argv.first}") unless argv.empty?

          $stderr.puts("Looking for potentially dead code...")
          graph = build_graph($stderr, workspace_path: current_workspace_path)
          candidates = graph.dead_code_candidates.sort_by(&:name)

          candidates.each do |candidate|
            puts(candidate.name)
            candidate.definitions.map(&:location).sort.each do |location|
              display = location.to_display
              path = display_path(display, workspace: graph.workspace_path)
              puts("  #{path}:#{display.start_line},#{display.start_column}-#{display.end_line},#{display.end_column}")
            end
            puts
          end

          print_summary(graph.documents.count, candidates.length)
        end

        private

        #: (Integer file_count, Integer candidate_count) -> void
        def print_summary(file_count, candidate_count)
          if candidate_count.zero?
            puts("#{file_count} #{pluralize("file", file_count)} inspected, no dead code candidates found")
            return
          end

          puts(
            "#{file_count} #{pluralize("file", file_count)} inspected, " \
              "#{candidate_count} dead code #{pluralize("candidate", candidate_count)} found",
          )
        end

        #: (String word, Integer count) -> String
        def pluralize(word, count)
          count == 1 ? word : "#{word}s"
        end
      end
    end
  end
end
