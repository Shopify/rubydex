# frozen_string_literal: true

require "rubydex/cli/command"

module Rubydex
  module CLI
    # `rdx lint [PATH]` — discovers project and dependency rules and runs them against a workspace.
    class Command
      class Lint < Command
        command "lint"
        arguments "[PATH]"
        summary "Run semantic lint rules against a workspace"

        #: -> void
        def run
          parse_options!

          workspace_path = File.expand_path(argv.shift || Dir.pwd)
          abort_with_usage("unexpected argument: #{argv.first}") unless argv.empty?
          abort_with_usage("workspace is not a directory: #{workspace_path}") unless File.directory?(workspace_path)

          # Keep top-level help lightweight: command discovery loads this file before the native
          # extension, while linter support is only needed when this command runs.
          require "rubydex/linter"

          rules = load_linter_rules(workspace_path)
          abort("No Rubydex::Linter::Rule subclasses were loaded") if rules.empty?
          config = Rubydex::Config.load(workspace_path)
          warn_unknown_rules(config.linter, rules)

          graph = build_graph($stderr, workspace_path:, config:)
          runner = Rubydex::Linter::Runner.new(graph, rules:, config: config.linter)
          rule_count = runner.rules.size
          $stderr.puts("Running #{rule_count} #{pluralize("rule", rule_count)}...")
          result = runner.run
          if result.diagnostics.empty?
            print_summary(graph.documents.count, result.diagnostics)
            return
          end

          print_offenses(result.diagnostics, graph)
          exit(1) unless result.success?
        end

        private

        #: (Rubydex::LinterConfig config, Array[singleton(Rubydex::Linter::Rule)] known_rule_classes) -> void
        def warn_unknown_rules(config, known_rule_classes)
          known_rule_names = known_rule_classes.map(&:rule_name).uniq.sort
          unknown_rule_names = config.rules.keys.reject { |name| known_rule_names.include?(name) }.sort
          return if unknown_rule_names.empty?

          formatted_names = unknown_rule_names.map { |name| "`#{name}`" }.join(", ")
          warn(
            "warning: linter config references rules that were not loaded: #{formatted_names}. " \
              "Known rules: #{known_rule_names.join(", ")}",
          )
        end

        #: (String workspace_path) -> Array[singleton(Rubydex::Linter::Rule)]
        def load_linter_rules(workspace_path)
          Rubydex::Linter::RuleLoader.load(workspace_path)
          Rubydex::Linter::Rule.subclasses
        rescue Rubydex::Linter::RuleLoadError => error
          abort(error.message)
        end

        #: (Array[Diagnostic] diagnostics, Graph graph) -> void
        def print_offenses(diagnostics, graph)
          puts("Offenses:")
          puts

          diagnostics.each do |diagnostic|
            puts(format_linter_diagnostic(diagnostic, workspace_path: graph.workspace_path))
            print_source_excerpt(diagnostic.location)
            puts
          end

          print_summary(graph.documents.count, diagnostics)
        end

        #: (Location location, workspace_path: String) -> String
        def format_linter_location(location, workspace_path:)
          display_location = location.to_display
          path = Rubydex::Linter::Helpers::PathHelpers.display_path(display_location, workspace: workspace_path)

          "#{path}:#{display_location.start_line}:#{display_location.start_column}"
        end

        #: (Diagnostic diagnostic, workspace_path: String) -> String
        def format_linter_diagnostic(diagnostic, workspace_path:)
          content = +"#{format_linter_location(diagnostic.location, workspace_path:)}: " \
            "#{diagnostic.severity.value}: #{diagnostic.rule}: #{diagnostic.message}"

          diagnostic.related_information.each do |information|
            content << "\n  #{format_linter_location(information.location, workspace_path:)}: #{information.message}"
          end

          content
        end

        #: (Location location) -> void
        def print_source_excerpt(location)
          line = source_line_for(location)
          return unless line

          end_column = location.end_line == location.start_line ? location.end_column : line.length
          carets = "^" * [end_column - location.start_column, 1].max

          puts
          puts(line)
          puts("#{" " * location.start_column}#{carets}")
        end

        #: (Location location) -> String?
        def source_line_for(location)
          source_lines_for(location.to_file_path)[location.start_line]
        rescue Errno::ENOENT, Errno::EACCES, Rubydex::Location::NotFileUriError
          nil
        end

        #: (String path) -> Array[String]
        def source_lines_for(path)
          @source_lines_cache ||= {} #: Hash[String, Array[String]]?
          @source_lines_cache[path] ||= File.readlines(path, chomp: true)
        end

        #: (Integer file_count, Array[Diagnostic] diagnostics) -> void
        def print_summary(file_count, diagnostics)
          if diagnostics.empty?
            puts("#{file_count} #{pluralize("file", file_count)} inspected, no offenses detected")
            return
          end

          severity_counts = diagnostics.map(&:severity).tally
          offense_count = diagnostics.length
          error_count = severity_counts.fetch(Rubydex::Severity::Error, 0)
          warning_count = severity_counts.fetch(Rubydex::Severity::Warning, 0)
          hint_count = severity_counts.fetch(Rubydex::Severity::Hint, 0)
          severity_summary = [
            "#{error_count} #{pluralize("error", error_count)}",
            "#{warning_count} #{pluralize("warning", warning_count)}",
            "#{severity_counts.fetch(Rubydex::Severity::Information, 0)} info",
            "#{hint_count} #{pluralize("hint", hint_count)}",
          ].join(", ")

          puts(
            "#{file_count} #{pluralize("file", file_count)} inspected, " \
              "#{offense_count} #{pluralize("offense", offense_count)} detected: #{severity_summary}",
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
