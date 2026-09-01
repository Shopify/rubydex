# frozen_string_literal: true

require "rubydex/cli/command"

module Rubydex
  module CLI
    # `rdx lint` — discovers project and dependency rules and runs them against the current workspace.
    class Command
      class Lint < Command
        command "lint"
        summary "Run semantic lint rules in the current workspace"

        #: -> void
        def run
          if argv.first == "explain"
            argv.shift
            require "rubydex/cli/command/lint/explain"
            Explain.new(argv).run
            return
          end

          parse_options! do |parser|
            parser.separator("")
            parser.separator("Commands:")
            parser.separator("  explain <RULE>  Print documentation for matching linter rules")
            parser.separator("")
            parser.separator("Options:")
          end

          abort_with_usage("unexpected argument: #{argv.first}") unless argv.empty?
          workspace_path = current_workspace_path

          # Keep top-level help lightweight: command discovery loads this file before the native
          # extension, while linter support is only needed when this command runs.
          require "rubydex/linter"

          custom_rules = load_linter_rules(workspace_path)
          warn_unknown_rules

          graph = build_graph($stderr, workspace_path:, config:)
          runner = Rubydex::Linter::Runner.new(graph, custom_rules:, config: linter_config)
          $stderr.puts("Linting...")
          diagnostics = runner.run
          by_severity = diagnostics.group_by { |diagnostic| diagnostic.rule.severity(linter_config) }

          if diagnostics.empty?
            print_summary(graph.documents.count, by_severity)
            return
          end

          print_offenses(by_severity, graph)
          exit(1) if by_severity[Rubydex::Severity::Error]&.any?
        end

        private

        #: -> Rubydex::Config
        def config
          @config ||= Rubydex::Config.load(current_workspace_path) #: Rubydex::Config?
        end

        #: -> Rubydex::LinterConfig
        def linter_config
          @linter_config ||= config.linter #: Rubydex::LinterConfig?
        end

        #: () -> void
        def warn_unknown_rules
          known_rule_names = (Rule.subclasses + Rubydex::Linter::CustomRule.subclasses).filter_map do |rule|
            name = rule.rule_name
            name unless name == "CustomRule"
          end
          known_rule_names.uniq!
          known_rule_names.sort!

          unknown_rule_names = linter_config.rules.keys.reject { |name| known_rule_names.include?(name) }.sort
          return if unknown_rule_names.empty?

          formatted_names = unknown_rule_names.map { |name| "`#{name}`" }.join(", ")
          warn(
            "warning: linter config references rules that were not loaded: #{formatted_names}. " \
              "Known rules: #{known_rule_names.join(", ")}",
          )
        end

        #: (String workspace_path) -> Array[singleton(Rubydex::Linter::CustomRule)]
        def load_linter_rules(workspace_path)
          Rubydex::Linter::RuleLoader.load(workspace_path)
          Rubydex::Linter::CustomRule.subclasses
        rescue Rubydex::Linter::RuleLoadError => error
          abort(error.message)
        end

        #: (Hash[singleton(Severity::Base), Array[Diagnostic]], Graph) -> void
        def print_offenses(grouped_diagnostics, graph)
          puts("Offenses:")
          puts

          Rubydex::Severity::ALL.each do |severity|
            diagnostics = grouped_diagnostics[severity]
            next unless diagnostics

            diagnostics.sort_by! do |diagnostic|
              location = diagnostic.location

              [
                location.uri,
                location.start_line,
                location.start_column,
                location.end_line,
                location.end_column,
                diagnostic.rule.rule_name,
                diagnostic.message,
              ]
            end

            diagnostics.each do |diagnostic|
              puts(format_linter_diagnostic(severity, diagnostic, workspace_path: graph.workspace_path))
              print_source_excerpt(diagnostic.location)
              puts
            end
          end

          print_summary(graph.documents.count, grouped_diagnostics)
          puts("For more information about a rule, run `rdx lint explain RuleName`.")
        end

        #: (Location location, workspace_path: String) -> String
        def format_linter_location(location, workspace_path:)
          display_location = location.to_display
          path = Rubydex::Linter::Helpers::PathHelpers.display_path(display_location, workspace: workspace_path)

          "#{path}:#{display_location.start_line}:#{display_location.start_column}"
        end

        #: (singleton(Severity::Base), Diagnostic, workspace_path: String) -> String
        def format_linter_diagnostic(severity, diagnostic, workspace_path:)
          content = +"#{format_linter_location(diagnostic.location, workspace_path:)}: " \
            "#{severity.value}: #{diagnostic.rule.rule_name}: #{diagnostic.message}"

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

        #: (Integer, Hash[singleton(Severity::Base), Array[Diagnostic]]) -> void
        def print_summary(file_count, grouped_diagnostics)
          if grouped_diagnostics.empty?
            puts("#{file_count} #{pluralize("file", file_count)} inspected, no offenses detected")
            return
          end

          offense_count = grouped_diagnostics.each_value.sum(&:length)

          error_count = grouped_diagnostics[Rubydex::Severity::Error]&.length || 0
          warning_count = grouped_diagnostics[Rubydex::Severity::Warning]&.length || 0
          information_count = grouped_diagnostics[Rubydex::Severity::Information]&.length || 0
          hint_count = grouped_diagnostics[Rubydex::Severity::Hint]&.length || 0

          severity_summary = [
            "#{error_count} #{pluralize("error", error_count)}",
            "#{warning_count} #{pluralize("warning", warning_count)}",
            "#{information_count} info",
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
