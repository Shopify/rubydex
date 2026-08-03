# frozen_string_literal: true

require "rubydex/cli/command"

module Rubydex
  module CLI
    # `rdx lint [PATH]` — loads project rules and runs them against a workspace.
    class Command
      class Lint < Command
        command "lint"
        arguments "[PATH]"
        summary "Run semantic lint rules against a workspace"

        #: -> void
        def run
          rule_files = [] #: Array[String]

          parse_options!(options: true) do |parser|
            parser.on("-r", "--require FILE", "Load rules from FILE, relative to PATH (repeatable)") do |file|
              rule_files << file
            end
          end

          workspace_path = File.expand_path(argv.shift || Dir.pwd)
          abort_with_usage("unexpected argument: #{argv.first}") unless argv.empty?
          abort_with_usage("`lint` requires at least one --require FILE") if rule_files.empty?
          abort_with_usage("workspace is not a directory: #{workspace_path}") unless File.directory?(workspace_path)

          # Keep top-level help lightweight: command discovery loads this file before the native
          # extension, while linter support is only needed when this command runs.
          require "rubydex/linter"

          rules = load_linter_rules(rule_files, workspace_path)
          abort("No Rubydex::Linter::Rule subclasses were loaded") if rules.empty?

          graph = build_graph($stderr, workspace_path: workspace_path, fail_on_index_errors: true)
          result = Rubydex::Linter::Runner.new(graph, rules: rules).run
          result.diagnostics.each { |diagnostic| puts(format_linter_diagnostic(diagnostic)) }
          exit(1) unless result.success?
        end

        private

        #: (Array[String] rule_files, String workspace_path) -> Array[singleton(Linter::Rule)]
        def load_linter_rules(rule_files, workspace_path)
          existing_rules = Rubydex::Linter::Rule.subclasses

          rule_files.each do |rule_file|
            require File.expand_path(rule_file, workspace_path)
          rescue LoadError, SyntaxError => error
            abort("Unable to load linter rules from #{rule_file}: #{error.message}")
          end

          Rubydex::Linter::Rule.subclasses - existing_rules
        end

        #: (Location location) -> String
        def format_linter_location(location)
          display_location = location.to_display
          path = begin
            display_location.to_file_path
          rescue Rubydex::Location::NotFileUriError
            display_location.uri
          end

          "#{path}:#{display_location.start_line}:#{display_location.start_column}"
        end

        #: (Diagnostic diagnostic) -> String
        def format_linter_diagnostic(diagnostic)
          content = +"#{format_linter_location(diagnostic.location)}: " \
            "#{diagnostic.severity.value}: #{diagnostic.rule}: #{diagnostic.message}"

          diagnostic.related_information.each do |information|
            content << "\n  #{format_linter_location(information.location)}: #{information.message}"
          end

          content
        end
      end
    end
  end
end
