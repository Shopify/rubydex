# frozen_string_literal: true

require "rubydex/cli/command"

module Rubydex
  module CLI
    # `rdx explain <RULE> [PATH]` — prints documentation for every discovered rule with that name.
    class Command
      class Explain < Command
        command "explain"
        arguments "<RULE> [PATH]"
        summary "Print complete documentation for a linter rule"

        #: -> void
        def run
          parse_options!

          rule_name = argv.shift
          abort_with_usage("`explain` requires a rule name argument") unless rule_name

          workspace_path = File.expand_path(argv.shift || Dir.pwd)
          abort_with_usage("unexpected argument: #{argv.first}") unless argv.empty?
          abort_with_usage("workspace is not a directory: #{workspace_path}") unless File.directory?(workspace_path)

          require "rubydex/linter"

          rules = load_linter_rules(workspace_path).select { |rule_class| rule_class.rule_name == rule_name }
          abort("Rule does not exist: #{rule_name}") if rules.empty?

          graph = Rubydex::Graph.configure_for_workspace(workspace_path)
          rule_files = rules.map do |rule_class|
            Object.const_source_location(rule_class.name).fetch(0)
          end
          graph.index_all([File.expand_path("../../linter/rule.rb", __dir__), *rule_files])
          graph.resolve

          puts(rules.sort_by(&:name).map { |rule_class| documentation_for(rule_class, graph) }.join("\n"))
        end

        private

        #: (String workspace_path) -> Array[singleton(Rubydex::Linter::Rule)]
        def load_linter_rules(workspace_path)
          Rubydex::Linter::RuleLoader.load(workspace_path)
        rescue Rubydex::Linter::RuleLoadError => error
          abort(error.message)
        end

        #: (singleton(Rubydex::Linter::Rule) rule_class, Graph graph) -> String
        def documentation_for(rule_class, graph)
          rule_name = rule_class.name #: as !nil
          declaration = graph[rule_name] #: as !nil
          documentation = declaration.definitions.flat_map do |definition|
            definition.comments.map { |comment| comment.string.gsub(/^#\s*/, "") }
          end.join("\n")

          return "#{rule_name}: no documentation available." if documentation.empty?

          <<~DOCUMENTATION
            #{rule_name}

            #{documentation}
          DOCUMENTATION
        end
      end
    end
  end
end
