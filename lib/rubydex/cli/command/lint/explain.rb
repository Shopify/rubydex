# frozen_string_literal: true

require "rubydex"
require "rubydex/linter/rule_loader"

module Rubydex
  module CLI
    class Command
      class Lint
        # `rdx lint explain <RULE>` — prints documentation for rules with a matching name.
        class Explain < Command
          BASE_RULE_NAME = "Rubydex::Linter::Rule" #: String
          BASE_RULE_PATH = File.expand_path("../../../linter/rule.rb", __dir__) #: String

          class << self
            #: -> String
            def usage_form
              "lint explain <RULE>"
            end
          end

          #: -> void
          def run
            parse_options!

            rule_name = argv.shift
            abort_with_usage("`lint explain` requires a rule name argument") unless rule_name
            abort_with_usage("unexpected argument: #{argv.first}") unless argv.empty?

            workspace_path = current_workspace_path

            graph = Rubydex::Graph.configure_for_workspace(workspace_path)
            graph.index_all([BASE_RULE_PATH, *Rubydex::Linter::RuleLoader.paths(workspace_path)])
            graph.resolve

            base_rule = graph[BASE_RULE_NAME]
            abort("Base rule class #{BASE_RULE_NAME} is not found. This is likely an issue in Rubydex itself") unless base_rule.is_a?(Rubydex::Class)

            rule_declarations = base_rule.descendants.grep(Rubydex::Class) #: as Array[Rubydex::Class]
            matched_rule_names = rule_declarations.filter_map do |declaration|
              declaration_name = declaration.name
              next unless declaration_name
              next if declaration_name == BASE_RULE_NAME
              next unless declaration_name.end_with?(rule_name)

              declaration
            end
            abort("Rule does not exist: #{rule_name}") if matched_rule_names.empty?

            puts(matched_rule_names.sort_by(&:name).map { |rule| documentation_for(rule) }.join("\n"))
          end

          private

          #: (Rubydex::Class rule) -> String
          def documentation_for(rule)
            rule_name = rule.name #: as !nil
            documentation = rule.definitions.flat_map do |definition|
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
end
