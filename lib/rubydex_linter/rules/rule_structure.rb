# frozen_string_literal: true

module Rubydex
  module Linter
    module Rules
      # Ensures discovered workspace linter rules follow these conventions:
      #
      # - A rule file does not define more than one linter rule.
      # - Each rule subclass outside a test directory is in a rule directory.
      # - Each checked rule subclass is in the `Rubydex::Linter::Rules` namespace.
      #
      # The rule directories are `rubydex_linter/rules/` and `lib/rubydex_linter/rules/`.
      # This rule does not report files in those directories that define no rule subclass.
      class RuleStructure < CustomRule
        include Helpers::SourceAccessHelpers

        BASE_RULE_NAME = "Rubydex::Linter::CustomRule" #: String
        RULE_NAMESPACE = "Rubydex::Linter::Rules" #: String
        RULE_FILE_PATTERNS = [
          "rubydex_linter/rules/**/*.rb",
          "lib/rubydex_linter/rules/**/*.rb",
        ].freeze #: Array[String]
        TEST_FILE_PATTERNS = ["test/**/*", "**/test/**/*"].freeze #: Array[String]

        class << self
          # @override
          #: -> singleton(Severity::Base)
          def default_severity
            Severity::Error
          end
        end

        # @override
        #: -> void
        def lint
          rules = child_classes(BASE_RULE_NAME)
          rule_definitions_by_file = {} #: Hash[String, Hash[Rubydex::Class, Definition]]

          rules.each do |rule|
            rule.definitions.each do |rule_definition|
              uri = rule_definition.document.uri
              path = path_for_uri(uri)
              next unless path_in_workspace?(path)

              if rule_file?(path)
                rule_definitions = (rule_definitions_by_file[uri] ||= {}) #: Hash[Rubydex::Class, Definition]
                rule_definitions[rule] ||= rule_definition
              elsif !test_file?(path)
                report_wrong_rule_directory(rule.name, rule_definition)
              end
            end

            rule_definition = rule.definitions.find do |definition|
              path = path_for_uri(definition.document.uri)
              path_in_workspace?(path) && (rule_file?(path) || !test_file?(path))
            end
            next unless rule_definition
            next if rule.name.start_with?("#{RULE_NAMESPACE}::")

            report_wrong_rule_namespace(rule.name, rule_definition)
          end

          rule_definitions_by_file.each do |uri, rule_definitions|
            next if rule_definitions.length <= 1

            add_diagnostic(
              "Each rule file must define only one linter rule; found #{rule_definitions.length}.",
              file_location(uri),
              related_information: rule_definitions.map do |rule, rule_definition|
                RelatedInformation.new(
                  "`#{rule.name}` is defined here.",
                  diagnostic_location(rule_definition),
                )
              end,
            )
          end
        end

        private

        #: (String, Definition) -> void
        def report_wrong_rule_directory(rule_name, rule_definition)
          add_diagnostic(
            "`#{rule_name}` must be defined under `rubydex_linter/rules/` or `lib/rubydex_linter/rules/`.",
            diagnostic_location(rule_definition),
          )
        end

        #: (String, Definition) -> void
        def report_wrong_rule_namespace(rule_name, rule_definition)
          add_diagnostic(
            "`#{rule_name}` must be defined under `#{RULE_NAMESPACE}`.",
            diagnostic_location(rule_definition),
          )
        end

        #: (String) -> bool
        def path_in_workspace?(path)
          workspace = graph.workspace_path
          path == workspace || path.start_with?("#{workspace}/")
        end

        #: (String) -> bool
        def rule_file?(path)
          path_matches_patterns?(path, RULE_FILE_PATTERNS)
        end

        #: (String) -> bool
        def test_file?(path)
          path_matches_patterns?(path, TEST_FILE_PATTERNS)
        end

        #: (String, Array[String]) -> bool
        def path_matches_patterns?(path, patterns)
          Helpers::PathHelpers.path_matches_patterns?(
            path,
            patterns,
            workspace: graph.workspace_path,
            flags: Helpers::PathHelpers::RUBOCOP_EXCLUDE_FNMATCH_FLAGS,
          )
        end
      end
    end
  end
end
