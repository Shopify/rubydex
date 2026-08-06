# frozen_string_literal: true

require "rubydex/linter"

module Rubydex
  module Linter
    module Rules
      # Ensures discovered linter rule files contain one correctly placed and named rule class.
      class RuleStructure < Rule
        include Helpers::SourceAccessHelpers

        BASE_RULE_NAME = "Rubydex::Linter::Rule" #: String
        RULE_NAMESPACE = "Rubydex::Linter::Rules" #: String
        RULE_FILE_PATTERNS = [
          "rubydex_linter/rules/**/*.rb",
          "lib/rubydex_linter/rules/**/*.rb",
        ].freeze #: Array[String]
        TEST_FILE_PATTERNS = ["test/**/*", "**/test/**/*"].freeze #: Array[String]

        # @override
        #: -> singleton(Severity::Base)
        def severity
          Severity::Error
        end

        # @override
        #: -> void
        def lint
          rule_class_names = child_classes(BASE_RULE_NAME).to_h { |rule_class| [rule_class.name, true] }

          graph.documents.each do |document|
            path = path_for_uri(document.uri)
            next unless workspace_path?(path)

            rule_file = rule_file?(path)
            next if !rule_file && test_file?(path)

            definitions = rule_definitions(document, rule_class_names)
            validate_rule_file(document, definitions) if rule_file

            definitions.each do |name, definition|
              validate_rule_definition(name, definition, path)
            end
          end
        end

        private

        #: (Document, Hash[String, bool]) -> Hash[String, ClassDefinition]
        def rule_definitions(document, rule_class_names)
          definitions = {} #: Hash[String, ClassDefinition]

          document.definitions.each do |definition|
            next unless definition.is_a?(ClassDefinition)

            declaration = definition.declaration
            next unless declaration
            next unless rule_class_names.key?(declaration.name)

            definitions[declaration.name] ||= definition
          end

          definitions
        end

        #: (Document, Hash[String, ClassDefinition]) -> void
        def validate_rule_file(document, definitions)
          return if definitions.one?

          add_diagnostic(
            "Each rule file must define exactly one class that inherits from `#{BASE_RULE_NAME}`; " \
              "found #{definitions.length}.",
            file_location(document.uri),
            related_information: definitions.map do |name, definition|
              RelatedInformation.new("`#{name}` is defined here.", diagnostic_location(definition))
            end,
          )
        end

        #: (String, ClassDefinition, String) -> void
        def validate_rule_definition(name, definition, path)
          unless rule_file?(path)
            add_diagnostic(
              "`#{name}` must be defined under `rubydex_linter/rules/` or `lib/rubydex_linter/rules/`.",
              diagnostic_location(definition),
            )
          end

          return if name.start_with?("#{RULE_NAMESPACE}::")

          add_diagnostic(
            "`#{name}` must be defined under `#{RULE_NAMESPACE}`.",
            diagnostic_location(definition),
          )
        end

        #: (String) -> bool
        def workspace_path?(path)
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
