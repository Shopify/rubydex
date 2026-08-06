# frozen_string_literal: true

module Rubydex
  module Linter
    # Loads project and bundled-gem rules using the Rubydex linter path convention.
    class RuleLoader
      RULE_GLOB = "rubydex_linter/rules/**/*.rb" #: String
      BUILT_IN_RULE_GLOB = File.expand_path("../../rubydex_linter/rules/**/*.rb", __dir__) #: String

      class << self
        #: (String workspace_path) -> Array[singleton(Rule)]
        def load(workspace_path)
          built_in_rule_files = Dir.glob(BUILT_IN_RULE_GLOB)
          workspace_rule_files = Dir.glob(RULE_GLOB, base: workspace_path).map do |rule_file|
            File.expand_path(rule_file, workspace_path)
          end
          dependency_rule_files = if ENV["BUNDLE_GEMFILE"]
            Gem.find_latest_files(RULE_GLOB)
          else
            []
          end

          rule_files = built_in_rule_files + workspace_rule_files + dependency_rule_files
          rule_files.each do |rule_file|
            require rule_file
          rescue LoadError, SyntaxError => error
            raise RuleLoadError, "Unable to load linter rules from #{rule_file}: #{error.message}", cause: error
          end

          expanded_rule_files = rule_files.map { |rule_file| File.expand_path(rule_file) }
          descendants_of(Rule).select do |rule_class|
            rule_name = rule_class.name #: as !nil
            source_location = Object.const_source_location(rule_name)
            source_location && expanded_rule_files.include?(File.expand_path(source_location.fetch(0)))
          end
        end

        private

        #: (singleton(Rule)) -> Array[singleton(Rule)]
        def descendants_of(parent)
          subclasses = parent.subclasses #: as Array[singleton(Rule)]
          subclasses.flat_map do |subclass|
            [subclass, *descendants_of(subclass)]
          end
        end
      end
    end

    class RuleLoadError < StandardError; end
  end
end
