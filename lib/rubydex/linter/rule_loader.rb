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
          existing_rules = Rule.subclasses
          rule_files = Dir.glob(BUILT_IN_RULE_GLOB)
          rule_files.concat(Dir.glob(RULE_GLOB, base: workspace_path).map do |rule_file|
            File.expand_path(rule_file, workspace_path)
          end)
          rule_files.concat(Gem.find_latest_files(RULE_GLOB)) if ENV["BUNDLE_GEMFILE"]

          rule_files.each do |rule_file|
            require rule_file
          rescue LoadError, SyntaxError => error
            raise RuleLoadError, "Unable to load linter rules from #{rule_file}: #{error.message}", cause: error
          end

          built_in_rules = [Rules::RuleStructure] #: Array[singleton(Rule)]
          built_in_rules | (Rule.subclasses - existing_rules)
        end
      end
    end

    class RuleLoadError < StandardError; end
  end
end
