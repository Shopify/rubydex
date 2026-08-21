# frozen_string_literal: true

module Rubydex
  module Linter
    # Loads project and bundled-gem rules using the Rubydex linter path convention.
    class RuleLoader
      BUILT_IN_RULE_GLOB = File.expand_path("../../{rubydex_linter,rubydex}/rules/**/*.rb", __dir__) #: String
      RULE_GLOB = "rubydex_linter/rules/**/*.rb" #: String

      class << self
        #: (String workspace_path) -> Array[String]
        def paths(workspace_path)
          rule_files = Dir.glob(BUILT_IN_RULE_GLOB)
          rule_files.concat(Dir.glob(RULE_GLOB, base: workspace_path).map do |rule_file|
            File.expand_path(rule_file, workspace_path)
          end)
          rule_files.concat(Gem.find_latest_files(RULE_GLOB)) if ENV["BUNDLE_GEMFILE"]

          # Bundler can return Rubydex's built-in rules through the gem search as well.
          rule_files.uniq
        end

        #: (String workspace_path) -> void
        def load(workspace_path)
          paths(workspace_path).each do |rule_file|
            require rule_file
          rescue LoadError, SyntaxError => error
            raise RuleLoadError, "Unable to load linter rules from #{rule_file}: #{error.message}", cause: error
          end
        end
      end
    end

    class RuleLoadError < StandardError; end
  end
end
