# frozen_string_literal: true

require "pathname"
require "uri"

module Rubydex
  module Linter
    module Helpers
      # @requires_ancestor: Rubydex::Linter::Rule
      module PathHelpers
        RUBOCOP_EXCLUDE_FNMATCH_FLAGS =
          File::FNM_PATHNAME | File::FNM_EXTGLOB | File::FNM_DOTMATCH #: Integer
        TEST_PATHS = ["test/**", "**/test/**", "**/*_test.rb"].freeze

        class << self
          #: (String, Array[String], workspace: String, ?flags: Integer) -> bool
          def path_matches_patterns?(path, patterns, workspace:, flags: 0)
            return false unless path == workspace || path.start_with?("#{workspace}/")

            relative_path = path.delete_prefix("#{workspace}/")
            patterns.any? { |pattern| File.fnmatch?(pattern, relative_path, flags) }
          end

          # Returns the path to show users for a location: relative to the workspace for files inside it, the absolute
          # path for files outside it (dependencies, for example) and the URI opaque for non file URIs, so that
          # `untitled:Untitled-1` displays as `Untitled-1`.
          #
          #: (Location, workspace: String) -> String
          def display_path(location, workspace:)
            path = location.to_file_path
            return path unless path == workspace || path.start_with?("#{workspace}/")

            Pathname.new(path).relative_path_from(workspace).to_s
          rescue Location::NotFileUriError
            URI(location.uri).opaque || location.uri
          end
        end

        #: (Enumerable[Rubydex::Definition], Array[String]) -> Array[Rubydex::Definition]
        def reject_definitions_in_paths(definitions, excluded_patterns)
          workspace = graph.workspace_path

          definitions.reject do |definition|
            path = path_for_definition(definition)
            path.nil? || (path != workspace && !path.start_with?("#{workspace}/")) ||
              path_matches_patterns?(path, excluded_patterns)
          end
        end

        #: (Enumerable[Rubydex::Definition], Array[String]) -> Array[Rubydex::Definition]
        def select_definitions_in_paths(definitions, patterns)
          definitions.select do |definition|
            path = path_for_definition(definition)
            path && path_matches_patterns?(path, patterns)
          end
        end

        private

        #: (String, Array[String]) -> bool
        def path_matches_patterns?(path, patterns)
          PathHelpers.path_matches_patterns?(path, patterns, workspace: graph.workspace_path)
        end

        #: (String) -> bool
        def test_path?(path)
          path_matches_patterns?(path, TEST_PATHS)
        end

        #: (Rubydex::Definition) -> String?
        def path_for_definition(definition)
          definition.location.to_file_path
        rescue Location::NotFileUriError
          nil
        end
      end
    end
  end
end
