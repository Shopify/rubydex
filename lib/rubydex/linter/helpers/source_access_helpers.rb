# frozen_string_literal: true

module Rubydex
  module Linter
    module Helpers
      # File and URI access for linter rules.
      module SourceAccessHelpers
        #: (String) -> Location
        def file_location(uri)
          Location.new(uri: uri, start_line: 0, end_line: 0, start_column: 0, end_column: 0)
        end

        #: (String) -> String
        def path_for_uri(uri)
          file_location(uri).to_file_path
        rescue Location::NotFileUriError
          uri
        end

        private

        #: (Location) -> String?
        def source_for_location(location)
          File.read(location.to_file_path)
        rescue Errno::ENOENT
          nil
        end
      end
    end
  end
end
