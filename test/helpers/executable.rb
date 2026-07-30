# frozen_string_literal: true

require "open3"
require "rbconfig"

module Test
  module Helpers
    module WithExecutable
      private

      #: (*String) -> [String, String, Process::Status]
      def run_executable(*arguments)
        Open3.capture3(
          RbConfig.ruby,
          "-rbundler/setup",
          executable_path,
          *arguments,
        )
      end

      #: -> String
      def executable_path
        File.expand_path("../../exe/rdx", __dir__)
      end
    end
  end
end
