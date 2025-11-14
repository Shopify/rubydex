# frozen_string_literal: true

require "fileutils"
require "pathname"

module Test
  module Helpers
    class Context
      class << self
        #: (?String? name) -> instance
        def mktmp!(name = nil)
          new(::Dir.mktmpdir(name))
        end
      end

      #: String
      attr_reader :absolute_path

      #: (String absolute_path) -> void
      def initialize(absolute_path)
        @absolute_path = ::File.realpath(absolute_path) #: String
      end

      #: (String relative_path) -> String
      def absolute_path_to(relative_path)
        ::File.join(@absolute_path, relative_path)
      end

      #: (String) -> String
      def uri_to(relative_path)
        path = absolute_path_to(relative_path)
        # TODO: This has to go away once we have a proper URI abstraction
        path.prepend("/") if Gem.win_platform?
        URI::File.build(path: path).to_s
      end

      #: (String relative_path, ?String contents, ?append: bool) -> void
      def write!(relative_path, contents = "", append: false)
        absolute_path = absolute_path_to(relative_path)
        FileUtils.mkdir_p(File.dirname(absolute_path))
        File.write(absolute_path, contents, mode: append ? "a" : "w")
      end

      #: (?String, ?relative: bool) -> Array[String]
      def glob(pattern = "**/*", relative: false)
        Dir.glob(absolute_path_to(pattern)).map do |path|
          if relative
            Pathname.new(path).relative_path_from(absolute_path).to_s
          else
            path
          end
        end.sort
      end

      #: -> void
      def destroy!
        FileUtils.rm_rf(absolute_path)
      end
    end

    module WithContext
      #: { (Context)-> void } -> void
      def with_context(&block)
        context = Context.mktmp!
        block.call(context)
      ensure
        context.destroy!
      end
    end
  end
end
