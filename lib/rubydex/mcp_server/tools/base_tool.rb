# frozen_string_literal: true

require "json"
require "pathname"
require "uri"

module Rubydex
  module MCPServer
    class BaseTool < Tool
      DEFAULT_LIMIT = 50

      #: (Graph) -> void
      def initialize(graph)
        super()
        @graph = graph
        @root_path = graph.workspace_path
      end

      #: (Hash | Error) -> Tool::Response
      def response(payload)
        Tool::Response.new([{ type: "text", text: JSON.generate(payload) }])
      end

      #: (Declaration) -> String
      def declaration_kind(declaration)
        return "<TODO>" if declaration.is_a?(Rubydex::Todo)

        declaration.class.name.delete_prefix("Rubydex::")
      end

      #: (String) -> String
      def format_path(uri)
        path = file_path_for_uri(uri)
        return uri unless path

        absolute_path = File.expand_path(path)
        absolute_root = File.expand_path(@root_path)
        relative_path = Pathname.new(absolute_path).relative_path_from(Pathname.new(absolute_root)).to_s

        relative_path.start_with?("..") ? absolute_path : relative_path
      end

      #: (String) -> String?
      def file_path_for_uri(uri)
        parsed = URI.parse(uri)
        return unless parsed.scheme == "file"

        path = URI.decode_uri_component(parsed.path)
        path.delete_prefix!("/") if Gem.win_platform?
        path
      rescue URI::InvalidURIError, ArgumentError
        nil
      end

      #: (String) -> Document?
      def document_for_path(file_path)
        absolute_target = if Pathname.new(file_path).absolute?
          file_path
        else
          File.join(@root_path, file_path)
        end
        canonical_target = File.realpath(absolute_target)
        @graph.documents.find do |document|
          path = file_path_for_uri(document.uri)
          path && File.expand_path(path) == canonical_target
        end
      end

      #: (Location) -> Hash
      def display_location(location)
        display = location.to_display
        {
          path: format_path(display.uri),
          line: display.start_line,
        }
      end

      #: (Enumerable, Integer?, Integer?, Integer) -> [Array, Integer]
      def paginate(items, offset, limit, max_limit)
        offset = (offset || 0).to_i
        offset = 0 unless offset.positive?
        limit = (limit || DEFAULT_LIMIT).to_i
        limit = DEFAULT_LIMIT unless limit.positive?
        limit = [limit, max_limit].min

        page = []
        index = 0
        items.each do |item|
          page << item if index >= offset && page.length < limit
          index += 1
        end

        [page, index]
      end

      #: (String) -> Declaration | Error
      def lookup_declaration(name)
        declaration = @graph[name]
        return declaration if declaration

        Error.new(
          "not_found",
          "Declaration '#{name}' not found",
          "Try search_declarations with a partial name to find the correct FQN",
        )
      end
    end
  end
end
