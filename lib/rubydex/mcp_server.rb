# frozen_string_literal: true

require "json"
require "pathname"
require "uri"

require "rubydex"
require "rubydex/mcp_server/protocol"

Dir[File.join(__dir__, "mcp_server", "tools", "*_tool.rb")].sort.each { |file| require file }

module Rubydex
  module MCPServer
    SERVER_INSTRUCTIONS = <<~TEXT
      Rubydex provides semantic Ruby code intelligence.

      Use these tools for Ruby source files (.rb, .rbi, .rbs) when you need structural information about declarations, locations, hierarchy, references, or codebase composition.

      Use text search instead for literal strings, comments, log messages, non-Ruby files, or content search rather than structural queries.

      Fully qualified name format: "Foo::Bar" for classes/modules/constants, "Foo::Bar#method_name" for instance methods.

      Pagination: tools that may return a high number of results include `total` for pagination. When `total` exceeds the number of returned items, use `offset` to fetch the next page.
    TEXT

    class State
      #: (String) -> void
      def initialize(root_path)
        @root_path = root_path
        @mutex = Mutex.new
        @graph = Graph.new(workspace_path: @root_path)
        @index_finished = false
        @error = nil
      end

      attr_reader :root_path

      #: -> Thread
      def spawn_indexer
        Thread.new do
          graph = @graph
          errors = graph.index_workspace
          errors.each { |error| warn("Indexing error: #{error}") }
          graph.resolve
          warn("Rubydex indexed #{graph.documents.count} files, #{graph.declarations.count} declarations")

          @mutex.synchronize do
            @index_finished = true
          end
        rescue Exception => e # rubocop:disable Lint/RescueException
          warn("Rubydex indexing failed: #{e.message}")
          @mutex.synchronize do
            @error = e.message
          end
        end
      end

      #: -> Graph | Error
      def graph_or_error
        @mutex.synchronize do
          if @error
            return Error.new(
              "indexing_failed",
              "Rubydex indexing failed: #{@error}",
              "Check server logs for details. The MCP server needs to be restarted.",
            )
          end

          return @graph if @index_finished
        end

        Error.new(
          "indexing",
          "Rubydex is still indexing the codebase",
          "The server is starting up. Please retry in a few seconds.",
        )
      end
    end

    class << self
      #: (?String) -> void
      def run(path = ".")
        root = File.realpath(path)
        state = State.new(root)
        state.spawn_indexer

        server = Server.new(server_state: state)

        StdioTransport.new(server).open
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

      #: (String, String) -> String
      def format_path(uri, root_path)
        path = file_path_for_uri(uri)
        return uri unless path

        absolute_path = File.expand_path(path)
        absolute_root = File.expand_path(root_path)
        relative_path = begin
          Pathname.new(absolute_path).relative_path_from(Pathname.new(absolute_root)).to_s
        rescue ArgumentError
          nil
        end
        return absolute_path unless relative_path

        relative_path.start_with?("..") ? absolute_path : relative_path
      end

      #: (String) -> String
      def path_for_uri(uri)
        file_path_for_uri(uri) || uri
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

      #: (Graph, String, String) -> Document?
      def document_for_path(graph, root_path, file_path)
        absolute_target = if Pathname.new(file_path).absolute?
          file_path
        else
          File.join(root_path, file_path)
        end
        canonical_target = File.realpath(absolute_target)
        graph.documents.find do |document|
          path = file_path_for_uri(document.uri)
          path && File.expand_path(path) == canonical_target
        end
      rescue SystemCallError
        absolute_target = File.expand_path(absolute_target)
        graph.documents.find do |document|
          path = file_path_for_uri(document.uri)
          path && File.expand_path(path) == absolute_target
        end
      end

      #: (Location, String) -> Hash
      def display_location(location, root_path)
        display = location.to_display
        {
          path: format_path(display.uri, root_path),
          line: display.start_line,
        }
      end

      #: (Enumerable, Integer?, Integer?, Integer) -> [Array, Integer]
      def paginate(items, offset, limit, max_limit)
        offset = offset.to_i if offset
        offset = 0 unless offset&.positive?
        limit = limit.to_i if limit
        limit = 50 unless limit&.positive?
        limit = [limit, max_limit].min

        page = []
        total = 0

        items.each do |item|
          page << item if total >= offset && page.length < limit
          total += 1
        end

        [page, total]
      end

      #: (Graph, String) -> Declaration | Error
      def lookup_declaration(graph, name)
        declaration = graph[name]
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
