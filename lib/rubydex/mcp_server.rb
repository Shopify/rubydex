# frozen_string_literal: true

require "json"
require "pathname"
require "uri"

require "rubydex"
require "rubydex/mcp_server/protocol"
require "rubydex/mcp_server/tools/codebase_stats_tool"
require "rubydex/mcp_server/tools/find_constant_references_tool"
require "rubydex/mcp_server/tools/get_declaration_tool"
require "rubydex/mcp_server/tools/get_descendants_tool"
require "rubydex/mcp_server/tools/get_file_declarations_tool"
require "rubydex/mcp_server/tools/search_declarations_tool"

module Rubydex
  module MCPServer
    SERVER_INSTRUCTIONS = <<~TEXT
      Rubydex provides semantic Ruby code intelligence.

      ONLY use these tools for Ruby files (.rb, .rbi, .rbs) -- never for Rust, JavaScript, or other languages.

      Use these tools INSTEAD OF Grep when working with Ruby code structure.

      Decision guide:
      - Know a name? -> search_declarations (fuzzy search by name)
      - Have an exact fully qualified name? -> get_declaration (full details with docs, ancestors, members)
      - Need reverse hierarchy? -> get_descendants (what inherits from this class/module)
      - Refactoring a class/module/constant? -> find_constant_references (all precise usages across codebase)
      - Exploring a file? -> get_file_declarations (structural overview)
      - Want general statistics? -> codebase_stats (size and composition)

      Typical workflow: search_declarations -> get_declaration -> find_constant_references.

      Fully qualified name format: "Foo::Bar" for classes/modules/constants, "Foo::Bar#method_name" for instance methods.

      Pagination: tools that may return a high number of results include `total` for pagination. When `total` exceeds the number of returned items, use `offset` to fetch the next page.

      Use Grep instead for: literal string search, log messages, comments, non-Ruby files, or content search rather than structural queries.
    TEXT
    TOOLS = [
      SearchDeclarationsTool,
      GetDeclarationTool,
      GetDescendantsTool,
      FindConstantReferencesTool,
      GetFileDeclarationsTool,
      CodebaseStatsTool,
    ].freeze

    class Error
      #: (String, ?String, ?String) -> void
      def initialize(error, message = nil, suggestion = nil)
        @error = error
        @message = message
        @suggestion = suggestion
      end

      #: (*untyped) -> String
      def to_json(*args)
        payload = { error: @error }
        payload[:message] = @message if @message
        payload[:suggestion] = @suggestion if @suggestion
        payload.to_json(*args)
      end
    end

    class State
      #: (String) -> void
      def initialize(root_path)
        @root_path = root_path
        @mutex = Mutex.new
        @graph = nil
        @error = nil
      end

      attr_reader :root_path

      #: -> Thread
      def spawn_indexer
        Thread.new do
          graph = Graph.new(workspace_path: @root_path)
          errors = graph.index_workspace
          errors.each { |error| warn("Indexing error: #{error}") }
          graph.resolve
          warn("Rubydex indexed #{graph.documents.count} files, #{graph.declarations.count} declarations")

          @mutex.synchronize do
            @graph = graph
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

          return @graph if @graph
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
