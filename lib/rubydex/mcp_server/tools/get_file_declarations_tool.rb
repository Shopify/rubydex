# frozen_string_literal: true

module Rubydex
  module MCPServer
    class GetFileDeclarationsTool < Tool
      tool_name "get_file_declarations"
      description "List all Ruby classes, modules, methods, and constants defined in a specific file. Returns a structural overview with names, kinds, and line numbers. Use this to understand a file's structure before reading it, or to see what a file contributes to the codebase. Accepts relative or absolute paths."
      input_schema(
        properties: {
          file_path: { type: "string", description: "File path (relative or absolute) to list declarations for" },
        },
        required: ["file_path"],
      )

      class << self
        #: (file_path: String, server_state: State) -> Tool::Response
        def call(file_path:, server_state:)
          graph = server_state.graph_or_error

          case graph
          when Error
            MCPServer.response(graph)
          else
            root_path = server_state.root_path
            absolute_target = if Pathname.new(file_path).absolute?
              file_path
            else
              File.join(root_path, file_path)
            end
            return file_not_found_response(file_path) unless File.exist?(absolute_target)

            document = MCPServer.document_for_path(graph, root_path, file_path)
            return file_not_found_response(file_path) unless document

            declarations = document.definitions.filter_map do |definition|
              declaration = definition.declaration
              next unless declaration

              {
                name: declaration.name,
                kind: MCPServer.declaration_kind(declaration),
                line: definition.location.to_display.start_line,
              }
            end

            MCPServer.response(file: MCPServer.format_path(document.uri, root_path), declarations: declarations)
          end
        end

        private

        #: (String) -> Tool::Response
        def file_not_found_response(file_path)
          MCPServer.response(
            Error.new(
              "not_found",
              "File '#{file_path}' not found in the index",
              "Use a relative path like 'app/models/user.rb' or an absolute path matching the indexed project",
            ),
          )
        end
      end
    end
  end
end
