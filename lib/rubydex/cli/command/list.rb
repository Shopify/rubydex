# frozen_string_literal: true

require "pathname"
require "uri"
require "rubydex/cli/command"

module Rubydex
  module CLI
    # `rdx list [docs|roots] [PATH]` — prints the files or roots used to index a workspace.
    class Command
      class List < Command
        command "list"
        arguments "[docs|roots] [PATH]"
        summary "Print indexed documents or graph roots"

        #: -> void
        def run
          parse_options!

          kind = argv.shift || "docs"
          workspace_path = File.expand_path(argv.shift || Dir.pwd)
          abort_with_usage("unexpected argument: #{argv.first}") unless argv.empty?
          abort_with_usage("workspace is not a directory: #{workspace_path}") unless File.directory?(workspace_path)

          config = Rubydex::Config.load(workspace_path)
          graph = Rubydex::Graph.new
          graph.load_config(config)

          case kind
          when "docs"
            graph.index_workspace

            graph.documents
              .map { |document| display_path_for_uri(document.uri, workspace_path:) }
              .sort
              .each { |path| puts(path) }
          when "roots"
            (graph.workspace_paths - graph.excluded_patterns)
              .map { |path| display_path_for_path(path, workspace_path:) }
              .sort
              .each { |path| puts(path) }
          else
            abort_with_usage("Unknown list target: #{kind}. Expected `docs` or `roots`.")
          end
        end

        private

        #: (String uri, workspace_path: String) -> String
        def display_path_for_uri(uri, workspace_path:)
          parsed_uri = URI(uri)
          path = parsed_uri.path
          return uri unless parsed_uri.scheme == "file" && path

          path.delete_prefix!("/") if Gem.win_platform?
          display_path_for_path(path, workspace_path:)
        end

        #: (String path, workspace_path: String) -> String
        def display_path_for_path(path, workspace_path:)
          relative_path = Pathname.new(path).relative_path_from(Pathname.new(workspace_path)).to_s
          relative_path.start_with?("../") ? path : relative_path
        end
      end
    end
  end
end
