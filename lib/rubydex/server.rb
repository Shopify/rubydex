# frozen_string_literal: true

require "rubydex/progress"
require "rubydex/version"

module Rubydex
  # Client/server mode for the `rdx` executable.
  #
  # A resident server process indexes and resolves the workspace once, and keeps the graph in
  # memory. Later commands reach it over a UNIX domain socket and skip that work.
  module Server
    # Increase this after any incompatible change to the request or response shape.
    PROTOCOL = 1

    class Error < StandardError; end

    class << self
      # Without `O_NOFOLLOW`, a symlink can redirect the permissions this code sets on its runtime
      # directory.
      #: -> bool
      def supported?
        Process.respond_to?(:fork) && defined?(::UNIXSocket) && !Gem.win_platform? &&
          !State::NOFOLLOW.nil?
      end

      # Builds a fully indexed + resolved graph for the workspace, and returns it with the errors the
      # indexer reported. `progress_io`, when given, receives human-readable progress messages.
      #
      # A caller that discards the errors records a file the indexer never read as successfully
      # indexed, which is why they come back rather than vanishing here.
      #: (workspace_path: String, ?progress_io: IO?) -> [Rubydex::Graph, Array[String]]
      def build_graph(workspace_path:, progress_io: nil)
        # The server boot must build the same graph as the inline CLI path.
        graph = Rubydex::Graph.configure_for_workspace(workspace_path)

        # `workspace_paths` lists every root to index, and it names gem directories that this install
        # may not have. Each absent root costs one error, and those phantom errors would drown the
        # ones that concern real files, so they never reach the indexer.
        roots = graph.workspace_paths.select { |path| File.exist?(path) }

        errors = [] #: Array[String]
        Progress.with_timer(progress_io, "Indexing workspace...") { errors = graph.index_all(roots) }
        Progress.with_timer(progress_io, "Resolving graph...") { graph.resolve }
        [graph, errors]
      end
    end
  end
end

require "rubydex/server/state"
require "rubydex/server/frame"
require "rubydex/server/core"
require "rubydex/server/client"
require "rubydex/server/commands"
