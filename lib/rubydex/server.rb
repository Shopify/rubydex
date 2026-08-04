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

      #: (workspace_path: String, ?progress_io: IO?) -> Rubydex::Graph
      def build_graph(workspace_path:, progress_io: nil)
        # The server boot must build the same graph as the inline CLI path.
        graph = Rubydex::Graph.configure_for_workspace(workspace_path)
        Progress.with_timer(progress_io, "Indexing workspace...") { graph.index_workspace }
        Progress.with_timer(progress_io, "Resolving graph...") { graph.resolve }
        graph
      end
    end
  end
end

require "rubydex/server/state"
require "rubydex/server/frame"
require "rubydex/server/core"
require "rubydex/server/client"
require "rubydex/server/commands"
