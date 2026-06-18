# frozen_string_literal: true

require "rubydex/version"

module Rubydex
  # Client/server mode for the `rdx` executable.
  #
  # The expensive indexing + resolution work is performed once by a resident server process that
  # keeps the built `Rubydex::Graph` in memory. Subsequent commands (currently `--query`) run against
  # the already-built graph over a UNIX domain socket, making follow-up queries effectively instant.
  #
  # See `tmp/rdx-server-plan/README.md` for the full design.
  module Server
    # Wire protocol version. Bump on any incompatible change to the request/response shape.
    PROTOCOL = 1

    # How long the client waits for the server's version line / handshake before giving up.
    HANDSHAKE_TIMEOUT = 10.0

    # How long the client waits for a freshly spawned server to become ready (socket to appear).
    BOOT_TIMEOUT = 120.0

    class Error < StandardError; end

    # Raised when a read from the server exceeds its timeout.
    class ServerReadTimeout < Error; end

    class << self
      # Whether server mode can run on the current platform. Requires `fork` + UNIX domain sockets.
      #: -> bool
      def supported?
        Process.respond_to?(:fork) && defined?(::UNIXSocket) && !Gem.win_platform?
      end

      # Whether the user has explicitly disabled the server via the environment.
      #: -> bool
      def disabled?
        ENV.key?("DISABLE_RDX_SERVER")
      end

      # Builds a fully indexed + resolved graph for the workspace. Shared by the inline path and the
      # server boot path. `progress_io`, when given, receives human-readable progress messages.
      #: (workspace_path: String, ?progress_io: IO?) -> Rubydex::Graph
      def build_graph(workspace_path:, progress_io: nil)
        graph = Rubydex::Graph.new(workspace_path: workspace_path)
        with_timer(progress_io, "Indexing workspace...") { graph.index_workspace }
        with_timer(progress_io, "Resolving graph...") { graph.resolve }
        graph
      end

      #: (IO? io, String message) { -> void } -> void
      def with_timer(io, message)
        unless io
          yield
          return
        end

        io.print(message)
        start = Process.clock_gettime(Process::CLOCK_MONOTONIC, :float_millisecond)
        yield
        duration = Process.clock_gettime(Process::CLOCK_MONOTONIC, :float_millisecond) - start
        io.puts(" finished in #{duration.round(2)}ms")
      end
    end
  end
end

require "rubydex/server/cache"
require "rubydex/server/request"
require "rubydex/server/core"
require "rubydex/server/client"
require "rubydex/server/commands"
