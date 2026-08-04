# frozen_string_literal: true

require "socket"

module Rubydex
  module Server
    # The resident server process. It answers one client at a time, in process, over a UNIX socket.
    #
    # The graph is a snapshot from boot. A file edited after that answers with its boot content.
    class Core
      #: (State state, ?lock: File?) -> void
      def initialize(state, lock: nil)
        @state = state
        @lock = lock
        @running = true
        @started_at = Process.clock_gettime(Process::CLOCK_MONOTONIC)
      end

      # Blocks for the lifetime of the server.
      #: -> void
      def run
        redirect_output
        # Recorded before the slow index, so a client can separate a boot crash from a slow start.
        # The socket is the readiness signal, and it appears only when the graph is ready.
        @state.record!

        require "rubydex"
        @graph = Server.build_graph(workspace_path: @state.workspace_path)

        server = open_socket
        log("rdx server ready (pid=#{Process.pid}, workspace=#{@state.workspace_path})")
        serve(server)
      rescue StandardError => e
        # A detached crash reaches nobody, so the log is the only record of it.
        log("rdx server crashed: #{e.class}: #{e.message}")
        e.backtrace&.each { |frame| log("  #{frame}") }
        raise
      ensure
        cleanup(server)
      end

      private

      #: -> UNIXServer
      def open_socket
        File.unlink(@state.socket_path) if File.exist?(@state.socket_path)
        # `sockaddr_un` caps the path near 104 bytes, so the bind uses a short relative name.
        server = Dir.chdir(File.dirname(@state.socket_path)) do
          UNIXServer.new(File.basename(@state.socket_path))
        end
        File.chmod(0o600, @state.socket_path)
        server
      end

      #: (UNIXServer server) -> void
      def serve(server)
        while @running
          client = begin
            server.accept
          rescue IOError, Errno::EBADF
            break
          end

          handle(client)
        end
      end

      #: (UNIXSocket client) -> void
      def handle(client)
        # The first write lets the client detect an old server before it sends a request.
        client.puts(@state.expected_version)

        request = Frame.read_request(client)
        return unless request

        if authorized?(request["token"])
          dispatch(request, client)
        else
          answer(client, response(stderr: "rdx server: unauthorized request\n", status: 1))
        end
      rescue Errno::EPIPE, Errno::ECONNRESET
        nil
      rescue Frame::Malformed, Frame::ReadTimeout => e
        # A peer that breaks the protocol must not stop the server.
        log("rdx server: rejected a request: #{e.class}: #{e.message}")
        answer(client, response(stderr: "rdx server: #{e.message}\n", status: 1))
      rescue StandardError => e
        # One failed connection must not end the accept loop.
        log("rdx server: internal error: #{e.class}: #{e.message}")
        e.backtrace&.first(20)&.each { |frame| log("  #{frame}") }
        answer(client, response(stderr: "rdx server: internal error: #{e.class}: #{e.message}\n", status: 1))
      ensure
        begin
          client.close
        rescue IOError
          nil
        end
      end

      # A client that already closed is not an error here.
      #: (UNIXSocket client, Hash[String, untyped] payload) -> void
      def answer(client, payload)
        Frame.write(client, payload)
      rescue Errno::EPIPE, Errno::ECONNRESET, IOError
        nil
      end

      #: (Hash[untyped, untyped] request, UNIXSocket client) -> void
      def dispatch(request, client)
        command = request["command"]

        payload = case command
        when "query"
          handle_query(request)
        when "status"
          response(stdout: status_report)
        when "stop"
          response
        else
          response(stderr: "rdx server: unknown command #{command.inspect}\n", status: 1)
        end

        Frame.write(client, payload)
        # Set after the write. A failed write leaves the server running, so the caller sees a
        # timeout instead of a server that vanished without an answer.
        @running = false if command == "stop"
      end

      # JSON can carry any type in these two fields. Unchecked, the extension raises `TypeError`,
      # and this server logs the caller's mistake as its own fault.
      #: (Hash[untyped, untyped] request) -> Hash[String, untyped]
      def handle_query(request)
        query = request["query"]
        unless query.is_a?(String)
          return response(stderr: "rdx server: the request carried no query string\n", status: 1)
        end

        # Only an absent key means the default. `|| "table"` would also accept a JSON `false`.
        format = request["query_format"]
        format = "table" if format.nil?
        unless format.is_a?(String)
          return response(stderr: "rdx server: the request carried no query format string\n", status: 1)
        end

        # Only the parse and the render answer for user input.
        begin
          response(stdout: Rubydex::Query.parse(query).render(@graph, format))
        rescue ArgumentError => e
          response(stderr: "#{e.message}\n", status: 1)
        end
      end

      #: (?stdout: String, ?stderr: String, ?status: Integer) -> Hash[String, untyped]
      def response(stdout: "", stderr: "", status: 0)
        { "stdout" => stdout, "stderr" => stderr, "status" => status }
      end

      #: -> String
      def status_report
        uptime = (Process.clock_gettime(Process::CLOCK_MONOTONIC) - @started_at).round(1)
        <<~STATUS
          rdx server running
            pid:       #{Process.pid}
            workspace: #{@state.workspace_path}
            socket:    #{@state.socket_path}
            uptime:    #{uptime}s
            version:   #{@state.expected_version}
        STATUS
      end

      #: (String? candidate) -> bool
      def authorized?(candidate)
        return false unless candidate.is_a?(String)

        expected = @state.token
        return false unless candidate.bytesize == expected.bytesize

        # Constant time, so the comparison leaks no part of the token through its duration.
        candidate.bytes.zip(expected.bytes).reduce(0) { |acc, (a, b)| acc | (a ^ b) }.zero?
      end

      #: -> void
      def redirect_output
        override = ENV["RDX_SERVER_LOG"]
        target = override && !override.empty? ? override : @state.log_path
        $stdout.reopen(target, "a")
        $stderr.reopen(target, "a")
        $stdout.sync = true
        $stderr.sync = true
      end

      #: (String message) -> void
      def log(message)
        $stdout.puts("[#{Time.now.iso8601}] #{message}")
      rescue StandardError
        nil
      end

      #: (UNIXServer? server) -> void
      def cleanup(server)
        begin
          server&.close
        rescue IOError
          nil
        end
        # The server holds the lock, so it removes its own socket without taking the lock again.
        @state.remove_socket!
        @lock&.flock(File::LOCK_UN)
        @lock&.close
      rescue StandardError
        nil
      end
    end
  end
end

require "time"
