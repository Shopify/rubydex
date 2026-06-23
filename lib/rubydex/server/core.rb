# frozen_string_literal: true

require "socket"

module Rubydex
  module Server
    # The resident server process. Builds the graph for one workspace, then serves requests over a
    # UNIX domain socket. Read-only `query` requests are answered in-process; there is no fork per
    # request (Phase 1).
    class Core
      #: (Cache cache, ?lock: File?, ?detach: bool) -> void
      def initialize(cache, lock: nil, detach: true)
        @cache = cache
        @lock = lock
        @detach = detach
        @mutex = Mutex.new
        @running = true
        @started_at = Process.clock_gettime(Process::CLOCK_MONOTONIC)
        @manifest = {} #: Hash[String, Float]
      end

      # Boots the graph, writes metadata and serves until stopped. Blocks for the lifetime of the
      # server.
      #: -> void
      def run
        require "rubydex"

        redirect_output if @detach
        @graph = Server.build_graph(workspace_path: @cache.workspace_path)
        @manifest = workspace_manifest
        @cache.token # ensure the token file exists before we accept connections
        @cache.write_metadata!

        server = open_socket
        log("rdx server ready (pid=#{Process.pid}, workspace=#{@cache.workspace_path})")
        serve(server)
      ensure
        cleanup(server)
      end

      private

      #: -> UNIXServer
      def open_socket
        File.unlink(@cache.socket_path) if File.exist?(@cache.socket_path)
        # `sockaddr_un` caps the path at ~104 bytes, so bind using a short relative name from inside
        # the socket's directory. The cwd is restored when the block returns.
        server = Dir.chdir(File.dirname(@cache.socket_path)) do
          UNIXServer.new(File.basename(@cache.socket_path))
        end
        File.chmod(0o600, @cache.socket_path)
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
        # Version gate: the server's first write lets the client detect an incompatible server and
        # restart it before sending any request.
        client.puts(@cache.expected_version)

        request = Request.read(client)
        return unless request

        unless authorized?(request["token"])
          Request.write(client, response(stderr: "rdx server: unauthorized request\n", status: 1))
          return
        end

        dispatch(request, client)
      rescue Errno::EPIPE, Errno::ECONNRESET
        nil
      ensure
        begin
          client.close
        rescue IOError
          nil
        end
      end

      #: (Hash[untyped, untyped] request, UNIXSocket client) -> void
      def dispatch(request, client)
        case request["command"]
        when "query"
          Request.write(client, handle_query(request))
        when "status"
          Request.write(client, response(stdout: status_report))
        when "stop"
          Request.write(client, response(stdout: "rdx server stopped\n"))
          @running = false
        else
          Request.write(client, response(stderr: "rdx server: unknown command #{request["command"].inspect}\n", status: 1))
        end
      end

      #: (Hash[untyped, untyped] request) -> Hash[String, untyped]
      def handle_query(request)
        query = request["query"]
        format = request["query_format"] || "table"

        @mutex.synchronize do
          refresh_if_stale
          response(stdout: @graph.query(query, format))
        end
      rescue ArgumentError => e
        response(stderr: "#{e.message}\n", status: 1)
      end

      # Detects workspace files that changed since the graph was built and applies incremental
      # updates before answering. Always correct, occasionally slow (Phase 1 freshness model).
      #: -> void
      def refresh_if_stale
        current = workspace_manifest
        changed = current.select { |path, mtime| @manifest[path] != mtime }.keys
        deleted = @manifest.keys - current.keys
        return if changed.empty? && deleted.empty?

        @graph.index_all(changed) unless changed.empty?
        deleted.each { |path| @graph.delete_document(uri_for(path)) }
        @graph.resolve
        @manifest = current
      end

      #: -> Hash[String, Float]
      def workspace_manifest
        manifest = {}
        collect_files(@cache.workspace_path, manifest)
        manifest
      end

      #: (String dir, Hash[String, Float] manifest) -> void
      def collect_files(dir, manifest)
        Dir.each_child(dir) do |entry|
          full = File.join(dir, entry)

          if File.directory?(full)
            next if Rubydex::Graph::IGNORED_DIRECTORIES.include?(entry)

            collect_files(full, manifest)
          elsif Rubydex::Graph::INDEXABLE_EXTENSIONS.include?(File.extname(entry))
            manifest[full] = File.mtime(full).to_f
          end
        end
      rescue Errno::ENOENT, Errno::EACCES
        nil
      end

      #: (String path) -> String
      def uri_for(path)
        path = "/#{path}" if Gem.win_platform?
        URI::File.build(path: path).to_s
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
            workspace: #{@cache.workspace_path}
            socket:    #{@cache.socket_path}
            uptime:    #{uptime}s
            version:   #{@cache.expected_version}
        STATUS
      end

      #: (String? candidate) -> bool
      def authorized?(candidate)
        return false unless candidate.is_a?(String)

        expected = @cache.token
        return false unless candidate.bytesize == expected.bytesize

        # Constant-time comparison to avoid leaking the token to other local users via timing.
        candidate.bytes.zip(expected.bytes).reduce(0) { |acc, (a, b)| acc | (a ^ b) }.zero?
      end

      #: -> void
      def redirect_output
        log_path = ENV["RDX_SERVER_LOG"]
        target = log_path && !log_path.empty? ? log_path : File::NULL
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
        @cache.clean!
        @lock&.flock(File::LOCK_UN)
        @lock&.close
      rescue StandardError
        nil
      end
    end
  end
end

require "time"
