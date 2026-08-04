# frozen_string_literal: true

require "socket"

module Rubydex
  module Server
    # The resident server process. It answers one client at a time, in process, over a UNIX socket.
    class Core
      # Errors that describe one path, and not the health of this process. The walk skips the entry
      # they name and carries on.
      #
      # A resource failure such as `EMFILE`, `ENFILE` or `EIO` is deliberately absent. Swallowing one
      # of those would make the walk skip every entry, and a refresh would then read the empty result
      # as "every file was deleted" and erase the graph. Those errors travel on instead, and the
      # request fails without the manifest moving.
      PATH_ERROR_NAMES = [:ENOENT, :EACCES, :ELOOP, :ENAMETOOLONG, :ENOTDIR].freeze #: Array[Symbol]

      # Resolved by lookup rather than named directly, because the `Errno` constants a build defines
      # are platform-dependent and this file still loads on Windows, where server mode cannot run. A
      # missing constant would otherwise break the load itself. A test pins that every name resolves
      # here, so a typo cannot hide behind the lookup.
      PATH_ERRORS = PATH_ERROR_NAMES.filter_map do |name|
        Errno.const_get(name, false) if Errno.const_defined?(name, false)
      end.freeze #: Array[Class]

      #: (State state, ?lock: File?) -> void
      def initialize(state, lock: nil)
        @state = state
        @lock = lock
        @mutex = Mutex.new
        @running = true
        @started_at = Process.clock_gettime(Process::CLOCK_MONOTONIC)
        @manifest = {} #: Hash[String, Float]
      end

      # Blocks for the lifetime of the server.
      #: -> void
      def run
        redirect_output
        # Recorded before the slow index, so a client can separate a boot crash from a slow start.
        # The socket is the readiness signal, and it appears only when the graph is ready.
        @state.record!

        require "rubydex"
        @graph, boot_errors = Server.build_graph(workspace_path: @state.workspace_path)
        @manifest = initial_manifest(boot_errors)

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

        @mutex.synchronize do
          refresh_if_stale

          # Only the parse and the render answer for user input. An `ArgumentError` from the refresh
          # above is a server fault, and reporting it as a bad query would blame the caller.
          begin
            response(stdout: Rubydex::Query.parse(query).render(@graph, format))
          rescue ArgumentError => e
            response(stderr: "#{e.message}\n", status: 1)
          end
        end
      end

      # Detects workspace files that changed since the graph was built and applies incremental
      # updates before answering. Always correct, occasionally slow (Phase 1 freshness model).
      #: -> void
      def refresh_if_stale
        current, unreadable = workspace_manifest
        previous = @manifest

        # A path the walk could not read this time contributes no entries, and whatever lives under
        # it is still there. Without this they would all look deleted, and one `chmod`, or one moment
        # during a checkout, would erase a whole subtree from the graph.
        hidden = unreadable.empty? ? [] : previous.keys.select { |path| under_any?(path, unreadable) }

        changed = current.select { |path, mtime| previous[path] != mtime }.keys
        deleted = previous.keys - current.keys - hidden
        return if changed.empty? && deleted.empty?

        failed = changed - index(changed)
        deleted.each { |path| @graph.delete_document(uri_for(path)) }
        @graph.resolve

        # Only a file that indexed cleanly becomes fresh. One that failed keeps its previous mtime,
        # so the next walk sees it as changed and tries it again. Recording the new mtime would call
        # a file the server never read "fresh" for the rest of its life. A failed file that is new
        # has no previous mtime, and stays out of the manifest for the same reason.
        @manifest = current.reject { |path, _| failed.include?(path) }
        failed.each { |path| @manifest[path] = previous[path] if previous.key?(path) }
        hidden.each { |path| @manifest[path] = previous[path] }
      end

      # Whether `path` is one of `prefixes` or sits beneath one. Equality matters because a single
      # file can be the thing that could not be read, and not only a directory above it.
      #: (String path, Array[String] prefixes) -> bool
      def under_any?(path, prefixes)
        prefixes.any? { |prefix| path == prefix || path.start_with?("#{prefix}/") }
      end

      # Indexes `paths` and returns the ones that indexed cleanly.
      #
      # `index_all` reports opaque messages for a whole batch, so a failure cannot be attributed to a
      # file. The graph cannot answer it either: a failed update leaves the previous document in
      # place, and mapping a path to the URI a document is stored under is exactly the parity
      # question that Group D still owns.
      #
      # So a failing batch is halved until each failure sits alone. A batch indexes far faster per
      # file than single calls do, which makes this much cheaper than asking file by file. Measured
      # on 1081 files with one unreadable among them: 21 calls in 75ms, against 1081 calls in 311ms.
      #: (Array[String] paths) -> Array[String]
      def index(paths)
        return paths if paths.empty?

        errors = @graph.index_all(paths)
        return paths if errors.empty?

        errors.each { |message| log("rdx server: index error: #{message}") }
        isolate(paths)
      end

      # Halves a batch that failed until every failure is isolated, and returns what indexed cleanly.
      #: (Array[String] paths) -> Array[String]
      def isolate(paths)
        return [] if paths.size <= 1

        middle = paths.size / 2
        [paths[0...middle], paths[middle..] || []].flat_map do |half|
          @graph.index_all(half).empty? ? half : isolate(half)
        end
      end

      # The manifest a fresh server starts from.
      #
      # With no errors, every file the walk found is fresh. With errors, the indexer cannot say which
      # file failed, so the walk's files go through `index` and only those that come back clean are
      # recorded. The rest stay out, and the first request retries them. Recording them from the walk
      # alone would call a file the server never read "fresh" for the rest of its life.
      #: (Array[String] errors) -> Hash[String, Float]
      def initial_manifest(errors)
        files, = workspace_manifest
        return files if errors.empty?

        # Logged before anything else. An unreadable workspace root leaves no files to attribute, and
        # a server that started on an empty graph must still say why.
        errors.each { |message| log("rdx server: boot index error: #{message}") }
        return files if files.empty?

        indexed = index(files.keys).to_h { |path| [path, true] }

        # `build_graph` resolved before this ran, and `index` has replaced documents since, so the
        # graph is resolved again before it serves anything.
        @graph.resolve
        files.select { |path, _| indexed.key?(path) }
      end

      # The indexable files under the workspace, and the directories the walk could not read.
      #: -> [Hash[String, Float], Array[String]]
      def workspace_manifest
        manifest = {} #: Hash[String, Float]
        unreadable = [] #: Array[String]
        # Mirror the indexer's discovery: recurse everything except paths matching the configured
        # exclude globs. The Rust indexer has no ignore-by-name list; it applies these glob patterns
        # to every entry (pruning directories and skipping files alike).
        patterns = @graph.excluded_patterns
        collect_files(@state.workspace_path, manifest, patterns, unreadable, top_level: true)
        [manifest, unreadable]
      end

      # The rescues sit at three levels on purpose:
      #
      # - The inner one isolates a single entry, so a file that vanished mid-walk cannot hide every
      #   entry after it in the same directory.
      # - `EACCES` on the directory itself records it as unreadable. Its files are still there, and
      #   the caller keeps them rather than treating the subtree as deleted.
      # - `ENOENT` on the directory means it really is gone, and so are its files.
      #
      # `each_child` streams. `Dir.children` would materialise every name in the directory, which
      # this project cannot afford on a hyper-scale workspace.
      #: (String dir, Hash[String, Float] manifest, Array[String] patterns, Array[String] unreadable, ?top_level: bool) -> void
      def collect_files(dir, manifest, patterns, unreadable, top_level: false)
        Dir.each_child(dir) do |entry|
          full = File.join(dir, entry)
          next if excluded_by_patterns?(full, patterns)

          begin
            if directory_to_walk?(full, top_level)
              collect_files(full, manifest, patterns, unreadable)
            elsif Rubydex::Graph::INDEXABLE_EXTENSIONS.include?(File.extname(entry))
              manifest[full] = File.mtime(full).to_f
            end
          rescue *PATH_ERRORS => error
            # One entry the platform will not answer for: it vanished mid-walk, it cannot be read, or
            # it is a symlink loop, where `lstat` succeeds but `mtime` follows the link and raises
            # `ELOOP`. Skipping it must not hide the rest of the directory.
            #
            # A permission error is different from the others, because the entry is still there and
            # may be a whole subtree. A directory that is readable but not searchable, mode `0400`,
            # lands here for every child: `each_child` lists the names and each `lstat` is refused.
            # The path is recorded so the caller keeps what it already knew, rather than reading the
            # gap as a deletion and erasing the subtree.
            unreadable << full if error.is_a?(Errno::EACCES)
            next
          end
        end
      rescue Errno::ENOENT
        # The directory is gone, and so are its files.
        nil
      rescue *PATH_ERRORS
        # Present but unreadable right now. Its files are not gone, so the caller keeps their entries
        # instead of erasing the subtree.
        unreadable << dir
      end

      # Whether the walk descends into `path`. The answer differs by depth, because the two sides of
      # discovery do:
      #
      # - `Graph#workspace_paths` asks `File.directory?` about the workspace's own children, so a
      #   symlinked directory there becomes an explicit root, and the Rust walker does traverse an
      #   explicit root (`collect_files_indexes_symlinked_directory_roots`).
      # - Below that the walker asks a `DirEntry` for its type, which never follows a symlink
      #   (`collect_files_does_not_follow_symlinked_directories`).
      #
      # Following at every depth is what let `ln -s .. sub/loop` record one file 32 times under ever
      # longer paths, until the platform refused. Following at neither depth would hide a whole
      # top-level symlinked directory that the indexer does read.
      #: (String path, bool top_level) -> bool
      def directory_to_walk?(path, top_level)
        top_level ? File.directory?(path) : File.lstat(path).directory?
      end

      # Mirrors the Rust indexer's `is_excluded`: an entry is skipped when any exclude glob matches
      # its path. `FNM_PATHNAME` keeps `*` from crossing `/` and enables `**` recursion, matching the
      # `glob` crate's `Pattern::matches_path` semantics.
      #: (String path, Array[String] patterns) -> bool
      def excluded_by_patterns?(path, patterns)
        patterns.any? { |pattern| File.fnmatch?(pattern, path, File::FNM_PATHNAME) }
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
