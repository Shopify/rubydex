# frozen_string_literal: true

require "digest"
require "fileutils"
require "json"
require "securerandom"
require "time"
require "tmpdir"

module Rubydex
  module Server
    # Owns the per-workspace runtime directory and the three files in it: `lock`, `state.json` and
    # `socket`. An "app id" keys the directory, so a new gem, Ruby or protocol forces a new server.
    #
    # `lock` and `state.json` stay separate files, for two reasons:
    #
    # - On Windows `flock` becomes `LockFileEx`, which denies every other process read access to the
    #   locked range. Data inside `lock` would be unreadable exactly while a server runs.
    # - A lock belongs to an inode, so nothing deletes `lock`. A second process would otherwise
    #   create a new file, lock that, and start a second server for this workspace.
    #
    # A held lock proves that a live process owns this workspace. The recorded pid is a display
    # value, and never a signal target.
    class State
      # A platform without the flag gets `nil`, and every caller refuses. A path-based fallback
      # would follow a planted symlink. `Server.supported?` reports the same fact before a boot.
      NOFOLLOW = File.const_defined?(:NOFOLLOW) ? File::NOFOLLOW : nil #: Integer?

      #: String
      attr_reader :workspace_path

      #: (?workspace_path: String) -> void
      def initialize(workspace_path: Dir.pwd)
        @workspace_path = File.expand_path(workspace_path)
      end

      #: -> String
      def app_id
        @app_id ||= Digest::SHA256.hexdigest(
          [@workspace_path, PROTOCOL, RUBY_VERSION, Rubydex::VERSION, ext_fingerprint].join("\0"),
        )[0, 16]
      end

      # A mismatch makes the client restart the server.
      #: -> String
      def expected_version
        "#{Rubydex::VERSION}:#{ext_fingerprint}"
      end

      #: -> String
      def dir
        @dir ||= File.join(runtime_root, app_id)
      end

      #: -> String
      def socket_path
        File.join(dir, "socket")
      end

      #: -> String
      def lock_path
        File.join(dir, "lock")
      end

      #: -> String
      def state_path
        File.join(dir, "state.json")
      end

      # Where a detached server writes its output, so a boot crash leaves a backtrace.
      #: -> String
      def log_path
        File.join(dir, "server.log")
      end

      # The base keeps its own permissions, because the user owns it. This validates it, and refuses
      # an unsafe one, instead of a chmod that could break other files there.
      #: -> void
      def ensure_dir!
        validate_base!
        ensure_private_dir(runtime_root)
        ensure_private_dir(dir)
      end

      # The caller takes `LOCK_EX | LOCK_NB` and keeps the handle. Nothing writes through it.
      #: -> File
      def open_lock
        ensure_dir!
        refusing_symlink(lock_path) do
          File.open(lock_path, File::RDWR | File::CREAT | nofollow!, 0o600)
        end
      end

      # A fresh token per boot stops a client that cached the token of a previous server.
      #: -> void
      def record!
        ensure_dir!
        @token = SecureRandom.hex(32)
        write_state(
          "pid" => Process.pid,
          "token" => @token,
          "version" => expected_version,
          "started_at" => Time.now.iso8601,
        )
      end

      #: -> Hash[String, untyped]?
      def read
        return unless runtime_trusted?

        payload = JSON.parse(File.read(state_path))
        payload.is_a?(Hash) ? payload : nil
      rescue Errno::ENOENT, JSON::ParserError
        nil
      end

      # The server knows its own token. A client reads it from the record.
      #: -> String?
      def token
        @token || recorded("token")
      end

      #: -> Integer?
      def server_pid
        pid = recorded("pid")
        pid.is_a?(Integer) && pid.positive? ? pid : nil
      end

      #: -> String?
      def started_at
        value = recorded("started_at")
        value.is_a?(String) ? value : nil
      end

      #: -> bool
      def version_compatible?
        recorded("version") == expected_version
      end

      # The lock answers this, and not the recorded pid: the kernel frees a lock when its holder
      # dies, so a recycled pid can never look alive.
      #: -> bool
      def server_running?
        return false unless runtime_trusted?

        refusing_symlink(lock_path) do
          File.open(lock_path, File::RDWR | nofollow!) do |file|
            if file.flock(File::LOCK_EX | File::LOCK_NB)
              file.flock(File::LOCK_UN)
              false
            else
              true
            end
          end
        end
      rescue Errno::ENOENT
        false
      end

      # Takes the lock first, so a live or a starting server keeps its socket.
      #: -> void
      def clean!
        return unless runtime_trusted?

        File.open(lock_path, File::RDWR | nofollow!) do |file|
          next unless file.flock(File::LOCK_EX | File::LOCK_NB)

          begin
            remove_socket!
          ensure
            file.flock(File::LOCK_UN)
          end
        end
      rescue Errno::ENOENT
        nil
      end

      # Only the server may call this, because it already holds the lock.
      #: -> void
      def remove_socket!
        File.unlink(socket_path)
      rescue Errno::ENOENT
        nil
      end

      private

      #: (String key) -> untyped
      def recorded(key)
        record = read
        record && record[key]
      end

      # A symlink in place of a runtime file means somebody put it there, so the error names that.
      #: [T] (String path) { -> T } -> T
      def refusing_symlink(path)
        yield
      rescue Errno::ELOOP
        raise Error, "#{path} is a symlink, and the server runtime files must be real files"
      end

      # Every open below is a security control, so a platform without the flag stops here.
      #: -> Integer
      def nofollow!
        NOFOLLOW || raise(Error, "this platform cannot open a path without following symlinks")
      end

      # `O_NOFOLLOW` protects only the component it opens, and Ruby has no `openat`, so every later
      # path-based open depends on a trustworthy base.
      #
      # Trustworthy means owned by this user or root, and either closed to other writers or sticky.
      # A sticky bit stops another user from a rename. `/tmp` passes; `chmod 777 /shared` does not.
      # This runs once per instance, because the canonical chain cannot change during one command.
      #: -> void
      def validate_base!
        return if @base_validated

        ensure_safe_base(base_dir)
        @base_validated = true
      end

      #: (String path) -> void
      def ensure_safe_base(path)
        current = File.expand_path(path)

        loop do
          check_base_component(current)

          parent = File.dirname(current)
          break if parent == current

          current = parent
        end
      end

      # Every component exists, because `realpath` resolved the base before this walk.
      #: (String path) -> void
      def check_base_component(path)
        stat = begin
          File.stat(path)
        rescue SystemCallError => error
          raise Error, "#{path} cannot be checked: #{error.message}"
        end

        raise Error, "#{path} is not a directory" unless stat.directory?

        if Process.respond_to?(:uid) && !stat.uid.zero? && stat.uid != Process.uid
          raise Error, "#{path} belongs to another user, so it cannot hold the server runtime directory"
        end

        if (stat.mode & 0o022) != 0 && (stat.mode & 0o1000) == 0
          raise Error, "#{path} is writable by other users and is not sticky, so the server runtime " \
            "directory would not be safe inside it"
        end
      end

      # `path` can survive from an earlier run, so this validates it. A path-based `chmod` would
      # follow a planted symlink, and a swap could beat it, so the work goes through one descriptor.
      #: (String path) -> void
      def ensure_private_dir(path)
        FileUtils.mkdir_p(path, mode: 0o700)

        with_private_dir(path) do |directory, stat|
          directory.chmod(0o700) unless (stat.mode & 0o777) == 0o700
        end
      end

      # `server_running?`, `clean!` and `read` run before `ensure_dir!`, and `O_NOFOLLOW` guards only
      # their last component. A runtime root owned by another user would choose the socket and the
      # token the client trusts, so they check the directories first, and create nothing.
      #
      # An absent directory answers `false`. One that exists but is not ours raises, because that
      # is tampering.
      #: -> bool
      def runtime_trusted?
        validate_base!
        trusted_dir?(runtime_root) && trusted_dir?(dir)
      end

      #: (String path) -> bool
      def trusted_dir?(path)
        with_private_dir(path) do |_directory, stat|
          raise Error, "#{path} is not private to this user" unless (stat.mode & 0o777) == 0o700
        end

        true
      rescue Errno::ENOENT, Errno::ENOTDIR
        false
      end

      # Yields the stat, so one caller corrects the mode and the other insists on it.
      #: [T] (String path) { (File directory, File::Stat stat) -> T } -> T
      def with_private_dir(path)
        refusing_symlink(path) do
          File.open(path, File::RDONLY | nofollow!) do |directory|
            stat = directory.stat
            raise Error, "#{path} is not a directory" unless stat.directory?

            if Process.respond_to?(:uid) && stat.uid != Process.uid
              raise Error, "#{path} belongs to another user"
            end

            yield(directory, stat)
          end
        end
      end

      # The temporary file shares the directory, so the rename stays on one filesystem and is atomic.
      #: (Hash[String, untyped] payload) -> void
      def write_state(payload)
        temp = "#{state_path}.#{Process.pid}"
        refusing_symlink(temp) do
          File.open(temp, File::WRONLY | File::CREAT | File::TRUNC | nofollow!, 0o600) do |file|
            file.write(JSON.dump(payload))
            file.flush
            file.fsync
          end
        end
        File.rename(temp, state_path)
      rescue StandardError
        begin
          File.unlink(temp)
        rescue Errno::ENOENT
          nil
        end
        raise
      end

      # One subdirectory per workspace lives here. It nests inside `base_dir`, which this never
      # modifies.
      #: -> String
      def runtime_root
        File.join(base_dir, "rubydex-#{uid}")
      end

      # The base is canonical, and it must already exist.
      #
      # Every runtime path derives from this string, so it holds no symlink component. The owner of a
      # link entry can swap it even when its target is safe, and a `stat` would approve the target.
      # A missing base is refused, because its creation would race the other writers in its parent.
      #: -> String
      def base_dir
        @base_dir ||= begin
          configured = configured_base

          begin
            File.realpath(configured)
          rescue SystemCallError => error
            raise Error, "#{configured} cannot hold the server runtime directory: #{error.message}"
          end
        end
      end

      #: -> String
      def configured_base
        override = ENV["RDX_SERVER_DIR"]
        return File.expand_path(override) if override && !override.empty?

        xdg = ENV["XDG_RUNTIME_DIR"]
        xdg && !xdg.empty? ? xdg : Dir.tmpdir
      end

      #: -> (Integer | String)
      def uid
        Process.respond_to?(:uid) ? Process.uid : "nobody"
      end

      # A recompiled extension invalidates a running server, because Ruby cannot reload it in place.
      #: -> String
      def ext_fingerprint
        @ext_fingerprint ||= begin
          lib_dir = File.expand_path("../..", __dir__)
          artifacts = Dir.glob(File.join(lib_dir, "**", "rubydex.{bundle,so}")) +
            Dir.glob(File.join(lib_dir, "librubydex_sys.*"))

          if artifacts.empty?
            "noext"
          else
            parts = artifacts.sort.map do |path|
              stat = File.stat(path)
              "#{File.basename(path)}:#{stat.size}:#{stat.mtime.to_i}"
            end
            Digest::SHA256.hexdigest(parts.join("|"))[0, 16]
          end
        end
      end
    end
  end
end
