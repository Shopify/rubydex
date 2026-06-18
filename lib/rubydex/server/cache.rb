# frozen_string_literal: true

require "digest"
require "fileutils"
require "tmpdir"

module Rubydex
  module Server
    # Owns the per-workspace runtime directory and the metadata files inside it (socket, pid, token,
    # version, lock).
    #
    # The directory is keyed by an "app id" derived from the workspace path, the Ruby version, the
    # rubydex version and a fingerprint of the loaded native extension. A gem upgrade (or a different
    # Ruby) therefore maps to a different directory and forces a fresh server, since the loaded C
    # extension cannot be hot-swapped.
    class Cache
      #: String
      attr_reader :workspace_path

      #: (?workspace_path: String) -> void
      def initialize(workspace_path: Dir.pwd)
        @workspace_path = File.expand_path(workspace_path)
      end

      # Stable identity for this workspace's server.
      #: -> String
      def app_id
        @app_id ||= Digest::SHA256.hexdigest(
          [@workspace_path, RUBY_VERSION, Rubydex::VERSION, ext_fingerprint].join("\0"),
        )[0, 16]
      end

      # Compatibility string written to the `version` file and sent as the server's version line.
      # A mismatch forces the client to restart the server.
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
      def pid_path
        File.join(dir, "pid")
      end

      #: -> String
      def token_path
        File.join(dir, "token")
      end

      #: -> String
      def version_path
        File.join(dir, "version")
      end

      #: -> String
      def lock_path
        File.join(dir, "lock")
      end

      # Default destination for the detached server's stdout/stderr, so a boot crash leaves a
      # discoverable backtrace instead of vanishing into /dev/null. Overridable via `RDX_SERVER_LOG`.
      #: -> String
      def log_path
        File.join(dir, "server.log")
      end

      # Creates the runtime directory (and its parent) with restrictive permissions.
      #: -> void
      def ensure_dir!
        FileUtils.mkdir_p(runtime_root, mode: 0o700)
        FileUtils.mkdir_p(dir, mode: 0o700)
        # mkdir_p won't tighten perms on a pre-existing dir; enforce them explicitly.
        File.chmod(0o700, runtime_root)
        File.chmod(0o700, dir)
      end

      # Returns the existing token, generating and persisting one if absent.
      #: -> String
      def token
        return @token if @token

        @token = if File.exist?(token_path)
          File.read(token_path).chomp
        else
          generate_token
        end
      end

      #: -> String
      def generate_token
        ensure_dir!
        value = SecureRandom.hex(32)
        File.write(token_path, value)
        File.chmod(0o600, token_path)
        @token = value
      end

      # Records the daemon's pid before the (slow) boot work, so the parent can detect a boot crash
      # and fail fast instead of waiting out the full boot timeout.
      #: -> void
      def write_pid!
        ensure_dir!
        File.write(pid_path, Process.pid.to_s)
      end

      #: -> void
      def write_metadata!
        ensure_dir!
        File.write(pid_path, Process.pid.to_s)
        File.write(version_path, expected_version)
      end

      # Whether the on-disk version file is compatible with the running gem/extension.
      #: -> bool
      def version_compatible?
        File.exist?(version_path) && File.read(version_path).chomp == expected_version
      end

      #: -> Integer?
      def server_pid
        return unless File.exist?(pid_path)

        pid = File.read(pid_path).to_i
        pid.zero? ? nil : pid
      end

      # Whether a process with the recorded pid is alive.
      #: -> bool
      def server_alive?
        pid = server_pid
        return false unless pid

        Process.kill(0, pid)
        true
      rescue Errno::ESRCH, Errno::EPERM
        false
      end

      # Removes stale artifacts left behind by a dead server.
      #: -> void
      def clean!
        [socket_path, pid_path, version_path].each do |path|
          File.unlink(path)
        rescue Errno::ENOENT
          nil
        end
      end

      private

      #: -> String
      def runtime_root
        base = ENV["RDX_SERVER_DIR"]
        return File.expand_path(base) if base && !base.empty?

        xdg = ENV["XDG_RUNTIME_DIR"]
        parent = xdg && !xdg.empty? ? xdg : Dir.tmpdir
        File.join(parent, "rubydex-#{uid}")
      end

      #: -> (Integer | String)
      def uid
        Process.respond_to?(:uid) ? Process.uid : "nobody"
      end

      # Fingerprint of the native extension artifacts so a recompiled/upgraded extension invalidates
      # any running server (the C extension cannot be reloaded in place).
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

require "securerandom"
