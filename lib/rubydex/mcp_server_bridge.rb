# frozen_string_literal: true

require "rbconfig"

module Rubydex
  # Launcher-only bridge for `exe/rubydex_mcp`.
  #
  # Do not require this file from `lib/rubydex.rb`. The main Ruby API owns graph construction through
  # `Rubydex::Graph`; this file exists only so the executable wrapper can inspect Bundler before it execs the Rust MCP
  # server. The Rust server still performs the indexing pass.
  module MCPServerBridge
    PASSTHROUGH_ARGUMENTS = ["-h", "--help", "-V", "--version"].freeze

    extend self

    def executable_name
      host_os = RbConfig::CONFIG.fetch("host_os")
      host_os.match?(/mswin|mingw|cygwin/) ? "rubydex_mcp.exe" : "rubydex_mcp"
    end

    def binary_path
      File.expand_path("bin/#{executable_name}", __dir__)
    end

    def passthrough?(argv)
      argv.any? { |arg| PASSTHROUGH_ARGUMENTS.include?(arg) }
    end

    # Activates the workspace's Bundler environment so dependency gems can be discovered.
    #
    # Side effect: sets ENV["BUNDLE_GEMFILE"] (only if unset) to the workspace Gemfile, since
    # `bundler/setup` resolves against that variable. This permanently mutates process env — acceptable
    # in the launcher, which execs the Rust binary immediately afterwards, but callers in long-lived
    # processes (e.g. tests) should save and restore ENV["BUNDLE_GEMFILE"] themselves.
    def setup_bundler_context(workspace_path)
      root = File.expand_path(workspace_path)
      root = File.dirname(root) if File.file?(root)

      gemfile = File.join(root, "Gemfile")
      ENV["BUNDLE_GEMFILE"] ||= gemfile if File.file?(gemfile)

      require "bundler/setup"
      require "bundler"

      true
    rescue LoadError, Bundler::BundlerError => e
      warn("Warning: failed to load Bundler context: #{e.message}")
      false
    end

    # Builds the full set of index roots: the workspace path(s) from argv plus every Bundler
    # dependency require_path.
    #
    # Contract note: only the first argv entry drives the Bundler context (see the call to
    # `setup_bundler_context(ARGV.first || Dir.pwd)` in `exe/rubydex_mcp`), but ALL argv entries
    # become index roots here. So dependency paths are resolved relative to the first path's
    # Gemfile, while every requested path is still indexed.
    def compute_index_paths(argv)
      paths = argv.empty? ? [Dir.pwd] : argv
      paths = paths.map { |path| File.expand_path(path) }

      paths.concat(dependency_index_paths)
      paths.uniq
    end

    def dependency_index_paths
      Bundler.load.specs.flat_map do |spec|
        spec.require_paths.filter_map do |path|
          next if File.absolute_path?(path)

          full_path = File.join(spec.full_gem_path, path)
          full_path if File.directory?(full_path)
        end
      end.uniq
    rescue StandardError => e
      warn("Warning: failed to collect Bundler dependency paths: #{e.message}")
      []
    end
  end
end
