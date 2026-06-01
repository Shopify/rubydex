# frozen_string_literal: true

require "rbconfig"

module Rubydex
  # Launcher-only helpers for `exe/rubydex_mcp`.
  #
  # Do not require this file from `lib/rubydex.rb`. It exists only so the Ruby executable wrapper can handle
  # host-specific process launch details before it execs the Rust MCP server.
  module MCPServerLauncher
    extend self

    def windows?
      RbConfig::CONFIG.fetch("host_os").match?(/mswin|mingw|cygwin/)
    end

    def executable_name
      windows? ? "rubydex_mcp.exe" : "rubydex_mcp"
    end

    def binary_path
      File.expand_path("bin/#{executable_name}", __dir__)
    end

    def host_loader(maps_path)
      File.foreach(maps_path) do |line|
        path = line.split.last
        next unless path && File.absolute_path?(path)
        next unless File.basename(path).match?(/\Ald-.*\.so/)

        return path if File.exist?(path)
      end

      nil
    rescue Errno::ENOENT
      nil
    end

    def exec_server(argv)
      binary = binary_path

      unless File.executable?(binary)
        abort(<<~MESSAGE.chomp)
          rubydex_mcp is not available at #{binary}.
          Install a precompiled rubydex gem, or reinstall rubydex with Cargo available so the MCP executable can be built locally.
        MESSAGE
      end

      exec(binary, *argv)
    rescue Errno::ENOENT
      loader = host_loader("/proc/self/maps")
      raise unless loader

      exec(loader, binary, *argv)
    end
  end
end
