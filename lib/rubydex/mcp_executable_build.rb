# frozen_string_literal: true

require "rbconfig"
require "shellwords"

module Rubydex
  class MCPExecutableBuild
    LINUX_MUSL_TARGETS = {
      "x86_64" => "x86_64-unknown-linux-musl",
      "aarch64" => "aarch64-unknown-linux-musl",
      "arm64" => "aarch64-unknown-linux-musl",
    }.freeze

    attr_reader :release, :static_musl, :host_os, :host_cpu

    def initialize(
      release:,
      static_musl: false,
      host_os: RbConfig::CONFIG.fetch("host_os"),
      host_cpu: RbConfig::CONFIG.fetch("host_cpu")
    )
      @release = release
      @static_musl = static_musl
      @host_os = host_os
      @host_cpu = host_cpu
    end

    def target
      return windows_target if windows?
      return linux_musl_target if static_musl && linux?

      nil
    end

    def executable_name
      windows? ? "rubydex_mcp.exe" : "rubydex_mcp"
    end

    def relative_binary_path
      parts = ["target"]
      parts << target if target
      parts << profile
      parts << executable_name
      parts.join("/")
    end

    def cargo_command(manifest_path)
      args = cargo_environment.map { |key, value| "#{key}=#{value}" }
      args += ["cargo", "build", "--manifest-path", manifest_path.to_s]
      args << "--release" if release
      args << "--package" << "rubydex-mcp"
      args << "--target" << target if target
      shell_command(args)
    end

    def verify_static_command(path, ruby: RbConfig.ruby)
      return unless target&.end_with?("-linux-musl")

      script = 'path = ARGV.fetch(0); abort("#{path} has a dynamic ELF interpreter") unless Rubydex::MCPExecutableBuild.static_elf?(path)'

      shell_command([
        ruby,
        "-I#{File.expand_path("..", __dir__)}",
        "-rrubydex/mcp_executable_build",
        "-e",
        script,
        path.to_s,
      ])
    end

    class << self
      def static_elf?(path)
        File.open(path, "rb") do |file|
          return false unless file.read(4) == "\x7fELF".b

          elf_class = file.read(1)&.unpack1("C")
          return false unless elf_class == 2

          file.seek(0x20)
          program_header_offset = file.read(8).unpack1("Q<")
          file.seek(0x36)
          program_header_entry_size = file.read(2).unpack1("v")
          program_header_count = file.read(2).unpack1("v")

          program_header_count.times do |index|
            file.seek(program_header_offset + (index * program_header_entry_size))
            return false if file.read(4).unpack1("V") == 3
          end

          true
        end
      rescue SystemCallError, EOFError
        false
      end
    end

    private

    def linux?
      host_os.include?("linux")
    end

    def windows?
      host_os.match?(/mswin|mingw|cygwin/)
    end

    def windows_target
      "x86_64-pc-windows-gnu"
    end

    def linux_musl_target
      LINUX_MUSL_TARGETS.fetch(host_cpu) do
        raise "Unsupported Linux CPU for static rubydex_mcp build: #{host_cpu}"
      end
    end

    def cargo_environment
      return {} unless target&.end_with?("-linux-musl")

      env_target = target.upcase.tr("-", "_")
      {
        "CC_#{target.tr("-", "_")}" => "musl-gcc",
        "CARGO_TARGET_#{env_target}_LINKER" => "musl-gcc",
        "CARGO_TARGET_#{env_target}_RUSTFLAGS" => "-C target-feature=+crt-static -C link-arg=-static",
      }
    end

    def profile
      release ? "release" : "debug"
    end

    def shell_command(args)
      args.map do |arg|
        if (match = arg.match(/\A([A-Za-z0-9_]+)=(.*)\z/))
          "#{match[1]}=#{Shellwords.escape(match[2])}"
        elsif arg.match?(/\A\$\([A-Za-z0-9_]+\)\z/)
          arg
        else
          Shellwords.escape(arg)
        end
      end.join(" ")
    end
  end
end
