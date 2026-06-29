# frozen_string_literal: true

require "minitest/autorun"
require "rbconfig"
require "shellwords"
require "tmpdir"

$LOAD_PATH.unshift(File.expand_path("../lib", __dir__))
require "rubydex/mcp_executable_build"

class MCPExecutableBuildTest < Minitest::Test
  def test_linux_release_uses_static_musl_target_for_x86_64
    build = Rubydex::MCPExecutableBuild.new(
      release: true,
      static_musl: true,
      host_os: "linux-gnu",
      host_cpu: "x86_64",
    )

    assert_equal("x86_64-unknown-linux-musl", build.target)
    assert_equal("target/x86_64-unknown-linux-musl/release/rubydex_mcp", build.relative_binary_path)
    assert_equal(
      "CC_x86_64_unknown_linux_musl=musl-gcc CARGO_TARGET_X86_64_UNKNOWN_LINUX_MUSL_LINKER=musl-gcc CARGO_TARGET_X86_64_UNKNOWN_LINUX_MUSL_RUSTFLAGS=-C\\ target-feature\\=+crt-static\\ -C\\ link-arg\\=-static cargo build --manifest-path rust/Cargo.toml --release --package rubydex-mcp --target x86_64-unknown-linux-musl",
      build.cargo_command("rust/Cargo.toml"),
    )
  end

  def test_linux_release_uses_static_musl_target_for_aarch64
    build = Rubydex::MCPExecutableBuild.new(
      release: true,
      static_musl: true,
      host_os: "linux-gnu",
      host_cpu: "aarch64",
    )

    assert_equal("aarch64-unknown-linux-musl", build.target)
    assert_equal("target/aarch64-unknown-linux-musl/release/rubydex_mcp", build.relative_binary_path)
  end

  def test_non_release_linux_keeps_host_target
    build = Rubydex::MCPExecutableBuild.new(
      release: false,
      host_os: "linux-gnu",
      host_cpu: "x86_64",
    )

    assert_nil(build.target)
    assert_equal("target/debug/rubydex_mcp", build.relative_binary_path)
    assert_equal(
      "cargo build --manifest-path rust/Cargo.toml --package rubydex-mcp",
      build.cargo_command("rust/Cargo.toml"),
    )
    assert_nil(build.verify_static_command("target/debug/rubydex_mcp"))
  end

  def test_source_install_linux_release_keeps_host_target
    build = Rubydex::MCPExecutableBuild.new(
      release: true,
      static_musl: false,
      host_os: "linux-gnu",
      host_cpu: "x86_64",
    )

    assert_nil(build.target)
    assert_equal("target/release/rubydex_mcp", build.relative_binary_path)
    assert_nil(build.verify_static_command("target/release/rubydex_mcp"))
  end

  def test_non_linux_release_keeps_host_target
    build = Rubydex::MCPExecutableBuild.new(
      release: true,
      host_os: "darwin24",
      host_cpu: "arm64",
    )

    assert_nil(build.target)
    assert_equal("target/release/rubydex_mcp", build.relative_binary_path)
  end

  def test_windows_uses_exe_name
    build = Rubydex::MCPExecutableBuild.new(
      release: true,
      host_os: "mingw32",
      host_cpu: "x64",
    )

    assert_equal("rubydex_mcp.exe", build.executable_name)
    assert_equal("target/x86_64-pc-windows-gnu/release/rubydex_mcp.exe", build.relative_binary_path)
  end

  def test_static_musl_build_has_verification_command
    build = Rubydex::MCPExecutableBuild.new(
      release: true,
      static_musl: true,
      host_os: "linux-gnu",
      host_cpu: "x86_64",
    )

    command = build.verify_static_command("target/x86_64-unknown-linux-musl/release/rubydex_mcp")
    argv = Shellwords.split(command)

    assert_equal(RbConfig.ruby, argv[0])
    assert_equal("-I#{File.expand_path("../lib", __dir__)}", argv[1])
    assert_equal("-rrubydex/mcp_executable_build", argv[2])
    assert_equal("-e", argv[3])
    assert_includes(argv[4], "Rubydex::MCPExecutableBuild.static_elf?(path)")
    assert_equal("target/x86_64-unknown-linux-musl/release/rubydex_mcp", argv[5])
  end

  def test_static_musl_build_can_verify_with_makefile_ruby
    build = Rubydex::MCPExecutableBuild.new(
      release: true,
      static_musl: true,
      host_os: "linux-gnu",
      host_cpu: "x86_64",
    )

    command = build.verify_static_command("target/x86_64-unknown-linux-musl/release/rubydex_mcp", ruby: "$(RUBY)")
    argv = Shellwords.split(command)

    assert_match(/\A\$\(RUBY\) /, command)
    assert_equal("$(RUBY)", argv[0])
  end

  def test_unsupported_linux_cpu_fails_loudly
    build = Rubydex::MCPExecutableBuild.new(
      release: true,
      static_musl: true,
      host_os: "linux-gnu",
      host_cpu: "powerpc64le",
    )

    error = assert_raises(RuntimeError) { build.target }
    assert_match(/Unsupported Linux CPU for static rubydex_mcp build: powerpc64le/, error.message)
  end

  def test_static_elf_has_no_interpreter
    Dir.mktmpdir do |dir|
      elf_path = File.join(dir, "rubydex_mcp")
      File.binwrite(elf_path, elf_header_with_program_header(type: 1))

      assert(Rubydex::MCPExecutableBuild.static_elf?(elf_path))
    end
  end

  def test_dynamic_elf_has_interpreter
    Dir.mktmpdir do |dir|
      elf_path = File.join(dir, "rubydex_mcp")
      File.binwrite(elf_path, elf_header_with_program_header(type: 3))

      refute(Rubydex::MCPExecutableBuild.static_elf?(elf_path))
    end
  end

  private

  def elf_header_with_program_header(type:)
    header = "\x7fELF".b
    header << [2, 1, 1, 0].pack("C*")
    header << ("\0".b * 8)
    header << [2, 62, 1].pack("v v V")
    header << [0, 64].pack("Q< Q<")
    header << [0].pack("Q<")
    header << [0, 64, 56, 1, 0, 0, 0].pack("V v v v v v v")

    program_header = [type, 0].pack("V V")
    program_header << [0, 0, 0, 0, 0, 0].pack("Q< Q< Q< Q< Q< Q<")

    header + program_header
  end
end
