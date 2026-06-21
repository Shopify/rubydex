# frozen_string_literal: true

require_relative "test_helper"

require "fileutils"
require "tmpdir"
require "tempfile"
require "rubydex/mcp_server_launcher"

class MCPServerExecutableTest < Minitest::Test
  def test_host_loader_reads_loader_from_maps
    dir = Dir.mktmpdir
    loader_path = File.join(dir, "ld-linux-x86-64.so.2")
    File.write(loader_path, "")

    maps = Tempfile.new("maps")
    maps.write(<<~MAPS)
      7f3d10000000-7f3d10022000 r--p 00000000 00:00 0 [heap]
      7f3d20000000-7f3d20022000 r-xp 00000000 00:00 0 #{loader_path}
    MAPS
    maps.close

    assert_equal(loader_path, Rubydex::MCPServerLauncher.host_loader(maps.path))
  ensure
    maps&.unlink
    FileUtils.remove_entry_secure(dir) if dir && File.directory?(dir)
  end

  def test_host_loader_ignores_missing_loader_paths
    Dir.mktmpdir do |dir|
      loader_path = File.join(dir, "ld-linux-x86-64.so.2")
      File.write(loader_path, "")

      maps = Tempfile.new("maps")
      maps.write(<<~MAPS)
        7f3d10000000-7f3d10022000 r-xp 00000000 00:00 0 /missing/ld-linux-x86-64.so.2
        7f3d20000000-7f3d20022000 r-xp 00000000 00:00 0 #{loader_path}
      MAPS
      maps.close

      assert_equal(loader_path, Rubydex::MCPServerLauncher.host_loader(maps.path))
    ensure
      maps&.unlink
    end
  end

  def test_host_loader_returns_nil_when_maps_file_is_missing
    assert_nil(Rubydex::MCPServerLauncher.host_loader("/missing/proc/self/maps"))
  end

  def test_host_loader_returns_existing_linux_loader
    skip("Linux-only") unless File.file?("/proc/self/maps")

    loader = Rubydex::MCPServerLauncher.host_loader("/proc/self/maps")

    refute_nil(loader)
    assert_path_exists(loader)
    assert_match(/\Ald-.*\.so/, File.basename(loader))
  end
end
