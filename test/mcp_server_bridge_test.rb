# frozen_string_literal: true

require_relative "test_helper"

require "rubydex/mcp_server_bridge"

class MCPServerBridgeTest < Minitest::Test
  def setup
    @original_bundle_gemfile = ENV.fetch("BUNDLE_GEMFILE", nil)
  end

  def teardown
    if @original_bundle_gemfile.nil?
      ENV.delete("BUNDLE_GEMFILE")
    else
      ENV["BUNDLE_GEMFILE"] = @original_bundle_gemfile
    end
  end

  def test_compute_index_paths_include_bundler_dependency_require_paths
    Rubydex::MCPServerBridge.setup_bundler_context(Dir.pwd)

    paths = Rubydex::MCPServerBridge.compute_index_paths([Dir.pwd])
    spec = Bundler.load.specs.find { |loaded_spec| loaded_spec.name == "rake" }
    expected_path = File.join(spec.full_gem_path, "lib")

    assert_includes(paths, Dir.pwd)
    assert_includes(paths, expected_path)
  end

  def test_passthrough_detects_help_and_version_flags
    assert(Rubydex::MCPServerBridge.passthrough?(["-h"]))
    assert(Rubydex::MCPServerBridge.passthrough?(["--help"]))
    assert(Rubydex::MCPServerBridge.passthrough?(["-V"]))
    assert(Rubydex::MCPServerBridge.passthrough?(["--version"]))
    assert(Rubydex::MCPServerBridge.passthrough?(["/some/path", "--help"]))
  end

  def test_passthrough_is_false_for_paths
    refute(Rubydex::MCPServerBridge.passthrough?([]))
    refute(Rubydex::MCPServerBridge.passthrough?(["/some/path"]))
    refute(Rubydex::MCPServerBridge.passthrough?(["/some/path", "/another/path"]))
  end

  def test_compute_index_paths_defaults_to_cwd_when_argv_empty
    Rubydex::MCPServerBridge.setup_bundler_context(Dir.pwd)

    paths = Rubydex::MCPServerBridge.compute_index_paths([])

    assert_includes(paths, Dir.pwd)
  end
end
