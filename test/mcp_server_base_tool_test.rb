# frozen_string_literal: true

require "test_helper"
require "mocha/minitest"
require "rubydex/mcp_server"

class MCPServerBaseToolTest < Minitest::Test
  def setup
    @tool = Rubydex::MCPServer::BaseTool.new(Rubydex::Graph.new(workspace_path: Dir.pwd))
  end

  def test_file_path_for_uri_removes_windows_file_uri_leading_slash
    Gem.stubs(:win_platform?).returns(true)

    assert_equal("D:/a/_temp/app.rb", @tool.file_path_for_uri("file:///D:/a/_temp/app.rb"))
  end

  def test_file_path_for_uri_decodes_file_uri_paths
    Gem.stubs(:win_platform?).returns(false)

    assert_equal("/tmp/my app.rb", @tool.file_path_for_uri("file:///tmp/my%20app.rb"))
  end

  def test_format_path_preserves_non_file_uris
    assert_equal("untitled:Untitled-1", @tool.format_path("untitled:Untitled-1"))
  end
end
