# frozen_string_literal: true

require "test_helper"

class GraphTest < Minitest::Test
  def test_indexing_a_list_of_file_paths
    graph = Index::Graph.new
    assert_nil(graph.index_all([__FILE__]))
  end

  def test_indexing_returns_errors
    graph = Index::Graph.new
    message = graph.index_all(["/non_existing_file.rb"])

    assert_equal("File read error: Error reading path '/non_existing_file.rb': No such file or directory (os error 2)", message)
  end

  def test_passing_invalid_arguments_to_index_all
    graph = Index::Graph.new

    assert_raises(TypeError) do
      graph.index_all("not an array")
    end

    assert_raises(TypeError) do
      graph.index_all([1, 2, 3])
    end
  end

  def test_setting_the_graph_configuration
    graph = Index::Graph.new

    assert_raises(TypeError) do
      graph.set_configuration(123)
    end

    graph.set_configuration(".ruby-lsp/graph.db")
    pass
  end
end
