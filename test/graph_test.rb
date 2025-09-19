# frozen_string_literal: true

require "test_helper"

class GraphTest < Minitest::Test
  def setup
    @graph = Index::Graph.new
  end

  def teardown
    # Clean up any database files created during tests
    Dir.glob("graph.db*").each { |f| File.delete(f) }
  end

  def test_indexing_a_list_of_file_paths
    @graph.set_configuration("graph.db")
    assert_nil(@graph.index_all([__FILE__]))
  end

  def test_passing_invalid_arguments_to_index_all
    assert_raises(TypeError) do
      @graph.index_all("not an array")
    end

    assert_raises(TypeError) do
      @graph.index_all([1, 2, 3])
    end
  end

  def test_setting_the_graph_configuration
    assert_raises(TypeError) do
      @graph.set_configuration(123)
    end

    assert_raises(RuntimeError) do
      @graph.set_configuration(".non-existing-folder/graph.db")
    end

    @graph.set_configuration("graph.db")
    pass
  end
end
