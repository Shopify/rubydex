# frozen_string_literal: true

require "test_helper"
require "helpers/context"

class GraphTest < Minitest::Test
  include Test::Helpers::WithContext

  def test_indexing_empty_context
    with_context do |context|
      graph = Saturn::Graph.new
      assert_nil(graph.index_all(context.glob("**/*.rb")))
    end
  end

  def test_indexing_context_files
    with_context do |context|
      graph = Saturn::Graph.new

      context.write!("foo.rb", "class Foo; end")
      context.write!("bar.rb", "class Bar; end")

      assert_nil(graph.index_all(context.glob("**/*.rb")))
    end
  end

  def test_passing_invalid_arguments_to_index_all
    graph = Saturn::Graph.new

    assert_raises(TypeError) do
      graph.index_all("not an array")
    end

    assert_raises(TypeError) do
      graph.index_all([1, 2, 3])
    end
  end

  def test_setting_the_graph_configuration
    graph = Saturn::Graph.new

    assert_raises(TypeError) do
      graph.set_configuration(123)
    end

    assert_raises(RuntimeError) do
      graph.set_configuration(".non-existing-folder/graph.db")
    end

    graph.set_configuration("graph.db")
    pass
  ensure
    Dir.glob("graph.db*").each { |f| File.delete(f) }
  end
end
