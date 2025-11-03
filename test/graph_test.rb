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

  def test_indexing_invalid_file_paths
    graph = Saturn::Graph.new

    assert_equal("File read error: Path 'not_found.rb' does not exist", graph.index_all(["not_found.rb"]))
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

  def test_graph_get_declaration
    with_context do |context|
      context.write!("file1.rb", "class A; end")
      context.write!("file2.rb", "class B; end")

      graph = Saturn::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      declaration = graph["A"]
      refute_nil(declaration)

      declaration = graph["B"]
      refute_nil(declaration)

      declaration = graph["C"]
      assert_nil(declaration)
    end
  end

  def test_list_all_declarations_enumerator
    with_context do |context|
      context.write!("file1.rb", "class A; end")
      context.write!("file2.rb", "class B; end")

      graph = Saturn::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      enumerator = graph.declarations

      assert_equal(3, enumerator.size)
      assert_equal(3, enumerator.count)
      assert_equal(3, enumerator.to_a.size)
    end
  end

  def test_list_all_declarations_with_block
    with_context do |context|
      context.write!("file1.rb", "class A; end")
      context.write!("file2.rb", "class B; end")

      graph = Saturn::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      declarations = []
      graph.declarations do |declaration|
        declarations << declaration
      end

      assert_equal(3, declarations.size)
    end
  end

  def test_graph_documents_enumerator
    with_context do |context|
      context.write!("file1.rb", "class A; end")
      context.write!("file2.rb", "class B; end")

      graph = Saturn::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      enumerator = graph.documents

      assert_equal(2, enumerator.size)
      assert_equal(2, enumerator.count)
      assert_equal(2, enumerator.to_a.size)
    end
  end

  def test_graph_documents_with_block
    with_context do |context|
      context.write!("file1.rb", "class A; end")
      context.write!("file2.rb", "class B; end")

      graph = Saturn::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      documents = []
      graph.documents do |document|
        documents << document
      end

      assert_equal(2, documents.size)
    end
  end
end
