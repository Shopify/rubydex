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
      context.write!("foo.rb", "class Foo; end")
      context.write!("bar.rb", "class Bar; end")

      graph = Saturn::Graph.new
      assert_nil(graph.index_all(context.glob("**/*.rb")))
    end
  end

  def test_indexing_invalid_file_paths
    graph = Saturn::Graph.new

    error = assert_raises(Saturn::IndexingError) do
      graph.index_all(["not_found.rb"])
    end

    assert_kind_of(Saturn::Error, error)
    assert_match(/File read error: Path `.*not_found.rb` does not exist/, error.message)
  end

  def test_indexing_with_parse_errors
    with_context do |context|
      context.write!("file.rb", "class Foo")

      graph = Saturn::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      assert_diagnostics(
        [
          { code: 2000, severity: :error, path: "file.rb", message: "expected an `end` to close the `class` statement" },
          { code: 2000, severity: :error, path: "file.rb", message: "unexpected end-of-input, assuming it is closing the parent top level context" },
        ],
        graph.diagnostics,
      )
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

  def test_graph_get_declaration
    with_context do |context|
      context.write!("file1.rb", "class A; end")
      context.write!("file2.rb", "class B; end")

      graph = Saturn::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

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
      graph.resolve

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
      graph.resolve

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

  private

  def assert_diagnostics(expected, actual)
    assert_equal(
      expected,
      actual.sort_by { |d| [d.location, d.message] }
        .map { |d| { code: d.code, severity: d.severity, path: File.basename(d.location.path), message: d.message } },
    )
  end
end
