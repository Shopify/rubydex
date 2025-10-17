# frozen_string_literal: true

require "test_helper"
require "helpers/context"

class GraphTest < Minitest::Test
  include Test::Helpers::WithContext

  def test_indexing_a_list_of_file_paths
    graph = Index::Graph.new
    assert_nil(graph.index_all([__FILE__]))
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

    assert_raises(RuntimeError) do
      graph.set_configuration(".non-existing-folder/graph.db")
    end

    graph.set_configuration("graph.db")
    pass
  ensure
    Dir.glob("graph.db*").each { |f| File.delete(f) }
  end

  def test_indexing_an_empty_context_returns_no_errors
    with_context do |context|
      graph = Index::Graph.new
      assert_nil(graph.index_all(context.glob("**/*.rb")))
    end
  end

  def test_list_all_declarations
    with_context do |context|
      context.write!("file1.rb", "class A; end")
      context.write!("file2.rb", "class B; end")

      graph = Index::Graph.new
      assert_nil(graph.index_all(context.glob("**/*.rb")))
      assert_equal(2, graph.declarations.size)
    end
  end

  def test_resolving_a_declaration_by_name
    with_context do |context|
      context.write!("file1.rb", "class A; end")
      context.write!("file2.rb", "class B; end")

      graph = Index::Graph.new
      assert_nil(graph.index_all(context.glob("**/*.rb")))

      refute_nil(graph["A"])
      refute_nil(graph["B"])
      assert_nil(graph["C"])
    end
  end

  def test_declaration_name
    with_context do |context|
      context.write!("file1.rb", "class A; end")

      graph = Index::Graph.new
      assert_nil(graph.index_all(context.glob("**/*.rb")))

      declaration = graph["A"]
      refute_nil(declaration)
      assert_equal("A", declaration.name)
    end
  end

  def test_declaration_definitions
    with_context do |context|
      context.write!("file1.rb", "class A; end")
      context.write!("file2.rb", "class A; end")

      graph = Index::Graph.new
      assert_nil(graph.index_all(context.glob("**/*.rb")))

      declaration = graph["A"]
      refute_nil(declaration)
      assert_equal(2, declaration.definitions.size)
    end
  end

  def test_definition_name_uri_and_offsets
    with_context do |context|
      context.write!("file1.rb", "class A; end")

      graph = Index::Graph.new
      assert_nil(graph.index_all(context.glob("**/*.rb")))

      declaration = graph["A"]
      refute_nil(declaration)

      definition = declaration.definitions.first
      assert_equal("A", definition.name)
      assert_match(%r{^file://}, definition.uri_path)
      assert(definition.uri_path.end_with?("/file1.rb"), "URI should end with /file1.rb")
      assert_equal(0, definition.start_location)
      assert_equal(12, definition.end_location)
    end
  end

  def test_definition_comments
    with_context do |context|
      context.write!("file2.rb", "# Class doc\nclass B; end\n")

      graph = Index::Graph.new
      assert_nil(graph.index_all(context.glob("**/*.rb")))

      declaration = graph["B"]
      refute_nil(declaration)

      definition = declaration.definitions.first
      assert_equal("Class doc", definition.comments)
    end
  end

  def test_definition_kind
    with_context do |context|
      context.write!("file1.rb", "class A; end")
      context.write!("file2.rb", "module A; end")

      graph = Index::Graph.new
      assert_nil(graph.index_all(context.glob("**/*.rb")))

      declaration = graph["A"]
      refute_nil(declaration)

      definitions = declaration.definitions
      assert_equal(2, definitions.size)
      assert_equal(["Class", "Module"], definitions.map(&:kind).sort)
    end
  end
end
