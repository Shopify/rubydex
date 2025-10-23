# frozen_string_literal: true

require "test_helper"
require "helpers/context"

class DeclarationTest < Minitest::Test
  include Test::Helpers::WithContext

  def test_instantiating_a_declaration_from_ruby_fails
    e = assert_raises(NoMethodError) do
      Saturn::Declaration.new
    end

    assert_equal("undefined method 'new' for class Saturn::Declaration", e.message)
  end

  def test_declaration_initialize_from_graph
    with_context do |context|
      context.write!("file1.rb", "class A; end")

      graph = Saturn::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      declaration = graph["A"]
      assert_instance_of(Saturn::Declaration, declaration)
      assert_equal("A", declaration.name)
    end
  end

  def test_definitions_enumerator
    with_context do |context|
      context.write!("file1.rb", "class A; end")
      context.write!("file2.rb", "class A; end")

      graph = Saturn::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      declaration = graph.declarations.first
      refute_nil(declaration)

      enumerator = declaration.definitions
      assert_equal(2, enumerator.size)
      assert_equal(2, enumerator.count)
      assert_equal(2, enumerator.to_a.size)
    end
  end

  def test_definitions_with_block
    with_context do |context|
      context.write!("file1.rb", "class A; end")
      context.write!("file2.rb", "class A; end")

      graph = Saturn::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      declaration = graph.declarations.first
      refute_nil(declaration)

      definitions = []
      declaration.definitions do |definition|
        definitions << definition
      end

      assert_equal(2, definitions.size)
    end
  end
end
