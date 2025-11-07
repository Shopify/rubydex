# frozen_string_literal: true

require "test_helper"
require "helpers/context"

class DeclarationTest < Minitest::Test
  include Test::Helpers::WithContext

  def test_instantiating_a_declaration_from_ruby_fails
    e = assert_raises(NoMethodError) do
      Saturn::Declaration.new
    end

    assert_equal("private method 'new' called for class Saturn::Declaration", e.message)
  end

  def test_declaration_initialize_from_graph
    with_context do |context|
      context.write!("file1.rb", "class A; end")

      graph = Saturn::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

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
      graph.resolve

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
      graph.resolve

      declaration = graph.declarations.first
      refute_nil(declaration)

      definitions = []
      declaration.definitions do |definition|
        definitions << definition
      end

      assert_equal(2, definitions.size)
    end
  end

  def test_unqualified_name
    with_context do |context|
      context.write!("file1.rb", "module A; class B; end; end")

      graph = Saturn::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      decl_a = graph["A"]
      refute_nil(decl_a)
      assert_equal("A", decl_a.unqualified_name)

      decl_b = graph["A::B"]
      refute_nil(decl_b)
      assert_equal("B", decl_b.unqualified_name)
    end
  end
end
