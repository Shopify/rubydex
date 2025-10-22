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

      declaration = graph.declarations.first
      assert_instance_of(Saturn::Declaration, declaration)
      assert_equal("A", declaration.name)
    end
  end
end
