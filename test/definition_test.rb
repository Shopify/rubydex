# frozen_string_literal: true

require "test_helper"
require "helpers/context"

class DefinitionTest < Minitest::Test
  include Test::Helpers::WithContext

  def test_instantiating_a_definition_from_ruby_fails
    e = assert_raises(NoMethodError) do
      Saturn::Definition.new
    end

    assert_equal("private method 'new' called for class Saturn::Definition", e.message)

    assert_raises(NoMethodError) { Saturn::ClassDefinition.new }
    assert_raises(NoMethodError) { Saturn::ModuleDefinition.new }
    assert_raises(NoMethodError) { Saturn::ConstantDefinition.new }
    assert_raises(NoMethodError) { Saturn::MethodDefinition.new }
    assert_raises(NoMethodError) { Saturn::AttrAccessorDefinition.new }
    assert_raises(NoMethodError) { Saturn::AttrReaderDefinition.new }
    assert_raises(NoMethodError) { Saturn::AttrWriterDefinition.new }
    assert_raises(NoMethodError) { Saturn::GlobalVariableDefinition.new }
    assert_raises(NoMethodError) { Saturn::InstanceVariableDefinition.new }
    assert_raises(NoMethodError) { Saturn::ClassVariableDefinition.new }
  end

  def test_definition_subclass_mapping
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class A
          @@c = 1
          attr_accessor :x
          attr_reader :y
          attr_writer :z
        end
        module M; end
        FOO = 1
        def bar; end
        $g = 1
        @i = 1
      RUBY

      graph = Saturn::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      defs = graph.declarations.map { |d| d.definitions.to_a }.flatten
      refute_empty(defs)

      # Ensure at least one of each subtype is present
      assert(defs.any? { |d| d.is_a?(Saturn::ClassDefinition) })
      assert(defs.any? { |d| d.is_a?(Saturn::ModuleDefinition) })
      assert(defs.any? { |d| d.is_a?(Saturn::ConstantDefinition) })
      assert(defs.any? { |d| d.is_a?(Saturn::MethodDefinition) })
      assert(defs.any? { |d| d.is_a?(Saturn::AttrAccessorDefinition) })
      assert(defs.any? { |d| d.is_a?(Saturn::AttrReaderDefinition) })
      assert(defs.any? { |d| d.is_a?(Saturn::AttrWriterDefinition) })
      assert(defs.any? { |d| d.is_a?(Saturn::GlobalVariableDefinition) })
      assert(defs.any? { |d| d.is_a?(Saturn::InstanceVariableDefinition) })
      assert(defs.any? { |d| d.is_a?(Saturn::ClassVariableDefinition) })
    end
  end
end
