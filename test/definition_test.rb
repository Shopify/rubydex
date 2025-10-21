# frozen_string_literal: true

require "test_helper"

class DefinitionTest < Minitest::Test
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
end
