# frozen_string_literal: true

require "test_helper"

class DefinitionTest < Minitest::Test
  def test_instantiating_a_definition_from_ruby_fails
    e = assert_raises(NoMethodError) do
      Saturn::Definition.new
    end

    assert_equal("undefined method 'new' for class Saturn::Definition", e.message)
  end
end
