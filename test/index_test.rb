# frozen_string_literal: true

require "test_helper"

class IndexRustyTest < Minitest::Test
  def test_that_it_has_a_version_number
    refute_nil(::Index::VERSION)
  end

  def test_add_entry
    repository = Index::Repository.new
    repository.add_entry("key", "value")
    entry = repository.get_entry("key")
    assert_equal(entry.value, "value")
  end

  def test_get_constant_number
    result = Index.get_constant_number
    assert_equal(42, result)
  end

  def test_increment_number
    result = Index.increment_number(10)
    assert_equal(11, result)
  end
end
