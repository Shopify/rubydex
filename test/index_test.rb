# frozen_string_literal: true

require "test_helper"

class IndexRustyTest < Minitest::Test
  def test_that_it_has_a_version_number
    refute_nil(::Index::VERSION)
  end

  def test_index_all
    repository = Index::Repository.new
    repository.index_all([__FILE__])
  end

  def test_add_and_get
    repository = Index::Repository.new
    repository.add_class("MyClass", 0, 30)
    entry = repository.get("MyClass")

    assert_equal(0, entry.start_offset)
    assert_equal(30, entry.end_offset)
  end
end
