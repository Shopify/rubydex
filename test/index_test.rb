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

  def test_cache_dump
    repository = Index::Repository.new
    repository.add_entry("key", "value")
    repository.add_entry("key2", "value")
    assert_raises(SystemExit) do
      repository.dump_to_cache
    end
  end
end
