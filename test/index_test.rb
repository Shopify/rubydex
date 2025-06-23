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

  def test_get_member
    repository = Index::Repository.new
    repository.add_entry("key", "value")
    entry = repository.get_entry("key")
    binding.irb
    assert_equal "member_value", entry.member.value
  end

  def test_get_nested_member
    repository = Index::Repository.new
    repository.add_entry("key", "value")
    entry = repository.get_entry("key")
    assert_equal "nested_value", entry.member.nested_member.value
  end
end
