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

  def test_create_point
    point = Index::Point.new(10, 20)
    assert_equal(10, point.x)
    assert_equal(20, point.y)
  end

  def test_point_with_zero_values
    point = Index::Point.new(0, 0)
    assert_equal(0, point.x)
    assert_equal(0, point.y)
  end

  def test_create_message
    message = Index::Message.new("Hello from native!")
    assert_equal("Hello from native!", message.content)
  end

  def test_message_with_empty_string
    message = Index::Message.new("")
    assert_equal("", message.content)
  end

  def test_message_with_unicode
    message = Index::Message.new("Hello 世界! 🌍")
    assert_equal("Hello 世界! 🌍", message.content)
  end

  def test_message_with_special_characters
    message = Index::Message.new("Special chars: @#$%^&*()_+{}|:<>?")
    assert_equal("Special chars: @#$%^&*()_+{}|:<>?", message.content)
  end
end
