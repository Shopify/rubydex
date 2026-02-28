# frozen_string_literal: true

require "test_helper"

class LocationTest < Minitest::Test
  def test_location_equality
    a = Rubydex::Location.new(uri: "file:///foo.rb", start_line: 0, end_line: 0, start_column: 0, end_column: 5)
    b = Rubydex::Location.new(uri: "file:///foo.rb", start_line: 0, end_line: 0, start_column: 0, end_column: 5)

    assert_equal(0, a <=> b)
    assert_equal(a, b)
  end

  def test_location_ordering_by_uri
    a = Rubydex::Location.new(uri: "file:///a.rb", start_line: 0, end_line: 0, start_column: 0, end_column: 0)
    b = Rubydex::Location.new(uri: "file:///b.rb", start_line: 0, end_line: 0, start_column: 0, end_column: 0)

    assert_equal(-1, a <=> b)
    assert_equal(1, b <=> a)
  end

  def test_location_ordering_by_start_line
    a = Rubydex::Location.new(uri: "file:///foo.rb", start_line: 1, end_line: 1, start_column: 0, end_column: 0)
    b = Rubydex::Location.new(uri: "file:///foo.rb", start_line: 2, end_line: 2, start_column: 0, end_column: 0)

    assert_equal(-1, a <=> b)
    assert_equal(1, b <=> a)
  end

  def test_location_ordering_by_start_column
    a = Rubydex::Location.new(uri: "file:///foo.rb", start_line: 0, end_line: 0, start_column: 0, end_column: 5)
    b = Rubydex::Location.new(uri: "file:///foo.rb", start_line: 0, end_line: 0, start_column: 3, end_column: 5)

    assert_equal(-1, a <=> b)
    assert_equal(1, b <=> a)
  end

  def test_location_compared_to_non_location_returns_negative
    loc = Rubydex::Location.new(uri: "file:///foo.rb", start_line: 0, end_line: 0, start_column: 0, end_column: 0)

    assert_equal(-1, loc <=> "not a location")
    assert_equal(-1, loc <=> 42)
    assert_equal(-1, loc <=> nil)
  end

  def test_location_sort
    c = Rubydex::Location.new(uri: "file:///foo.rb", start_line: 5, end_line: 5, start_column: 0, end_column: 0)
    a = Rubydex::Location.new(uri: "file:///foo.rb", start_line: 0, end_line: 0, start_column: 0, end_column: 0)
    b = Rubydex::Location.new(uri: "file:///foo.rb", start_line: 3, end_line: 3, start_column: 0, end_column: 0)

    assert_equal([a, b, c], [c, a, b].sort)
  end

  def test_location_to_display
    loc = Rubydex::Location.new(uri: "file:///foo.rb", start_line: 0, end_line: 5, start_column: 3, end_column: 10)
    display = loc.to_display

    assert_instance_of(Rubydex::DisplayLocation, display)
    assert_equal(1, display.start_line)
    assert_equal(6, display.end_line)
    assert_equal(4, display.start_column)
    assert_equal(11, display.end_column)
  end

  def test_location_to_s
    loc = Rubydex::Location.new(uri: "file:///foo.rb", start_line: 0, end_line: 5, start_column: 3, end_column: 10)
    assert_equal("#{loc.path}:1:4-6:11", loc.to_s)
  end

  def test_display_location_equality
    a = Rubydex::DisplayLocation.new(uri: "file:///foo.rb", start_line: 1, end_line: 1, start_column: 1, end_column: 6)
    b = Rubydex::DisplayLocation.new(uri: "file:///foo.rb", start_line: 1, end_line: 1, start_column: 1, end_column: 6)

    assert_equal(0, a <=> b)
    assert_equal(a, b)
  end

  def test_display_location_to_display_returns_self
    display = Rubydex::DisplayLocation.new(uri: "file:///foo.rb", start_line: 1, end_line: 1, start_column: 1, end_column: 6)
    display2 = display.to_display
    assert_same(display, display2)
    assert_same(display, display2.to_display)
  end

  def test_display_location_to_s
    display = Rubydex::DisplayLocation.new(uri: "file:///foo.rb", start_line: 1, end_line: 6, start_column: 4, end_column: 11)
    assert_equal("#{display.path}:1:4-6:11", display.to_s)
  end

  def test_location_and_its_display_location_are_equal
    loc = Rubydex::Location.new(uri: "file:///foo.rb", start_line: 0, end_line: 0, start_column: 0, end_column: 5)
    display = loc.to_display
    assert_equal(0, loc <=> display)
    assert_equal(0, display <=> loc)
  end

  def test_location_and_display_location_ordering_across_types
    loc = Rubydex::Location.new(uri: "file:///foo.rb", start_line: 5, end_line: 5, start_column: 0, end_column: 0)
    display = Rubydex::Location.new(
      uri: "file:///foo.rb",
      start_line: 2,
      end_line: 2,
      start_column: 0,
      end_column: 0,
    ).to_display

    assert_equal(1, loc <=> display)
    assert_equal(-1, display <=> loc)
  end

  def test_sorting_mixed_locations_and_display_locations
    loc_a = Rubydex::Location.new(uri: "file:///foo.rb", start_line: 5, end_line: 5, start_column: 0, end_column: 0)
    display_b = Rubydex::Location.new(
      uri: "file:///foo.rb",
      start_line: 3,
      end_line: 3,
      start_column: 0,
      end_column: 0,
    ).to_display
    loc_c = Rubydex::Location.new(uri: "file:///foo.rb", start_line: 1, end_line: 1, start_column: 0, end_column: 0)

    assert_equal([loc_c, display_b, loc_a], [loc_a, display_b, loc_c].sort)
  end
end
