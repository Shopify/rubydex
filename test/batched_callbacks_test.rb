# frozen_string_literal: true

require "test_helper"

class BatchedCallbacksTest < Minitest::Test
  def setup
    @graph = Rubydex::Graph.new
  end

  def test_capture_dsl_methods_returns_self
    result = @graph.capture_dsl_methods(["belongs_to", "has_many"])
    assert_equal @graph, result
  end

  def test_each_dsl_file_requires_block
    @graph.capture_dsl_methods(["belongs_to"])
    @graph.index_all([fixture_path("belongs_to_example.rb")])

    assert_raises(LocalJumpError) do
      @graph.each_dsl_file
    end
  end

  def test_dsl_events_are_captured
    @graph.capture_dsl_methods(["belongs_to", "has_many"])
    @graph.index_all([fixture_path("belongs_to_example.rb")])

    captured_files = []
    captured_events = []

    @graph.each_dsl_file do |file_path, events|
      captured_files << file_path
      captured_events.concat(events)
    end

    assert_equal 1, captured_files.size
    assert captured_files.first.end_with?("belongs_to_example.rb")

    # Should have captured belongs_to and has_many
    method_names = captured_events.map(&:method_name)
    assert_includes method_names, "belongs_to"
    assert_includes method_names, "has_many"
  end

  def test_dsl_event_has_correct_attributes
    @graph.capture_dsl_methods(["belongs_to"])
    @graph.index_all([fixture_path("belongs_to_example.rb")])

    event = nil
    @graph.each_dsl_file do |_file_path, events|
      event = events.find { |e| e.method_name == "belongs_to" }
    end

    refute_nil event
    assert_equal "belongs_to", event.method_name
    assert_kind_of Integer, event.id
    assert_nil event.parent_id # Top-level DSL call
    assert_kind_of Integer, event.offset
    refute event.has_block
    assert_kind_of Array, event.arguments
    assert_equal ["author"], event.arguments
  end

  def test_parent_id_chain_for_nested_dsl_calls
    @graph.capture_dsl_methods(["describe", "context", "let"])
    @graph.index_all([fixture_path("rspec_let_example.rb")])

    captured_events = []
    @graph.each_dsl_file do |_file_path, events|
      captured_events.concat(events)
    end

    # Should have: describe (id:0), let (id:1, parent:0), context (id:2, parent:0), let (id:3, parent:2)
    describe_event = captured_events.find { |e| e.method_name == "describe" }
    let_events = captured_events.select { |e| e.method_name == "let" }
    context_event = captured_events.find { |e| e.method_name == "context" }

    refute_nil describe_event
    refute_nil context_event
    assert_equal 2, let_events.size

    # describe should have no parent
    assert_nil describe_event.parent_id
    assert describe_event.has_block

    # context should have describe as parent
    assert_equal describe_event.id, context_event.parent_id
    assert context_event.has_block

    # First let (value) should have describe as parent
    first_let = let_events.find { |e| e.arguments.include?("value") }
    refute_nil first_let
    assert_equal describe_event.id, first_let.parent_id

    # Second let (other) inside context should have context as parent
    second_let = let_events.find { |e| e.arguments.include?("other") }
    refute_nil second_let
    assert_equal context_event.id, second_let.parent_id
  end

  def test_no_events_without_filter
    # Don't set any DSL filter
    @graph.index_all([fixture_path("belongs_to_example.rb")])

    event_count = 0
    @graph.each_dsl_file do |_file_path, events|
      event_count += events.size
    end

    assert_equal 0, event_count
  end

  def test_clear_dsl_capture_filter
    @graph.capture_dsl_methods(["belongs_to"])
    @graph.clear_dsl_capture_filter
    @graph.index_all([fixture_path("belongs_to_example.rb")])

    event_count = 0
    @graph.each_dsl_file do |_file_path, events|
      event_count += events.size
    end

    assert_equal 0, event_count
  end

  def test_dsl_event_has_file_path
    @graph.capture_dsl_methods(["belongs_to"])
    @graph.index_all([fixture_path("belongs_to_example.rb")])

    event = nil
    @graph.each_dsl_file do |_file_path, events|
      event = events.first
    end

    refute_nil event
    assert event.file_path.end_with?("belongs_to_example.rb")
  end

  private

  def fixture_path(name)
    File.expand_path("fixtures/#{name}", __dir__)
  end
end
