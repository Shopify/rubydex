# frozen_string_literal: true

require "test_helper"
require "helpers/context"

class DefinitionTest < Minitest::Test
  include Test::Helpers::WithContext

  def test_instantiating_a_definition_from_ruby_fails
    e = assert_raises(NoMethodError) do
      Rubydex::Definition.new
    end

    assert_match(/private method .new. called for.* Rubydex::Definition/, e.message)

    assert_raises(NoMethodError) { Rubydex::ClassDefinition.new }
    assert_raises(NoMethodError) { Rubydex::ModuleDefinition.new }
    assert_raises(NoMethodError) { Rubydex::ConstantDefinition.new }
    assert_raises(NoMethodError) { Rubydex::MethodDefinition.new }
    assert_raises(NoMethodError) { Rubydex::AttrAccessorDefinition.new }
    assert_raises(NoMethodError) { Rubydex::AttrReaderDefinition.new }
    assert_raises(NoMethodError) { Rubydex::AttrWriterDefinition.new }
    assert_raises(NoMethodError) { Rubydex::GlobalVariableDefinition.new }
    assert_raises(NoMethodError) { Rubydex::InstanceVariableDefinition.new }
    assert_raises(NoMethodError) { Rubydex::ClassVariableDefinition.new }
    assert_raises(NoMethodError) { Rubydex::MethodAliasDefinition.new }
    assert_raises(NoMethodError) { Rubydex::GlobalVariableAliasDefinition.new }
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
        alias foo bar
        alias $baz $qux
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      defs = graph.documents
        .map { |d| d.definitions.to_a }
        .flatten
        .sort_by(&:location)

      assert_instance_of(Rubydex::ClassDefinition, defs[0])
      assert_instance_of(Rubydex::ClassVariableDefinition, defs[1])
      assert_instance_of(Rubydex::AttrAccessorDefinition, defs[2])
      assert_instance_of(Rubydex::AttrReaderDefinition, defs[3])
      assert_instance_of(Rubydex::AttrWriterDefinition, defs[4])
      assert_instance_of(Rubydex::ModuleDefinition, defs[5])
      assert_instance_of(Rubydex::ConstantDefinition, defs[6])
      assert_instance_of(Rubydex::MethodDefinition, defs[7])
      assert_instance_of(Rubydex::GlobalVariableDefinition, defs[8])
      assert_instance_of(Rubydex::InstanceVariableDefinition, defs[9])
      assert_instance_of(Rubydex::MethodAliasDefinition, defs[10])
      assert_instance_of(Rubydex::GlobalVariableAliasDefinition, defs[11])
    end
  end

  def test_definition_location
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class A
          def foo; end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      def_a = graph.documents.first.definitions.find { |d| d.name == "A" }
      refute_nil(def_a)
      location = def_a.location
      refute_nil(location)
      assert_equal(context.uri_to("file1.rb"), location.uri)
      assert_equal(context.absolute_path_to("file1.rb"), location.path)
      assert_equal(1, location.start_line)
      assert_equal(1, location.start_column)
      assert_equal(3, location.end_line)
      assert_equal(4, location.end_column)

      def_foo = graph.documents.first.definitions.find { |d| d.name == "foo" }
      refute_nil(def_foo)
      location = def_foo.location
      refute_nil(location)
      assert_equal(context.uri_to("file1.rb"), location.uri)
      assert_equal(context.absolute_path_to("file1.rb"), location.path)
      assert_equal(2, location.start_line)
      assert_equal(3, location.start_column)
      assert_equal(2, location.end_line)
      assert_equal(15, location.end_column)
    end
  end

  def test_definition_comments
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        # This is a class comment
        # Multi-line comment
        class Foo
          # Method comment
          def bar; end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      foo_comments = graph.documents.first.definitions.find { |d| d.name == "Foo" }.comments
      assert_equal(
        [
          "# This is a class comment (#{context.absolute_path_to("file1.rb")}:1:1-1:26)",
          "# Multi-line comment (#{context.absolute_path_to("file1.rb")}:2:1-2:21)",
        ],
        foo_comments.map { |c| "#{c.string} (#{normalized_comment_location(c)})" },
      )

      bar_comments = graph.documents.first.definitions.find { |d| d.name == "bar" }.comments
      assert_equal(
        ["# Method comment (#{context.absolute_path_to("file1.rb")}:4:3-4:19)"],
        bar_comments.map { |c| "#{c.string} (#{normalized_comment_location(c)})" },
      )
    end
  end

  private

  # Comment locations on Windows include the carriage return. This means that the end column is off by one when compared
  # to Unix locations. This method creates a fake adjusted location for Windows so that we can assert locations once
  def normalized_comment_location(comment)
    loc = comment.location
    return loc unless Gem.win_platform?

    Rubydex::Location.new(
      uri: loc.uri,
      start_line: loc.start_line,
      start_column: loc.start_column,
      end_line: loc.end_line,
      end_column: loc.end_column - 1,
    )
  end
end
