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
    assert_raises(NoMethodError) { Rubydex::ConstantAliasDefinition.new }
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
        ALIAS = M
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
      assert_instance_of(Rubydex::ConstantAliasDefinition, defs[6])
      assert_instance_of(Rubydex::ConstantDefinition, defs[7])
      assert_instance_of(Rubydex::MethodDefinition, defs[8])
      assert_instance_of(Rubydex::GlobalVariableDefinition, defs[9])
      assert_instance_of(Rubydex::InstanceVariableDefinition, defs[10])
      assert_instance_of(Rubydex::MethodAliasDefinition, defs[11])
      assert_instance_of(Rubydex::GlobalVariableAliasDefinition, defs[12])
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

      def_a = graph.documents.find { |d| d.uri == context.uri_to("file1.rb") }.definitions.find { |d| d.name == "A" }
      refute_nil(def_a)
      location = def_a.location.to_display
      refute_nil(location)
      assert_equal(context.uri_to("file1.rb"), location.uri)
      assert_equal(context.absolute_path_to("file1.rb"), location.to_file_path)
      assert_equal(1, location.start_line)
      assert_equal(1, location.start_column)
      assert_equal(3, location.end_line)
      assert_equal(4, location.end_column)

      def_foo = graph.documents.find { |d| d.uri == context.uri_to("file1.rb") }.definitions.find { |d| d.name == "foo()" }
      refute_nil(def_foo)
      location = def_foo.location.to_display
      refute_nil(location)
      assert_equal(context.uri_to("file1.rb"), location.uri)
      assert_equal(context.absolute_path_to("file1.rb"), location.to_file_path)
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

      foo_comments = graph.documents.find { |d| d.uri == context.uri_to("file1.rb") }.definitions.find { |d| d.name == "Foo" }.comments
      assert_equal(
        [
          "# This is a class comment (#{context.absolute_path_to("file1.rb")}:1:1-1:26)",
          "# Multi-line comment (#{context.absolute_path_to("file1.rb")}:2:1-2:21)",
        ],
        foo_comments.map { |c| "#{c.string} (#{normalized_comment_location(c)})" },
      )

      bar_comments = graph.documents.find { |d| d.uri == context.uri_to("file1.rb") }.definitions.find { |d| d.name == "bar()" }.comments
      assert_equal(
        ["# Method comment (#{context.absolute_path_to("file1.rb")}:4:3-4:19)"],
        bar_comments.map { |c| "#{c.string} (#{normalized_comment_location(c)})" },
      )
    end
  end

  def test_definition_deprecated
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        # @deprecated
        class Deprecated; end

        class NotDeprecated; end

        # Multi-line comment
        # @deprecated Use something else
        def deprecated_method; end

        # @deprecated
        # more comment
        def also_deprecated_method; end

        # Not @deprecated
        def not_deprecated_method; end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      document = graph.documents.find { |d| d.uri == context.uri_to("file1.rb") }
      definitions = document.definitions

      assert(definitions.find { |d| d.name == "Deprecated" }.deprecated?)
      refute(definitions.find { |d| d.name == "NotDeprecated" }.deprecated?)
      assert(definitions.find { |d| d.name == "deprecated_method()" }.deprecated?)
      assert(definitions.find { |d| d.name == "also_deprecated_method()" }.deprecated?)
      refute(definitions.find { |d| d.name == "not_deprecated_method()" }.deprecated?)
    end
  end

  def test_definition_deprecated_newlines
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        # @deprecated

        class DeprecatedWithBlank; end

        # @deprecated Use something else

        class DeprecatedWithMessage; end

        # Multi-line comment
        # @deprecated

        def deprecated_method; end

        # @deprecated
        # more comment

        def also_deprecated_method; end

        # Not @deprecated
        class NotDeprecated; end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      document = graph.documents.find { |d| d.uri == context.uri_to("file1.rb") }
      definitions = document.definitions

      assert(definitions.find { |d| d.name == "DeprecatedWithBlank" }.deprecated?)
      assert(definitions.find { |d| d.name == "DeprecatedWithMessage" }.deprecated?)
      assert(definitions.find { |d| d.name == "deprecated_method()" }.deprecated?)
      assert(definitions.find { |d| d.name == "also_deprecated_method()" }.deprecated?)
      refute(definitions.find { |d| d.name == "NotDeprecated" }.deprecated?)
    end
  end

  def test_class_definition_superclass
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class Parent; end
        class Child < Parent; end
        class NoSuperclass; end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      defs = graph.documents.find { |d| d.uri == context.uri_to("file1.rb") }.definitions

      # Before resolution, the superclass should be an unresolved constant reference
      child_def = defs.find { |d| d.name == "Child" }
      superclass_ref = child_def.superclass
      assert_instance_of(Rubydex::UnresolvedConstantReference, superclass_ref)

      # A class with no superclass returns nil
      no_super_def = defs.find { |d| d.name == "NoSuperclass" }
      assert_nil(no_super_def.superclass)

      # After resolution, the superclass should be a resolved constant reference
      graph.resolve
      superclass_ref = child_def.superclass
      assert_instance_of(Rubydex::ResolvedConstantReference, superclass_ref)
      assert_equal("Parent", superclass_ref.declaration.name)
    end
  end

  def test_class_definition_mixins
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        module M1; end
        module M2; end
        module M3; end

        class WithMixins
          include M1
          prepend M2
          extend M3
        end

        class NoMixins; end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      defs = graph.documents.find { |d| d.uri == context.uri_to("file1.rb") }.definitions

      # No mixins returns empty array
      no_mixins_def = defs.find { |d| d.name == "NoMixins" }
      assert_empty(no_mixins_def.mixins)

      # Before resolution, mixins have unresolved constant references in insertion order
      with_mixins_def = defs.find { |d| d.name == "WithMixins" }
      mixins = with_mixins_def.mixins
      assert_equal(3, mixins.length)

      assert_instance_of(Rubydex::Include, mixins[0])
      assert_instance_of(Rubydex::UnresolvedConstantReference, mixins[0].constant_reference)

      assert_instance_of(Rubydex::Prepend, mixins[1])
      assert_instance_of(Rubydex::UnresolvedConstantReference, mixins[1].constant_reference)

      assert_instance_of(Rubydex::Extend, mixins[2])
      assert_instance_of(Rubydex::UnresolvedConstantReference, mixins[2].constant_reference)

      # After resolution, mixins have resolved constant references
      graph.resolve
      mixins = with_mixins_def.mixins

      assert_instance_of(Rubydex::Include, mixins[0])
      assert_instance_of(Rubydex::ResolvedConstantReference, mixins[0].constant_reference)
      assert_equal("M1", mixins[0].constant_reference.declaration.name)

      assert_instance_of(Rubydex::Prepend, mixins[1])
      assert_instance_of(Rubydex::ResolvedConstantReference, mixins[1].constant_reference)
      assert_equal("M2", mixins[1].constant_reference.declaration.name)

      assert_instance_of(Rubydex::Extend, mixins[2])
      assert_instance_of(Rubydex::ResolvedConstantReference, mixins[2].constant_reference)
      assert_equal("M3", mixins[2].constant_reference.declaration.name)
    end
  end

  def test_module_definition_mixins
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        module M1; end
        module WithMixins
          include M1
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      defs = graph.documents.find { |d| d.uri == context.uri_to("file1.rb") }.definitions
      mod_def = defs.find { |d| d.name == "WithMixins" }
      mixins = mod_def.mixins

      assert_equal(1, mixins.length)
      assert_instance_of(Rubydex::Include, mixins[0])
      assert_equal("M1", mixins[0].constant_reference.declaration.name)
    end
  end

  def test_module_extend_self_mixins
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        module M
          extend self
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      defs = graph.documents.find { |d| d.uri == context.uri_to("file1.rb") }.definitions
      mod_def = defs.find { |d| d.is_a?(Rubydex::ModuleDefinition) }
      refute_nil(mod_def)
      mixins = mod_def.mixins

      assert_equal(1, mixins.length)
      assert_instance_of(Rubydex::Extend, mixins[0])
      assert_equal("M", mixins[0].constant_reference.declaration.name)
    end
  end

  def test_singleton_class_definition_mixins
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        module M; end

        class Foo
          class << self
            include M
          end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      defs = graph.documents.find { |d| d.uri == context.uri_to("file1.rb") }.definitions
      singleton_def = defs.find { |d| d.is_a?(Rubydex::SingletonClassDefinition) }
      refute_nil(singleton_def)
      mixins = singleton_def.mixins

      assert_equal(1, mixins.length)
      assert_instance_of(Rubydex::Include, mixins[0])
      assert_equal("M", mixins[0].constant_reference.declaration.name)
    end
  end

  private

  # Comment locations on Windows include the carriage return. This means that the end column is off by one when compared
  # to Unix locations. This method creates a fake adjusted location for Windows so that we can assert locations once
  def normalized_comment_location(comment)
    loc = comment.location.to_display
    return loc unless Gem.win_platform?

    Rubydex::DisplayLocation.new(
      uri: loc.uri,
      start_line: loc.start_line,
      start_column: loc.start_column,
      end_line: loc.end_line,
      end_column: loc.end_column - 1,
    )
  end
end
