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

      name_location = def_foo.name_location.to_display
      refute_nil(name_location)
      assert_equal(context.uri_to("file1.rb"), name_location.uri)
      assert_equal(2, name_location.start_line)
      assert_equal(7, name_location.start_column)
      assert_equal(2, name_location.end_line)
      assert_equal(10, name_location.end_column)
    end
  end

  def test_definition_document
    with_context do |context|
      context.write!("file1.rb", "class A; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      document = graph.documents.find { |doc| doc.uri == context.uri_to("file1.rb") }
      definition = document.definitions.find { |defn| defn.name == "A" }
      refute_nil(definition)

      assert_instance_of(Rubydex::Document, definition.document)
      assert_equal(context.uri_to("file1.rb"), definition.document.uri)
    end
  end

  def test_definition_document_raises_when_definition_is_gone
    with_context do |context|
      context.write!("file1.rb", "class A; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      document = graph.documents.find { |doc| doc.uri == context.uri_to("file1.rb") }
      definition = document.definitions.find { |defn| defn.name == "A" }
      refute_nil(definition)

      graph.delete_document(context.uri_to("file1.rb"))

      error = assert_raises(RuntimeError) { definition.document }
      assert_equal("Definition not found", error.message)
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

  def test_method_definition_signatures_ruby
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class Foo
          def bar(a, b = 1, *c, d, e:, f: 1, **g, &h); end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      method_def = graph["Foo#bar()"].definitions.first

      sigs = method_def.signatures
      assert_equal(1, sigs.length)

      sig = sigs[0]
      assert_equal(8, sig.parameters.length)

      path = context.absolute_path_to("file1.rb")

      sig.parameters[0].tap do |param|
        assert_instance_of(Rubydex::Signature::PositionalParameter, param)
        assert_equal(:a, param.name)
        assert_equal("#{path}:2:11-2:12", param.location.to_display.to_s)
      end

      sig.parameters[1].tap do |param|
        assert_instance_of(Rubydex::Signature::OptionalPositionalParameter, param)
        assert_equal(:b, param.name)
        assert_equal("#{path}:2:14-2:15", param.location.to_display.to_s)
      end

      sig.parameters[2].tap do |param|
        assert_instance_of(Rubydex::Signature::RestPositionalParameter, param)
        assert_equal(:c, param.name)
        assert_equal("#{path}:2:22-2:23", param.location.to_display.to_s)
      end

      sig.parameters[3].tap do |param|
        assert_instance_of(Rubydex::Signature::PostParameter, param)
        assert_equal(:d, param.name)
        assert_equal("#{path}:2:25-2:26", param.location.to_display.to_s)
      end

      sig.parameters[4].tap do |param|
        assert_instance_of(Rubydex::Signature::KeywordParameter, param)
        assert_equal(:e, param.name)
        assert_equal("#{path}:2:28-2:29", param.location.to_display.to_s)
      end

      sig.parameters[5].tap do |param|
        assert_instance_of(Rubydex::Signature::OptionalKeywordParameter, param)
        assert_equal(:f, param.name)
        assert_equal("#{path}:2:32-2:33", param.location.to_display.to_s)
      end

      sig.parameters[6].tap do |param|
        assert_instance_of(Rubydex::Signature::RestKeywordParameter, param)
        assert_equal(:g, param.name)
        assert_equal("#{path}:2:40-2:41", param.location.to_display.to_s)
      end

      sig.parameters[7].tap do |param|
        assert_instance_of(Rubydex::Signature::BlockParameter, param)
        assert_equal(:h, param.name)
        assert_equal("#{path}:2:44-2:45", param.location.to_display.to_s)
      end
    end
  end

  def test_method_definition_signatures_singleton_method
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class Foo
          def self.bar(...); end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      sig = graph["Foo::<Foo>#bar()"].definitions.first.signatures[0]
      assert_equal(1, sig.parameters.length)

      sig.parameters[0].tap do |param|
        assert_instance_of(Rubydex::Signature::ForwardParameter, param)
        assert_equal(:"...", param.name)
        path = context.absolute_path_to("file1.rb")
        assert_equal("#{path}:2:16-2:19", param.location.to_display.to_s)
      end
    end
  end

  def test_method_definition_signatures_rbs
    with_context do |context|
      context.write!("foo.rbs", <<~RBS)
        class Foo
          def baz: (Integer a, ?String b, *Symbol c, Integer d, name: String, ?age: Integer, **String rest) { () -> void } -> void
        end
      RBS

      path = context.absolute_path_to("foo.rbs")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.{rb,rbs}"))
      graph.resolve

      method_def = graph["Foo#baz()"].definitions.first

      assert_equal("#{path}:2:7-2:10", method_def.name_location.to_display.to_s)
      assert_equal(1, method_def.signatures.length)

      sig = method_def.signatures[0]

      sig.parameters[0].tap do |param|
        assert_instance_of(Rubydex::Signature::PositionalParameter, param)
        assert_equal(:a, param.name)
        assert_equal("#{path}:2:21-2:22", param.location.to_display.to_s)
      end

      sig.parameters[1].tap do |param|
        assert_instance_of(Rubydex::Signature::OptionalPositionalParameter, param)
        assert_equal(:b, param.name)
        assert_equal("#{path}:2:32-2:33", param.location.to_display.to_s)
      end

      sig.parameters[2].tap do |param|
        assert_instance_of(Rubydex::Signature::RestPositionalParameter, param)
        assert_equal(:c, param.name)
        assert_equal("#{path}:2:43-2:44", param.location.to_display.to_s)
      end

      sig.parameters[3].tap do |param|
        assert_instance_of(Rubydex::Signature::PostParameter, param)
        assert_equal(:d, param.name)
        assert_equal("#{path}:2:54-2:55", param.location.to_display.to_s)
      end

      sig.parameters[4].tap do |param|
        assert_instance_of(Rubydex::Signature::KeywordParameter, param)
        assert_equal(:name, param.name)
        assert_equal("#{path}:2:57-2:61", param.location.to_display.to_s)
      end

      sig.parameters[5].tap do |param|
        assert_instance_of(Rubydex::Signature::OptionalKeywordParameter, param)
        assert_equal(:age, param.name)
        assert_equal("#{path}:2:72-2:75", param.location.to_display.to_s)
      end

      sig.parameters[6].tap do |param|
        assert_instance_of(Rubydex::Signature::RestKeywordParameter, param)
        assert_equal(:rest, param.name)
        assert_equal("#{path}:2:95-2:99", param.location.to_display.to_s)
      end

      sig.parameters[7].tap do |param|
        assert_instance_of(Rubydex::Signature::BlockParameter, param)
        assert_equal(:block, param.name)
        assert_equal("#{path}:2:101-2:115", param.location.to_display.to_s)
      end
    end
  end

  def test_method_definition_signatures_rbs_singleton_method
    with_context do |context|
      context.write!("foo.rbs", <<~RBS)
        class Foo
          def self.bar: (Integer a) -> void
        end
      RBS

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.{rb,rbs}"))
      graph.resolve

      method_def = graph["Foo::<Foo>#bar()"].definitions.first

      sig = method_def.signatures[0]
      assert_equal(1, sig.parameters.length)

      sig.parameters[0].tap do |param|
        assert_instance_of(Rubydex::Signature::PositionalParameter, param)
        assert_equal(:a, param.name)
        path = context.absolute_path_to("foo.rbs")
        assert_equal("#{path}:2:26-2:27", param.location.to_display.to_s)
      end
    end
  end

  def test_method_definition_signatures_rbs_overload
    with_context do |context|
      context.write!("foo.rbs", <<~RBS)
        class Foo
          def foo: (String a) -> void
                 | (Integer b) -> void
        end
      RBS

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.{rb,rbs}"))
      graph.resolve

      method_def = graph["Foo#foo()"].definitions.first

      assert_equal(2, method_def.signatures.length)

      path = context.absolute_path_to("foo.rbs")

      method_def.signatures[0].tap do |sig|
        assert_equal(1, sig.parameters.length)
        sig.parameters[0].tap do |param|
          assert_instance_of(Rubydex::Signature::PositionalParameter, param)
          assert_equal(:a, param.name)
          assert_equal("#{path}:2:20-2:21", param.location.to_display.to_s)
        end
      end

      method_def.signatures[1].tap do |sig|
        assert_equal(1, sig.parameters.length)
        sig.parameters[0].tap do |param|
          assert_instance_of(Rubydex::Signature::PositionalParameter, param)
          assert_equal(:b, param.name)
          assert_equal("#{path}:3:21-3:22", param.location.to_display.to_s)
        end
      end
    end
  end

  def test_method_definition_signatures_rbs_untyped_parameters
    with_context do |context|
      context.write!("foo.rbs", <<~RBS)
        class Foo
          def baz: (?) -> void
        end
      RBS

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.{rb,rbs}"))
      graph.resolve

      method_def = graph["Foo#baz()"].definitions.first

      assert_equal(1, method_def.signatures.length)

      # It currently translates to 0 parameter signature
      assert_empty(method_def.signatures[0].parameters)
    end
  end

  def test_method_alias_definition_signatures
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class Foo
          def foo(a, b); end
          alias bar foo
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      alias_def = graph["Foo#bar()"].definitions.first
      assert_instance_of(Rubydex::MethodAliasDefinition, alias_def)

      signatures = alias_def.signatures
      assert_equal(1, signatures.length)

      params = signatures.first.parameters
      assert_equal(2, params.length)
      assert_equal(:a, params[0].name)
      assert_equal(:b, params[1].name)
    end
  end

  def test_method_alias_definition_signatures_unresolved
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class Foo
          alias bar nonexistent
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      alias_def = graph["Foo#bar()"].definitions.first
      assert_instance_of(Rubydex::MethodAliasDefinition, alias_def)

      signatures = alias_def.signatures
      assert_empty(signatures)
    end
  end

  def test_definition_lexical_owner_and_lexical_nesting
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class Foo
          class Bar
            class Baz; end
          end
        end

        class Foo::Qux
          module Quux
            def hello; end
          end
        end

        class Foo
          Class.new do
            def world; end
          end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      definitions = graph.documents.find { |d| d.uri == context.uri_to("file1.rb") }.definitions

      foo_def = definitions.find { |d| d.name == "Foo" }
      assert_nil(foo_def.lexical_owner)
      assert_lexical_nesting_equal([], foo_def)

      bar_def = definitions.find { |d| d.name == "Bar" }
      assert_same_definition(foo_def, bar_def.lexical_owner)
      assert_lexical_nesting_equal(["Foo"], bar_def)

      baz_def = definitions.find { |d| d.name == "Baz" }
      assert_same_definition(bar_def, baz_def.lexical_owner)
      assert_lexical_nesting_equal(["Foo::Bar", "Foo"], baz_def)

      hello_def = definitions.find { |d| d.name == "hello()" }
      assert_lexical_nesting_equal(["Foo::Qux::Quux", "Foo::Qux"], hello_def)
      assert_instance_of(Rubydex::ModuleDefinition, hello_def.lexical_owner)

      world_def = definitions.find { |d| d.name == "world()" }
      assert_lexical_nesting_equal([/<anonymous>\z/, "Foo"], world_def)
    end
  end

  def test_definition_lexical_owner_for_absolute_constant_path
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class Foo
          class ::Bar; end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      definitions = graph.documents.find { |d| d.uri == context.uri_to("file1.rb") }.definitions

      foo_def = definitions.find { |d| d.name == "Foo" }
      bar_def = definitions.find { |d| d.name == "Bar" }

      assert_equal("Bar", bar_def.declaration.name)
      assert_same_definition(foo_def, bar_def.lexical_owner)
      assert_lexical_nesting_equal(["Foo"], bar_def)
    end
  end

  def test_definition_declaration
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class Foo
          def bar; end
        end

        module M; end
        FOO = 1
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      defs = graph.documents.find { |d| d.uri == context.uri_to("file1.rb") }.definitions

      class_def = defs.find { |d| d.name == "Foo" }
      method_def = defs.find { |d| d.name == "bar()" }
      module_def = defs.find { |d| d.name == "M" }
      const_def = defs.find { |d| d.name == "FOO" }

      # Before resolution, declarations do not exist yet
      assert_nil(class_def.declaration)
      assert_nil(method_def.declaration)
      assert_nil(module_def.declaration)
      assert_nil(const_def.declaration)

      graph.resolve

      class_decl = class_def.declaration
      assert_instance_of(Rubydex::Class, class_decl)
      assert_equal("Foo", class_decl.name)

      method_decl = method_def.declaration
      assert_instance_of(Rubydex::Method, method_decl)
      assert_equal("Foo#bar()", method_decl.name)

      module_decl = module_def.declaration
      assert_instance_of(Rubydex::Module, module_decl)
      assert_equal("M", module_decl.name)

      const_decl = const_def.declaration
      assert_instance_of(Rubydex::Constant, const_decl)
      assert_equal("FOO", const_decl.name)
    end
  end

  def test_method_alias_definition_target
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class Foo
          def foo(a, b); end
          alias bar foo
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      alias_def = graph["Foo#bar()"].definitions.first
      assert_instance_of(Rubydex::MethodAliasDefinition, alias_def)

      target = alias_def.target
      assert_instance_of(Rubydex::Method, target)
      assert_equal("Foo#foo()", target.name)
    end
  end

  def test_method_alias_definition_target_through_chain
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class Foo
          def foo; end
          alias bar foo
          alias baz bar
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      alias_def = graph["Foo#baz()"].definitions.first
      assert_instance_of(Rubydex::MethodAliasDefinition, alias_def)

      target = alias_def.target
      assert_instance_of(Rubydex::Method, target)
      assert_equal("Foo#foo()", target.name)
    end
  end

  def test_method_alias_definition_target_unresolved_returns_nil
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class Foo
          alias bar nonexistent
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      alias_def = graph["Foo#bar()"].definitions.first
      assert_instance_of(Rubydex::MethodAliasDefinition, alias_def)
      assert_nil(alias_def.target)
    end
  end

  def test_method_alias_definition_target_raises_on_cycle
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class Foo
          alias a b
          alias b a
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      alias_def = graph["Foo#a()"].definitions.first
      assert_instance_of(Rubydex::MethodAliasDefinition, alias_def)

      assert_raises(Rubydex::AliasCycleError) { alias_def.target }
    end
  end

  private

  def assert_same_definition(expected, actual)
    assert_equal(expected.name, actual.name)
    assert_equal(expected.location.to_display, actual.location.to_display)
  end

  def assert_lexical_nesting_equal(expected, definition)
    actual = definition.lexical_nesting.map { |nesting| nesting.declaration.name }

    assert_equal(expected.size, actual.size)
    expected.zip(actual).each do |expected_name, actual_name|
      if expected_name.is_a?(Regexp)
        assert_match(expected_name, actual_name)
      else
        assert_equal(expected_name, actual_name)
      end
    end
  end

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
