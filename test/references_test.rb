# frozen_string_literal: true

require "test_helper"
require "helpers/context"

class ReferencesTest < Minitest::Test
  include Test::Helpers::WithContext

  def test_graph_constant_references_enumerator
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class A; end
        class B < A
          def foo
            puts B
          end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      enumerator = graph.constant_references

      assert_equal(6, enumerator.size)
      assert_equal(6, enumerator.count)
      assert_equal(6, enumerator.to_a.size)
    end
  end

  def test_graph_constant_references_with_block
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class A; end
        class B < A
          def foo
            puts B
          end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      references = []
      graph.constant_references do |unresolved_reference|
        references << unresolved_reference
      end
      assert_equal(6, references.size)

      references.sort_by!(&:location)

      ref1, ref2 = references

      assert_kind_of(Rubydex::ConstantReference, ref1)
      assert_kind_of(Rubydex::UnresolvedConstantReference, ref1)
      assert_equal("A", ref1.name)
      assert_equal("#{context.absolute_path_to("file1.rb")}:2:11-2:12", ref1.location.to_display.to_s)

      assert_kind_of(Rubydex::ConstantReference, ref2)
      assert_kind_of(Rubydex::UnresolvedConstantReference, ref2)
      assert_equal("B", ref2.name)
      assert_equal("#{context.absolute_path_to("file1.rb")}:4:10-4:11", ref2.location.to_display.to_s)
    end
  end

  def test_graph_resolved_constant_references
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class A; end
        class B < A
          def foo
            puts B
          end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      file_uri = context.uri_to("file1.rb")
      references = graph.constant_references.to_a
        .select { |r| r.location.uri == file_uri }
        .sort_by(&:location)
      assert_equal(2, references.size)

      ref1, ref2 = references

      assert_kind_of(Rubydex::ConstantReference, ref1)
      assert_kind_of(Rubydex::ResolvedConstantReference, ref1)
      assert_equal("#{context.absolute_path_to("file1.rb")}:2:11-2:12", ref1.location.to_display.to_s)

      decl1 = ref1.declaration
      assert_kind_of(Rubydex::Class, decl1)
      assert_equal("A", decl1.name)

      assert_kind_of(Rubydex::ResolvedConstantReference, ref2)
      assert_equal("#{context.absolute_path_to("file1.rb")}:4:10-4:11", ref2.location.to_display.to_s)

      decl2 = ref2.declaration
      assert_kind_of(Rubydex::Class, decl2)
      assert_equal("B", decl2.name)
    end
  end

  def test_declaration_references_are_resolved
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class A; end
        class B < A; end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      decl_a = graph["A"]
      refs = decl_a.references.to_a
      assert_equal(1, refs.size)

      ref = refs.first
      assert_kind_of(Rubydex::ResolvedConstantReference, ref)
      assert_kind_of(Rubydex::ConstantReference, ref)
      assert_equal("A", ref.declaration.name)
    end
  end

  def test_resolved_reference_becomes_unresolved_after_removing_declaration
    with_context do |context|
      context.write!("a.rb", "class A; end")
      context.write!("b.rb", "class B < A; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      b_uri = context.uri_to("b.rb")
      refs = graph.constant_references.to_a.select { |r| r.location.uri == b_uri }
      assert_equal(1, refs.size)
      assert_kind_of(Rubydex::ResolvedConstantReference, refs.first)

      graph.delete_document(context.uri_to("a.rb"))
      graph.resolve

      refs = graph.constant_references.to_a.select { |r| r.location.uri == b_uri }
      assert_equal(1, refs.size)
      assert_kind_of(Rubydex::UnresolvedConstantReference, refs.first)
      assert_equal("A", refs.first.name)
    end
  end

  def test_constant_references_returns_mix_of_resolved_and_unresolved
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class A; end
        class B < A
          def foo
            Unknown
          end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      file_uri = context.uri_to("file1.rb")
      references = graph.constant_references.to_a
        .select { |r| r.location.uri == file_uri }
        .sort_by(&:location)

      resolved = references.select { |r| r.is_a?(Rubydex::ResolvedConstantReference) }
      unresolved = references.select { |r| r.is_a?(Rubydex::UnresolvedConstantReference) }

      assert_equal(1, resolved.size)
      assert_equal(1, unresolved.size)

      assert_kind_of(Rubydex::ConstantReference, resolved.first)
      assert_equal("A", resolved.first.declaration.name)

      assert_kind_of(Rubydex::ConstantReference, unresolved.first)
      assert_equal("Unknown", unresolved.first.name)
    end
  end

  def test_constant_references_to_aliases
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class Foo; end
        MyAlias = Foo

        class Bar
          def baz
            Foo
            MyAlias
          end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      references = graph.constant_references.to_a.sort_by(&:location)

      foo_ref = references.find { |r| r.is_a?(Rubydex::ResolvedConstantReference) && r.declaration.name == "Foo" }
      alias_ref = references.find { |r| r.is_a?(Rubydex::ResolvedConstantReference) && r.declaration.name == "MyAlias" }

      refute_nil(foo_ref)
      assert_kind_of(Rubydex::Class, foo_ref.declaration)

      refute_nil(alias_ref)
      assert_kind_of(Rubydex::ConstantAlias, alias_ref.declaration)

      assert_equal("MyAlias", alias_ref.declaration.references.first.declaration.name)
    end
  end

  def test_graph_method_references_enumerator
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class A; end
        class B < A
          def foo
            puts B
          end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      enumerator = graph.method_references

      assert_equal(1, enumerator.size)
      assert_equal(1, enumerator.count)
      assert_equal(1, enumerator.to_a.size)
    end
  end

  def test_graph_method_references_with_block
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class A; end
        class B < A
          def foo
            puts B
          end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      references = []
      graph.method_references do |unresolved_reference|
        references << unresolved_reference
      end
      assert_equal(1, references.size)

      references.sort_by!(&:location)

      ref1 = references[0]
      assert_kind_of(Rubydex::MethodReference, ref1)
      assert_equal("puts", ref1.name)
      assert_equal("#{context.absolute_path_to("file1.rb")}:4:5-4:9", ref1.location.to_display.to_s)
    end
  end
end
