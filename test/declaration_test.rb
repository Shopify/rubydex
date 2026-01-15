# frozen_string_literal: true

require "test_helper"
require "helpers/context"

class DeclarationTest < Minitest::Test
  include Test::Helpers::WithContext

  def test_instantiating_a_declaration_from_ruby_fails
    e = assert_raises(NoMethodError) do
      Rubydex::Declaration.new
    end

    assert_match(/private method .new. called for.* Rubydex::Declaration/, e.message)
  end

  def test_declaration_initialize_from_graph
    with_context do |context|
      context.write!("file1.rb", "class A; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      declaration = graph["A"]
      assert_instance_of(Rubydex::Declaration, declaration)
      assert_equal("A", declaration.name)
    end
  end

  def test_definitions_enumerator
    with_context do |context|
      context.write!("file1.rb", "class A; end")
      context.write!("file2.rb", "class A; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      declaration = graph.declarations.find { |decl| decl.name == "A" }
      refute_nil(declaration)

      enumerator = declaration.definitions
      assert_equal(2, enumerator.size)
      assert_equal(2, enumerator.count)
      assert_equal(2, enumerator.to_a.size)
    end
  end

  def test_definitions_with_block
    with_context do |context|
      context.write!("file1.rb", "class A; end")
      context.write!("file2.rb", "class A; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      declaration = graph.declarations.find { |decl| decl.name == "A" }
      refute_nil(declaration)

      definitions = []
      declaration.definitions do |definition|
        definitions << definition
      end

      assert_equal(2, definitions.size)
    end
  end

  def test_unqualified_name
    with_context do |context|
      context.write!("file1.rb", "module A; class B; end; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      decl_a = graph["A"]
      refute_nil(decl_a)
      assert_equal("A", decl_a.unqualified_name)

      decl_b = graph["A::B"]
      refute_nil(decl_b)
      assert_equal("B", decl_b.unqualified_name)
    end
  end

  def test_members_returns_methods_defined_in_class
    with_context do |context|
      context.write!("file.rb", <<~RUBY)
        class Post
          def save; end
          def validate; end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      post = graph["Post"]
      member_names = post.members.map(&:unqualified_name)
      assert_equal(2, member_names.size)
      assert_includes(member_names, "save")
      assert_includes(member_names, "validate")
    end
  end

  def test_members_returns_empty_for_class_with_no_methods
    with_context do |context|
      context.write!("file.rb", "class Post; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      post = graph["Post"]
      assert_equal(0, post.members.count)
    end
  end

  def test_members_returns_nested_constants
    with_context do |context|
      context.write!("file.rb", <<~RUBY)
        module Foo
          class Bar; end
          module Baz; end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      foo = graph["Foo"]
      member_names = foo.members.map(&:unqualified_name)
      assert_equal(2, member_names.size)
      assert_includes(member_names, "Bar")
      assert_includes(member_names, "Baz")
    end
  end

  def test_ancestors_returns_linearized_ancestor_chain
    with_context do |context|
      context.write!("file.rb", <<~RUBY)
        module Mixin; end
        class Parent; end
        class Child < Parent
          include Mixin
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      child = graph["Child"]
      ancestor_names = child.ancestors.map(&:name)

      # Linearized order: [Child, Mixin, Parent, Object]
      assert_includes(ancestor_names, "Child")
      assert_includes(ancestor_names, "Mixin")
      assert_includes(ancestor_names, "Parent")
    end
  end

  def test_ancestors_returns_empty_for_module_with_no_mixins
    with_context do |context|
      context.write!("file.rb", "module Standalone; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      standalone = graph["Standalone"]
      ancestor_names = standalone.ancestors.map(&:name)
      # Module's ancestors include itself
      assert_includes(ancestor_names, "Standalone")
    end
  end

  def test_ancestors_enumerator_has_size
    with_context do |context|
      context.write!("file.rb", "class Post; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      post = graph["Post"]
      assert(post.ancestors.size >= 1) # At least Post itself
    end
  end
end
