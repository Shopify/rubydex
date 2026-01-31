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

  def test_declaration_member
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class A
          def foo; end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      declaration = graph["A"]
      assert_instance_of(Rubydex::Declaration, declaration)

      member_declaration = declaration.member("foo()")
      assert_instance_of(Rubydex::Declaration, member_declaration)
      assert_equal("A#foo()", member_declaration.name)
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

  def test_singleton_class
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        module Foo
          class Bar
            class << self
              def something; end
            end
          end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      assert_nil(graph["Foo"].singleton_class)

      bar = graph["Foo::Bar"]
      assert_equal("Foo::Bar::<Bar>", bar.singleton_class.name)
    end
  end

  def test_declaration_owner
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        module Foo
          class Bar
            class << self
              def something; end
            end
          end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      assert_equal("Object", graph["Foo"].owner.name)
      assert_equal("Foo", graph["Foo::Bar"].owner.name)
      assert_equal("Foo::Bar", graph["Foo::Bar::<Bar>"].owner.name)
      assert_equal("Foo::Bar::<Bar>", graph["Foo::Bar::<Bar>#something()"].owner.name)
    end
  end
end
