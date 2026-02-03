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
      assert_instance_of(Rubydex::Class, declaration)
      assert_kind_of(Rubydex::Namespace, declaration)
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
      assert_instance_of(Rubydex::Class, declaration)

      member_declaration = declaration.member("foo()")
      assert_instance_of(Rubydex::Method, member_declaration)
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

  def test_declaration_kinds_return_specialized_classes
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        module Foo; end
        class Bar
          @@my_class_var = 1

          def initialize
            @my_instance_var = 1
          end

          class << self
            def singleton_method; end
          end

          def instance_method; end
        end

        MY_CONSTANT = 1
        MyAlias = Bar
        $my_global = 1
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      # Module
      assert_instance_of(Rubydex::Module, graph["Foo"])
      assert_kind_of(Rubydex::Namespace, graph["Foo"])

      # Class
      assert_instance_of(Rubydex::Class, graph["Bar"])
      assert_kind_of(Rubydex::Namespace, graph["Bar"])

      # SingletonClass
      assert_instance_of(Rubydex::SingletonClass, graph["Bar::<Bar>"])
      assert_kind_of(Rubydex::Namespace, graph["Bar::<Bar>"])

      # Method
      assert_instance_of(Rubydex::Method, graph["Bar#instance_method()"])

      # Constant
      assert_instance_of(Rubydex::Constant, graph["MY_CONSTANT"])

      # ConstantAlias
      assert_instance_of(Rubydex::ConstantAlias, graph["MyAlias"])

      # GlobalVariable
      assert_instance_of(Rubydex::GlobalVariable, graph["$my_global"])

      # InstanceVariable
      assert_instance_of(Rubydex::InstanceVariable, graph["Bar\#@my_instance_var"])

      # ClassVariable
      assert_instance_of(Rubydex::ClassVariable, graph["Bar\#@@my_class_var"])

      # All should be Declarations
      [
        graph["Foo"],
        graph["Bar"],
        graph["Bar::<Bar>"],
        graph["Bar#instance_method()"],
        graph["MY_CONSTANT"],
        graph["MyAlias"],
        graph["$my_global"],
        graph["Bar\#@my_instance_var"],
        graph["Bar\#@@my_class_var"],
      ].each do |decl|
        assert_kind_of(Rubydex::Declaration, decl, "Expected #{decl.name} to be a Declaration")
      end
    end
  end
end
