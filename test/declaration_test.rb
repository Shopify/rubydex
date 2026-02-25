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

  def test_descendants
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        module Foo; end
        module Bar; end

        class Parent; end
        class Child < Parent
          include Foo
          prepend Bar
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      assert_equal(["Child", "Parent"], graph["Parent"].descendants.map(&:name))
      assert_equal(["Child", "Foo"], graph["Foo"].descendants.map(&:name))
      assert_equal(["Child", "Bar"], graph["Bar"].descendants.map(&:name))
    end
  end

  def test_ancestors
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        module Foo; end
        module Bar; end

        class Parent; end
        class Child < Parent
          include Foo
          prepend Bar
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      child = graph["Child"]
      assert_equal(["Bar", "Child", "Foo", "Parent", "Object"], child.ancestors.map(&:name))
    end
  end

  def test_cyclic_ancestors
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        module Foo
          include Foo
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      foo = graph["Foo"]
      assert_equal(["Foo"], foo.ancestors.map(&:name))
    end
  end

  def test_finding_an_inherited_method_definition
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class Parent
          def foo; end
        end

        class Child < Parent; end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      child = graph["Child"]
      foo = nil

      child.ancestors.each do |decl|
        member = decl.member("foo()")
        foo = member if member
      end

      assert_equal("Parent#foo()", foo.name)
    end
  end

  def test_finding_a_method_only_inherited
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class Parent
          def foo; end
        end

        class Child < Parent
          def foo; end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      # Demonstrating how to get only the inherited version of a method (useful for go to definition on `super` calls)
      child = graph["Child"]
      foo = nil
      found_main_ancestor = false

      child.ancestors.each do |decl|
        if decl.name == child.name
          found_main_ancestor = true
        elsif !found_main_ancestor
          next
        end

        member = decl.member("foo()")
        foo = member if member
      end

      assert_equal("Parent#foo()", foo.name)
    end
  end

  def test_finding_an_inherited_instance_variable
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class Parent
          def initialize
            @name = "John"
          end
        end

        class Child < Parent; end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      child = graph["Child"]
      name = nil

      child.ancestors.each do |decl|
        member = decl.member("@name")
        name = member if member
      end

      assert_equal("Parent\#@name", name.name)
    end
  end

  def test_find_member_returns_inherited_members
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class Parent
          @@class_var = 1

          def initialize
            @name = "John"
          end
        end

        class Child < Parent
          def initialize
            super
          end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      child = graph["Child"]
      decl = child.find_member("@name")
      assert_equal("Parent\#@name", decl.name)

      decl = child.find_member("@@class_var")
      assert_equal("Parent\#@@class_var", decl.name)

      decl = child.find_member("initialize()", only_inherited: true)
      assert_equal("Parent#initialize()", decl.name)
    end
  end
end
