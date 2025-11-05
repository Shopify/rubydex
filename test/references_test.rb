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

      graph = Saturn::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      enumerator = graph.constant_references

      assert_equal(2, enumerator.size)
      assert_equal(2, enumerator.count)
      assert_equal(2, enumerator.to_a.size)
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

      graph = Saturn::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      references = []
      graph.constant_references do |unresolved_reference|
        references << unresolved_reference
      end
      assert_equal(2, references.size)

      references.sort_by!(&:location)

      ref1, ref2 = references

      assert_kind_of(Saturn::ConstantReference, ref1)
      assert_equal("A", ref1.name)
      assert_equal("#{context.absolute_path_to("file1.rb")}:2:11-2:12", ref1.location.to_s)

      assert_kind_of(Saturn::ConstantReference, ref2)
      assert_equal("B", ref2.name)
      assert_equal("#{context.absolute_path_to("file1.rb")}:4:10-4:11", ref2.location.to_s)
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

      graph = Saturn::Graph.new
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

      graph = Saturn::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      references = []
      graph.method_references do |unresolved_reference|
        references << unresolved_reference
      end
      assert_equal(1, references.size)

      references.sort_by!(&:location)

      ref1 = references[0]
      assert_kind_of(Saturn::MethodReference, ref1)
      assert_equal("puts", ref1.name)
      assert_equal("#{context.absolute_path_to("file1.rb")}:4:5-4:9", ref1.location.to_s)
    end
  end
end
