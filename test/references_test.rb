# frozen_string_literal: true

require "test_helper"
require "helpers/context"

class ReferencesTest < Minitest::Test
  include Test::Helpers::WithContext

  def test_graph_unresolved_references_enumerator
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

      enumerator = graph.unresolved_references

      assert_equal(3, enumerator.size)
      assert_equal(3, enumerator.count)
      assert_equal(3, enumerator.to_a.size)
    end
  end

  def test_graph_unresolved_references_with_block
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

      unresolved_references = []
      graph.unresolved_references do |unresolved_reference|
        unresolved_references << unresolved_reference
      end
      assert_equal(3, unresolved_references.size)

      unresolved_references.sort_by!(&:location)

      ref1 = unresolved_references[0]

      assert_kind_of(Saturn::UnresolvedConstantReference, ref1)
      assert_equal("A", ref1.name)
      assert_equal("#{context.absolute_path_to("file1.rb")}:2:11-2:12", ref1.location.to_s)

      ref2 = unresolved_references[1]
      assert_kind_of(Saturn::UnresolvedMethodReference, ref2)
      assert_equal("puts", ref2.name)
      assert_equal("#{context.absolute_path_to("file1.rb")}:4:5-4:9", ref2.location.to_s)

      ref3 = unresolved_references[2]
      assert_kind_of(Saturn::UnresolvedConstantReference, ref3)
      assert_equal("B", ref3.name)
      assert_equal("#{context.absolute_path_to("file1.rb")}:4:10-4:11", ref3.location.to_s)
    end
  end
end
