# frozen_string_literal: true

require "test_helper"
require "helpers/context"

class DocumentTest < Minitest::Test
  include Test::Helpers::WithContext

  def test_instantiating_a_document_from_ruby_fails
    e = assert_raises(NoMethodError) do
      Rubydex::Document.new
    end

    assert_match(/private method .new. called for.* Rubydex::Document/, e.message)
  end

  def test_document_initialize_from_graph
    with_context do |context|
      context.write!("file1.rb", "class A; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      document = graph.documents.first
      assert_instance_of(Rubydex::Document, document)
      assert_equal(context.uri_to("file1.rb"), document.uri)
    end
  end

  def test_definitions_enumerator
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class A; end
        class B; end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      document = graph.documents.first
      refute_nil(document)

      enumerator = document.definitions
      assert_equal(2, enumerator.size)
      assert_equal(2, enumerator.count)
      assert_equal(2, enumerator.to_a.size)
    end
  end

  def test_definitions_with_block
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class A; end
        class B; end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      document = graph.documents.first
      refute_nil(document)

      definitions = []
      document.definitions do |definition|
        definitions << definition
      end

      assert_equal(2, definitions.size)
    end
  end
end
