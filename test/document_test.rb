# frozen_string_literal: true

require "test_helper"
require "helpers/context"

class DocumentTest < Minitest::Test
  include Test::Helpers::WithContext

  def test_instantiating_a_document_from_ruby_fails
    e = assert_raises(NoMethodError) do
      Saturn::Document.new
    end

    assert_equal("undefined method 'new' for class Saturn::Document", e.message)
  end

  def test_document_initialize_from_graph
    with_context do |context|
      context.write!("file1.rb", "class A; end")

      graph = Saturn::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      document = graph.documents.first
      assert_instance_of(Saturn::Document, document)
    end
  end
end
