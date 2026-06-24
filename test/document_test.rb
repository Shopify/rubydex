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

      document = graph.documents.find { |d| d.uri == context.uri_to("file1.rb") }
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

      document = graph.documents.find { |d| d.uri == context.uri_to("file1.rb") }
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

      document = graph.documents.find { |d| d.uri == context.uri_to("file1.rb") }
      refute_nil(document)

      definitions = []
      document.definitions do |definition|
        definitions << definition
      end

      assert_equal(2, definitions.size)
    end
  end

  def test_method_references_enumerator
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class A
          def foo
            bar
          end
        end
      RUBY

      context.write!("file2.rb", <<~RUBY)
        class B
          def foo
            baz
          end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      document = graph.documents.find { |d| d.uri == context.uri_to("file1.rb") }
      refute_nil(document)

      enumerator = document.method_references
      assert_equal(1, enumerator.size)
      assert_equal(1, enumerator.count)
      assert_equal(1, enumerator.to_a.size)
      assert_equal(["bar"], enumerator.map(&:name))

      other_document = graph.documents.find { |d| d.uri == context.uri_to("file2.rb") }
      refute_nil(other_document)
      assert_equal(["baz"], other_document.method_references.map(&:name))
    end
  end

  def test_method_references_with_block
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class A
          def foo
            bar
          end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      document = graph.documents.find { |d| d.uri == context.uri_to("file1.rb") }
      refute_nil(document)

      references = []
      document.method_references do |reference|
        references << reference
      end

      assert_equal(1, references.size)

      reference = references.first
      assert_kind_of(Rubydex::MethodReference, reference)
      assert_equal("bar", reference.name)
      assert_equal("#{context.absolute_path_to("file1.rb")}:3:5-3:8", reference.location.to_display.to_s)
    end
  end
end
