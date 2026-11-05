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

  def test_unattached_comment_groups
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        class Foo
          # attached to bar
          def bar; end

          #: type rel = Shop::PrivateRelation
          #: type other = Shop::OtherRelation

          # dynamic receiver comments are not attached to a definition
          def receiver.foo; end

          module Bar
            # comment on a module_function definition
            module_function def baz; end
          end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      document = graph.documents.find { |doc| doc.uri == context.uri_to("file1.rb") }
      assert_equal(
        [
          ["#: type rel = Shop::PrivateRelation", "#: type other = Shop::OtherRelation"],
          ["# dynamic receiver comments are not attached to a definition"],
        ],
        document.comments.map { |group| group.map(&:string) },
      )

      bar = document.definitions.find { |definition| definition.name == "bar()" }
      assert_equal(["# attached to bar"], bar.comments.map(&:string))
      baz = document.definitions.find { |definition| definition.name == "baz()" }
      assert_equal(["# comment on a module_function definition"], baz.comments.map(&:string))
      assert_instance_of(Rubydex::Comment, document.comments.first.first)
      assert_equal(5, document.comments.first.first.location.to_display.start_line)
    end
  end

  def test_comments_is_empty_when_all_comments_are_attached
    with_context do |context|
      context.write!("file1.rb", <<~RUBY)
        # attached to Foo
        class Foo; end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      document = graph.documents.find { |doc| doc.uri == context.uri_to("file1.rb") }
      assert_empty(document.comments)
    end
  end

  def test_comments_is_empty_for_an_empty_document
    with_context do |context|
      context.write!("file1.rb", "")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      document = graph.documents.find { |doc| doc.uri == context.uri_to("file1.rb") }
      assert_empty(document.comments)
    end
  end

  def test_unattached_comments_can_be_read_repeatedly
    with_context do |context|
      context.write!("file1.rb", "# unattached\n")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      document = graph.documents.find { |doc| doc.uri == context.uri_to("file1.rb") }
      first = document.comments
      GC.start
      second = document.comments

      assert_equal(["# unattached"], first.flatten.map(&:string))
      assert_equal(["# unattached"], second.flatten.map(&:string))
      assert_equal(1, first.first.first.location.to_display.start_line)
      assert_equal(1, second.first.first.location.to_display.start_line)
    end
  end

  def test_trailing_inline_comment_is_not_attached_to_the_next_definition
    with_context do |context|
      context.write!("file1.rb", "def first; end # belongs to first\ndef second; end\n")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      document = graph.documents.find { |doc| doc.uri == context.uri_to("file1.rb") }
      second = document.definitions.find { |definition| definition.name == "second()" }

      assert_empty(second.comments)
      assert_equal([["# belongs to first"]], document.comments.map { |group| group.map(&:string) })
    end
  end

  def test_unattached_comments_preserve_nul_bytes
    with_context do |context|
      context.write!("file1.rb", "# unattached before\0after\n")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      document = graph.documents.find { |doc| doc.uri == context.uri_to("file1.rb") }
      assert_equal(["# unattached before\0after"], document.comments.flatten.map(&:string))
    end
  end

  def test_unattached_comment_memory_is_freed_when_ruby_construction_raises
    with_context do |context|
      context.write!("file1.rb", "# unattached\n")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      document = graph.documents.find { |doc| doc.uri == context.uri_to("file1.rb") }

      comment_class = Rubydex::Comment
      original_initialize = comment_class.instance_method(:initialize)
      original_verbose = $VERBOSE
      begin
        $VERBOSE = nil
        comment_class.define_method(:initialize) { |**_kwargs| raise "comment construction failed" }
        $VERBOSE = original_verbose

        error = assert_raises(RuntimeError) { document.comments }
        assert_equal("comment construction failed", error.message)
      ensure
        $VERBOSE = nil
        comment_class.define_method(:initialize, original_initialize) if original_initialize
        $VERBOSE = original_verbose
      end
    end
  end
end
