# frozen_string_literal: true

require "test_helper"
require "helpers/context"

class GraphTest < Minitest::Test
  include Test::Helpers::WithContext

  def test_indexing_empty_context
    with_context do |context|
      graph = Rubydex::Graph.new
      assert_empty(graph.index_all(context.glob("**/*.rb")))
    end
  end

  def test_indexing_context_files
    with_context do |context|
      context.write!("foo.rb", "class Foo; end")
      context.write!("bar.rb", "class Bar; end")

      graph = Rubydex::Graph.new
      assert_empty(graph.index_all(context.glob("**/*.rb")))
    end
  end

  def test_indexing_invalid_file_paths
    graph = Rubydex::Graph.new

    errors = graph.index_all(["not_found.rb"])

    assert_equal(1, errors.length)
    assert_match(/FileError: Path `.*not_found.rb` does not exist/, errors.first)
  end

  def test_indexing_with_parse_errors
    with_context do |context|
      context.write!("file.rb", "class Foo")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      assert_diagnostics(
        [
          { rule: "parse-error", path: "file.rb", message: "expected an `end` to close the `class` statement" },
          { rule: "parse-error", path: "file.rb", message: "unexpected end-of-input, assuming it is closing the parent top level context" },
        ],
        graph.diagnostics,
      )
    end
  end

  def test_passing_invalid_arguments_to_index_all
    graph = Rubydex::Graph.new

    assert_raises(TypeError) do
      graph.index_all("not an array")
    end

    assert_raises(TypeError) do
      graph.index_all([1, 2, 3])
    end
  end

  def test_graph_get_declaration
    with_context do |context|
      context.write!("file1.rb", "class A; end")
      context.write!("file2.rb", "class B; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      declaration = graph["A"]
      refute_nil(declaration)

      declaration = graph["B"]
      refute_nil(declaration)

      declaration = graph["C"]
      assert_nil(declaration)
    end
  end

  def test_list_all_declarations_enumerator
    with_context do |context|
      context.write!("file1.rb", "class A; end")
      context.write!("file2.rb", "class B; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      enumerator = graph.declarations

      # Object, Class, Module + the indexed files
      assert_equal(5, enumerator.size)
      assert_equal(5, enumerator.count)
      assert_equal(5, enumerator.to_a.size)
    end
  end

  def test_list_all_declarations_with_block
    with_context do |context|
      context.write!("file1.rb", "class A; end")
      context.write!("file2.rb", "class B; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      declarations = []
      graph.declarations do |declaration|
        declarations << declaration
      end

      assert_equal(5, declarations.size)
    end
  end

  def test_graph_documents_enumerator
    with_context do |context|
      context.write!("file1.rb", "class A; end")
      context.write!("file2.rb", "class B; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      enumerator = graph.documents

      assert_equal(2, enumerator.size)
      assert_equal(2, enumerator.count)
      assert_equal(2, enumerator.to_a.size)
    end
  end

  def test_graph_documents_with_block
    with_context do |context|
      context.write!("file1.rb", "class A; end")
      context.write!("file2.rb", "class B; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      documents = []
      graph.documents do |document|
        documents << document
      end

      assert_equal(2, documents.size)
    end
  end

  def test_graph_search
    with_context do |context|
      context.write!("foo.rb", "class Foo; end")
      context.write!("bar.rb", "class Bar; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      results = graph.search("Fo")
      assert_equal(["Foo"], results.map(&:name))
    end
  end

  def test_graph_encoding_setter
    with_context do |context|
      context.write!("foo.rb", <<~RUBY)
        class Foo
          def initialize
            @叫聲😍x = "喵"
          end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      foo = graph["Foo\#@叫聲😍x"].definitions.first

      # UTF-8: code units => number of bytes
      loc = foo.location.to_display
      assert_equal(3, loc.start_line)
      assert_equal(5, loc.start_column)
      assert_equal(3, loc.end_line)
      assert_equal(17, loc.end_column)

      # UTF-16: code units => 1 for 1,2 byte characters, 2 for 3,4 byte characters
      graph.encoding = "utf16"
      loc = foo.location.to_display
      assert_equal(3, loc.start_line)
      assert_equal(5, loc.start_column)
      assert_equal(3, loc.end_line)
      assert_equal(11, loc.end_column)

      # UTF-32: code units => 1 for all characters
      graph.encoding = "utf32"
      loc = foo.location.to_display
      assert_equal(3, loc.start_line)
      assert_equal(5, loc.start_column)
      assert_equal(3, loc.end_line)
      assert_equal(10, loc.end_column)
    end
  end

  def test_graph_encoding_setter_with_invalid_value
    graph = Rubydex::Graph.new

    error = assert_raises(ArgumentError) do
      graph.encoding = "invalid-encoding"
    end

    assert_match(/invalid encoding `invalid-encoding` \(should be utf8, utf16 or utf32\)/, error.message)

    assert_raises(TypeError) do
      graph.encoding = 123
    end
  end

  def test_graph_resolve_constant
    with_context do |context|
      context.write!("foo.rb", <<~RUBY)
        module Bar; end

        module Foo
          CONST = 123

          class Bar::Baz
            CONST
          end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      const = graph.resolve_constant("CONST", ["Foo", "Bar::Baz"])
      assert_equal("Foo::CONST", const.name)
    end
  end

  def test_graph_resolve_with_invalid_argument
    graph = Rubydex::Graph.new

    assert_raises(TypeError) do
      graph.resolve_constant(123, ["Foo", "Bar::Baz"])
    end

    assert_raises(TypeError) do
      graph.resolve_constant("CONST", ["Foo", 123])
    end

    assert_raises(TypeError) do
      graph.resolve_constant("CONST", "Not an array")
    end
  end

  def test_graph_resolve_non_existing_constant
    graph = Rubydex::Graph.new
    assert_nil(graph.resolve_constant("CONST", ["Foo", "Bar::Baz"]))
  end

  def test_graph_resolve_constant_alias
    with_context do |context|
      context.write!("foo.rb", <<~RUBY)
        module Foo
          CONST = 1
        end

        ALIAS = Foo
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      const = graph.resolve_constant("ALIAS::CONST", [])
      assert_equal("Foo::CONST", const.name)
    end
  end

  def test_graph_resolve_instance_variable
    with_context do |context|
      context.write!("foo.rb", <<~RUBY)
        module Bar; end

        module Foo
          class Bar::Baz
            def initialize
              @instance_var = 1
            end

            def Bar.something
              @singleton_var = 2
            end

            def self.other_thing
              @other_singleton_var = 3
            end
          end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      baz = graph.resolve_constant("Bar::Baz", ["Foo"])
      assert_equal("Bar::Baz", baz.name)
      assert_equal("Bar::Baz\#@instance_var", baz.member("@instance_var").name)

      bar = graph.resolve_constant("Bar", ["Foo", "Bar::Baz"])
      assert_equal("Bar", bar.name)
      assert_equal("Bar::<Bar>\#@singleton_var", bar.singleton_class.member("@singleton_var").name)

      baz_singleton = graph.resolve_constant("Bar::Baz", ["Foo", "Bar::Baz"])
      assert_equal("Bar::Baz", baz_singleton.name)
      assert_equal(
        "Bar::Baz::<Baz>\#@other_singleton_var",
        baz_singleton.singleton_class.member("@other_singleton_var").name,
      )
    end
  end

  def test_graph_resolve_class_variable
    with_context do |context|
      context.write!("foo.rb", <<~RUBY)
        module Bar; end

        module Foo
          class Bar::Baz
            def initialize
              @@class_var_1 = 1
            end

            def Bar.something
              @@class_var_2 = 2
            end

            def self.other_thing
              @@class_var_3 = 3
            end
          end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      baz = graph.resolve_constant("Bar::Baz", ["Foo"])
      assert_equal("Bar::Baz", baz.name)
      assert_equal("Bar::Baz\#@@class_var_1", baz.member("@@class_var_1").name)

      assert_equal("Bar::Baz\#@@class_var_2", baz.member("@@class_var_2").name)

      baz_singleton = graph.resolve_constant("Bar::Baz", ["Foo", "Bar::Baz"])
      assert_equal("Bar::Baz", baz_singleton.name)
      assert_equal(
        "Bar::Baz\#@@class_var_3",
        baz_singleton.member("@@class_var_3").name,
      )
    end
  end

  def test_resolve_require_path
    with_context do |context|
      context.write!("lib/foo/bar.rb", "class Bar; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      load_paths = [context.absolute_path_to("lib")]
      document = graph.resolve_require_path("foo/bar", load_paths)

      assert_instance_of(Rubydex::Document, document)
      assert(document.uri.end_with?("lib/foo/bar.rb"))
    end
  end

  def test_require_paths
    with_context do |context|
      context.write!("lib1/foo/bar.rb", "class Bar1; end")
      context.write!("lib1/baz/qux.rb", "class Qux; end")
      context.write!("lib2/foo/bar.rb", "class Bar2; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      # Returns all require paths, deduplicated by load path order
      load_path = [context.absolute_path_to("lib1"), context.absolute_path_to("lib2")]
      results = graph.require_paths(load_path)

      assert_equal(["baz/qux", "foo/bar"], results.sort)

      assert_empty(graph.require_paths([]))
    end
  end

  def test_delete_uri_removes_document_and_definitions
    with_context do |context|
      context.write!("foo.rb", "class Foo; end")
      context.write!("bar.rb", "class Bar; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      assert_equal(2, graph.documents.count)
      foo = graph["Foo"]

      deleted = graph.delete_document(context.uri_to("foo.rb"))
      assert_instance_of(Rubydex::Document, deleted)

      # Existing reference to foo doesn't crash, but data is no longer available in the graph
      assert_empty(foo.definitions.to_a)
      assert_nil(graph["Foo"])

      assert_equal(1, graph.documents.count)
      assert_equal("Bar", graph.documents.first.definitions.first.name)
    end
  end

  def test_delete_uri_with_non_existing_uri
    graph = Rubydex::Graph.new
    assert_nil(graph.delete_document("file:///non_existing.rb"))
  end

  def test_delete_uri_with_invalid_argument
    graph = Rubydex::Graph.new

    assert_raises(TypeError) do
      graph.delete_document(123)
    end
  end

  def test_index_source_with_ruby
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", "class Foo; end", "ruby")
    graph.resolve

    assert_equal(1, graph.documents.count)
    refute_nil(graph["Foo"])
  end

  def test_index_source_with_rbs
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rbs", "class Foo\nend", "rbs")

    assert_equal(1, graph.documents.count)
  end

  def test_index_source_with_unknown_language_id
    graph = Rubydex::Graph.new

    error = assert_raises(ArgumentError) do
      graph.index_source("file:///foo.py", "class Foo: pass", "python")
    end

    assert_match(/unsupported language_id `python`/, error.message)
  end

  def test_index_source_with_invalid_arguments
    graph = Rubydex::Graph.new

    assert_raises(TypeError) do
      graph.index_source(123, "class Foo; end", "ruby")
    end

    assert_raises(TypeError) do
      graph.index_source("file:///foo.rb", 123, "ruby")
    end

    assert_raises(TypeError) do
      graph.index_source("file:///foo.rb", "class Foo; end", 123)
    end
  end

  def test_index_source_replaces_existing_document
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", "class Foo; end", "ruby")
    graph.resolve

    refute_nil(graph["Foo"])
    assert_nil(graph["Bar"])

    graph.index_source("file:///foo.rb", "class Bar; end", "ruby")
    graph.resolve

    assert_equal(1, graph.documents.count)
    assert_nil(graph["Foo"])
    refute_nil(graph["Bar"])
  end

  def test_index_source_with_invalid_source_encoding
    graph = Rubydex::Graph.new
    error = assert_raises(ArgumentError) do
      graph.index_source("file:///test.rb", "\xFF\xFE".b, "ruby")
    end
    assert_match(/source is not valid UTF-8/, error.message)
  end

  def test_index_source_with_null_bytes_in_source
    # Edge case supported by Prism. The `\0` cannot be confused with a string termination null byte
    graph = Rubydex::Graph.new
    source = "%\0abc\0"
    graph.index_source("file:///test.rb", source, "ruby")
  end

  def test_require_paths_with_invalid_arguments
    graph = Rubydex::Graph.new

    assert_raises(TypeError) do
      graph.require_paths("not an array")
    end

    assert_raises(TypeError) do
      graph.require_paths([1, 2, 3])
    end
  end

  def test_workspace_paths
    with_context do |context|
      context.write!("lib/foo.rb", "class Foo; end")
      context.write!("app/bar.rb", "class Bar; end")
      context.write!(".git/config", "")
      context.write!("node_modules/pkg/index.js", "")
      context.write!("top_level.rb", "class TopLevel; end")

      graph = Rubydex::Graph.new(workspace_path: context.absolute_path)
      paths = graph.workspace_paths

      # Includes workspace directories
      assert_includes(paths, context.absolute_path_to("lib"))
      assert_includes(paths, context.absolute_path_to("app"))

      # Excludes ignored directories
      refute_includes(paths, context.absolute_path_to(".git"))
      refute_includes(paths, context.absolute_path_to("node_modules"))

      # Includes the top level files
      assert_includes(paths, context.absolute_path_to("top_level.rb"))

      # Includes gem dependency paths from Bundler
      gem_require_paths = Bundler.locked_gems.specs.flat_map do |lazy_spec|
        spec = Gem::Specification.find_by_name(lazy_spec.name)
        spec.require_paths.reject { |path| File.absolute_path?(path) }
      rescue Gem::MissingSpecError
        []
      end

      gem_require_paths.each do |require_path|
        assert(paths.any? { |path| path.end_with?(require_path) }, "Expect workspace paths to include dependency require path `#{require_path}`")
      end

      assert_equal(paths.length, paths.uniq.length)
    end
  end

  def test_index_workspace_includes_rbs_core_definitions
    # Ensure the `rbs` gem is installed on CI
    Gem.install("rbs") if ENV["CI"]

    graph = Rubydex::Graph.new
    graph.index_workspace
    graph.resolve

    ["Kernel", "Object", "BasicObject", "Integer"].each do |core_namespace|
      rbs_kernel = graph[core_namespace].definitions.find do |definition|
        uri = URI(definition.location.uri)
        File.extname(uri.path) == ".rbs"
      end
      assert(rbs_kernel, "Expected to find RBS definition for `#{core_namespace}` in the graph")
    end
  end

  private

  def assert_diagnostics(expected, actual)
    assert_equal(
      expected,
      actual.sort_by { |d| [d.location, d.message] }
        .map { |d| { rule: d.rule, path: File.basename(d.location.path), message: d.message } },
    )
  end
end
