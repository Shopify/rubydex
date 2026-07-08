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

  def test_indexing_ruby_file_extensions
    with_context do |context|
      context.write!("foo.rb", "class Foo; end")
      context.write!("task.rake", "class Task; end")
      context.write!("config.ru", "class Config; end")
      context.write!("notes.txt", "class Notes; end")

      graph = Rubydex::Graph.new
      assert_empty(graph.index_all([context.absolute_path]))
      graph.resolve

      refute_nil(graph["Foo"])
      refute_nil(graph["Task"])
      refute_nil(graph["Config"])
      assert_nil(graph["Notes"])
    end
  end

  def test_indexing_invalid_file_paths
    graph = Rubydex::Graph.new

    errors = graph.index_all(["not_found.rb"])

    assert_equal(1, errors.length)
    assert_match(/FileError: Path `.*not_found.rb` does not exist/, errors.first)
  end

  def test_load_config_from_explicit_path_adds_exclusions
    with_context do |context|
      context.write!(".rubydex_custom", "exclude = [\"vendor\", \"generated\"]\n")

      graph = Rubydex::Graph.new(workspace_path: context.absolute_path)
      graph.load_config(".rubydex_custom")

      # Excluded paths are resolved against the workspace path.
      assert_includes(graph.excluded_patterns, context.absolute_path_to("vendor"))
      assert_includes(graph.excluded_patterns, context.absolute_path_to("generated"))
      assert_includes(graph.excluded_patterns, context.absolute_path_to("node_modules"))
    end
  end

  def test_load_config_without_argument_loads_the_default_rubydex
    with_context do |context|
      context.write!("rubydex.toml", "exclude = [\"vendor\", \"generated\"]\n")

      graph = Rubydex::Graph.new(workspace_path: context.absolute_path)
      graph.load_config

      assert_includes(graph.excluded_patterns, context.absolute_path_to("vendor"))
      assert_includes(graph.excluded_patterns, context.absolute_path_to("generated"))
      assert_includes(graph.excluded_patterns, context.absolute_path_to("node_modules"))
    end
  end

  def test_load_config_without_argument_ignores_a_missing_default_rubydex
    with_context do |context|
      graph = Rubydex::Graph.new(workspace_path: context.absolute_path)

      # A missing default `rubydex.toml` is not an error; defaults remain in place.
      graph.load_config
      assert_includes(graph.excluded_patterns, context.absolute_path_to("node_modules"))
    end
  end

  def test_load_config_raises_when_file_is_missing
    graph = Rubydex::Graph.new
    assert_raises(Rubydex::ConfigError) { graph.load_config(".rubydex_missing") }
  end

  def test_load_config_fails_if_argument_is_not_string
    graph = Rubydex::Graph.new
    assert_raises(TypeError) { graph.load_config(123) }
  end

  def test_indexing_with_parse_errors
    with_context do |context|
      context.write!("file.rb", "class Foo")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      assert_diagnostics(
        [
          { rule: :"parse-error", path: "file.rb", message: "expected an `end` to close the `class` statement" },
          { rule: :"parse-error", path: "file.rb", message: "unexpected end-of-input, assuming it is closing the parent top level context" },
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

  def test_graph_get_declaration_accepts_leading_double_colon
    with_context do |context|
      context.write!("file.rb", "module Foo; class Bar; end; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      # Built-ins are root-scoped, so `Object` and `::Object` must resolve to the same declaration.
      refute_nil(graph["::Object"])
      assert_equal(graph["Object"].name, graph["::Object"].name)

      # Same for indexed declarations, top-level and nested.
      refute_nil(graph["::Foo"])
      refute_nil(graph["::Foo::Bar"])
      assert_equal(graph["Foo::Bar"].name, graph["::Foo::Bar"].name)

      # Unknown names still return nil when prefixed.
      assert_nil(graph["::DoesNotExist"])
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

      # Object, Class, Module, BasicObject, Kernel + the indexed files
      assert_equal(7, enumerator.size)
      assert_equal(7, enumerator.count)
      assert_equal(7, enumerator.to_a.size)
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

      assert_equal(7, declarations.size)
    end
  end

  def test_graph_documents_enumerator
    with_context do |context|
      context.write!("file1.rb", "class A; end")
      context.write!("file2.rb", "class B; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))

      enumerator = graph.documents

      assert_equal(3, enumerator.size)
      assert_equal(3, enumerator.count)
      assert_equal(3, enumerator.to_a.size)
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

      assert_equal(3, documents.size)
    end
  end

  def test_graph_fuzzy_search
    with_context do |context|
      context.write!("foo.rb", "class Foo; end")
      context.write!("bar.rb", "class Bar; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      results = graph.fuzzy_search("Fo")
      assert_equal(["Foo"], results.map(&:name))
    end
  end

  def test_graph_search
    with_context do |context|
      context.write!("foo.rb", <<~RUBY)
        class Foo
          def is_a_foo?; end
        end

        class Bar < Foo
          def is_a?(other); end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      results = graph.search("#is_a?()")
      assert_equal(["Bar#is_a?()"], results.map(&:name))
    end
  end

  def test_graph_search_matches_any_of_multiple_queries
    with_context do |context|
      context.write!("foo.rb", <<~RUBY)
        class Foo
          def is_a_foo?; end
        end

        class Bar
          def is_a_bar?; end
        end

        class Baz
          def something_else; end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      results = graph.search("#is_a_foo?()", "#is_a_bar?()").map(&:name).sort
      assert_equal(["Bar#is_a_bar?()", "Foo#is_a_foo?()"], results)
    end
  end

  def test_graph_search_requires_at_least_one_query
    graph = Rubydex::Graph.new
    assert_raises(ArgumentError) { graph.search }
    assert_raises(ArgumentError) { graph.fuzzy_search }
  end

  def test_workspace_path_defaults_to_pwd
    graph = Rubydex::Graph.new
    assert_equal(Dir.pwd, File.expand_path(graph.workspace_path))
  end

  def test_workspace_path_can_be_passed_to_initialize
    with_context do |context|
      graph = Rubydex::Graph.new(workspace_path: context.absolute_path)
      assert_equal(context.absolute_path, graph.workspace_path)
    end
  end

  def test_workspace_path_setter
    graph = Rubydex::Graph.new
    graph.workspace_path = "/some/workspace"

    assert_equal("/some/workspace", graph.workspace_path)
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

  def test_graph_resolve_constant_with_empty_name
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

      assert_nil(graph.resolve_constant("", []))
      assert_nil(graph.resolve_constant("", ["Foo"]))
      assert_nil(graph.resolve_constant("", ["Foo", "Bar::Baz"]))
      assert_nil(graph.resolve_constant("::", []))
      assert_nil(graph.resolve_constant("Foo::", []))
      assert_nil(graph.resolve_constant("Foo::Bar::", ["Baz"]))
    end
  end

  def test_graph_resolve_constant_with_empty_nesting
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

      assert_equal("Bar", graph.resolve_constant("Bar", []).name)
      assert_equal("Foo", graph.resolve_constant("Foo", []).name)
    end
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

      assert_equal(3, graph.documents.count)
      foo = graph["Foo"]

      deleted = graph.delete_document(context.uri_to("foo.rb"))
      assert_instance_of(Rubydex::Document, deleted)

      # Existing reference to foo doesn't crash, but data is no longer available in the graph
      assert_empty(foo.definitions.to_a)
      assert_nil(graph["Foo"])

      assert_equal(2, graph.documents.count)
      bar_doc = graph.documents.find { |d| d.uri == context.uri_to("bar.rb") }
      assert_equal("Bar", bar_doc.definitions.first.name)
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

    assert_equal(2, graph.documents.count)
    refute_nil(graph["Foo"])
  end

  def test_index_source_with_rbs
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rbs", "class Foo\nend", "rbs")

    assert_equal(2, graph.documents.count)
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

    assert_equal(2, graph.documents.count)
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
      context.write!("top_level.rb", "class TopLevel; end")
      context.write!("top_level.rake", "class TopLevelRake; end")
      context.write!("top_level.rbs", "class TopLevelRbs; end")
      context.write!("config.ru", "class ConfigRu; end")

      graph = Rubydex::Graph.new(workspace_path: context.absolute_path)
      paths = graph.workspace_paths

      # Includes workspace directories
      assert_includes(paths, context.absolute_path_to("lib"))
      assert_includes(paths, context.absolute_path_to("app"))

      # Includes the top level files
      assert_includes(paths, context.absolute_path_to("top_level.rb"))
      assert_includes(paths, context.absolute_path_to("top_level.rake"))
      assert_includes(paths, context.absolute_path_to("top_level.rbs"))
      assert_includes(paths, context.absolute_path_to("config.ru"))

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

  def test_index_workspace_includes_user_defined_rbs_files
    with_context do |context|
      context.write!("sig/foo.rbs", <<~RBS)
        class Foo
        end
      RBS

      graph = Rubydex::Graph.new(workspace_path: context.absolute_path)
      graph.index_workspace
      graph.resolve

      assert_equal("Foo", graph["Foo"].name)
    end
  end

  def test_complete_expression
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", "class Foo\n  CONST = 1\n  def bar; end\nend", "ruby")
    graph.resolve

    candidates = graph.complete_expression(["Foo"], self_receiver: "Foo")

    # Declaration candidates
    constants = candidates.select { |c| c.is_a?(Rubydex::Constant) }
    assert(constants.any? { |c| c.name == "Foo::CONST" })

    methods = candidates.select { |c| c.is_a?(Rubydex::Method) }
    assert(methods.any? { |c| c.name == "Foo#bar()" })

    # Keyword candidates
    keywords = candidates.select { |c| c.is_a?(Rubydex::Keyword) }
    keyword_names = keywords.map(&:name)
    assert_includes(keyword_names, "if")
    assert_includes(keyword_names, "yield")

    # Keywords have documentation
    if_keyword = keywords.find { |c| c.name == "if" }
    refute_nil(if_keyword)
    assert_kind_of(String, if_keyword.documentation)
    refute_empty(if_keyword.documentation)
  end

  def test_complete_expression_inside_singleton_method_body
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", <<~RUBY, "ruby")
      $global_var = 1
      class Foo
        @ivar = 1
        @@class_var = 1

        class << self
          def bar
          end
        end
      end
    RUBY
    graph.resolve

    candidates = graph.complete_expression(["Foo", "<Foo>"], self_receiver: "Foo::<Foo>")

    # Singleton methods defined in the singleton class block
    methods = candidates.select { |c| c.is_a?(Rubydex::Method) }
    assert(methods.any? { |c| c.name == "Foo::<Foo>#bar()" })

    # Instance variables belong to the singleton class
    ivars = candidates.select { |c| c.is_a?(Rubydex::InstanceVariable) }
    assert(ivars.any? { |c| c.name == "Foo::<Foo>\#@ivar" })

    # Class variables from the attached object
    cvars = candidates.select { |c| c.is_a?(Rubydex::ClassVariable) }
    assert(cvars.any? { |c| c.name == "Foo\#@@class_var" })

    # Global variables are accessible everywhere
    globals = candidates.select { |c| c.is_a?(Rubydex::GlobalVariable) }
    assert(globals.any? { |c| c.name == "$global_var" })

    # Top-level constants are accessible
    declarations = candidates.select { |c| c.is_a?(Rubydex::Declaration) }
    assert(declarations.any? { |c| c.name == "Foo" })

    # Keywords are always available
    keywords = candidates.select { |c| c.is_a?(Rubydex::Keyword) }
    assert(keywords.any? { |c| c.name == "if" })
  end

  def test_complete_expression_with_singleton_method_receiver
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", <<~RUBY, "ruby")
      module Outer
        OUTER_CONST = 1
        @@outer_cvar = 0

        class Foo
          FOO_CONST = 2
          @@foo_cvar = 3
          @ivar = 4

          def instance_m; end
          def self.singleton_m; end
        end

        def Foo.bar
          # Completion inside here:
          # - self is Foo (the class), so methods/ivars come from Foo::<Foo>
          # - class variables follow LEXICAL scope [Outer], NOT self
          # - constants come from the lexical scope [Outer], NOT from Foo
        end
      end
    RUBY
    graph.resolve

    # Lexical nesting at `def Foo.bar` is [Outer]; self receiver is Foo::<Foo>
    candidates = graph.complete_expression(["Outer"], self_receiver: "Outer::Foo::<Foo>")

    # Methods: singleton methods defined on Foo (live on Foo::<Foo>)
    methods = candidates.select { |c| c.is_a?(Rubydex::Method) }
    method_names = methods.map(&:name)
    assert_includes(method_names, "Outer::Foo::<Foo>#singleton_m()")
    assert_includes(method_names, "Outer::Foo::<Foo>#bar()")
    # Instance methods of Foo should NOT be callable on the class itself
    refute_includes(method_names, "Outer::Foo#instance_m()")

    # Instance variables: class instance variables of Foo (stored on Foo::<Foo>)
    ivars = candidates.select { |c| c.is_a?(Rubydex::InstanceVariable) }
    assert(ivars.any? { |c| c.name == "Outer::Foo::<Foo>\#@ivar" })

    # Class variables follow lexical scope: Outer's cvars are visible, Foo's must NOT leak
    cvars = candidates.select { |c| c.is_a?(Rubydex::ClassVariable) }
    cvar_names = cvars.map(&:name)
    assert_includes(cvar_names, "Outer\#@@outer_cvar")
    refute_includes(cvar_names, "Outer::Foo\#@@foo_cvar")

    # Constants: from lexical scope [Outer], plus top-level. Foo::FOO_CONST must NOT leak.
    declarations = candidates.select { |c| c.is_a?(Rubydex::Declaration) }
    decl_names = declarations.map(&:name)
    assert_includes(decl_names, "Outer::OUTER_CONST")
    assert_includes(decl_names, "Outer::Foo")
    refute_includes(decl_names, "Outer::Foo::FOO_CONST")
  end

  def test_complete_expression_with_def_self_method
    # `def self.bar` inside a class: lexical nesting is [Foo] (NOT [Foo, <Foo>]),
    # but self is Foo::<Foo>.
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", <<~RUBY, "ruby")
      class Foo
        FOO_CONST = 1
        @@class_var = 2
        @ivar = 3

        def instance_m; end

        def self.bar
          # completion point
        end
      end
    RUBY
    graph.resolve

    candidates = graph.complete_expression(["Foo"], self_receiver: "Foo::<Foo>")

    # Methods: singleton methods only
    methods = candidates.select { |c| c.is_a?(Rubydex::Method) }
    method_names = methods.map(&:name)
    assert_includes(method_names, "Foo::<Foo>#bar()")
    refute_includes(method_names, "Foo#instance_m()")

    # Instance variables: from Foo::<Foo>
    ivars = candidates.select { |c| c.is_a?(Rubydex::InstanceVariable) }
    assert(ivars.any? { |c| c.name == "Foo::<Foo>\#@ivar" })

    # Class variables: from attached object Foo
    cvars = candidates.select { |c| c.is_a?(Rubydex::ClassVariable) }
    assert(cvars.any? { |c| c.name == "Foo\#@@class_var" })

    # Constants: FOO_CONST is visible because Foo IS in the lexical nesting
    declarations = candidates.select { |c| c.is_a?(Rubydex::Declaration) }
    decl_names = declarations.map(&:name)
    assert_includes(decl_names, "Foo::FOO_CONST")
    assert_includes(decl_names, "Foo")
  end

  def test_complete_method_argument_with_singleton_method_receiver
    # `def Foo.bar(x:)` — inside the argument list of a call from within this method.
    # self-type / nesting split must propagate through method_argument_completion too.
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", <<~RUBY, "ruby")
      module Outer
        OUTER_CONST = 1
        @@outer_cvar = 0

        class Foo
          FOO_CONST = 2
          @ivar = 3
          @@foo_cvar = 4

          def self.helper(name:); end
        end

        def Foo.bar
          # completion inside `Foo.helper(|)` here
        end
      end
    RUBY
    graph.resolve

    candidates = graph.complete_method_argument(
      "Outer::Foo::<Foo>#helper()",
      ["Outer"],
      self_receiver: "Outer::Foo::<Foo>",
    )

    # Keyword argument from the method being called
    keyword_params = candidates.select { |c| c.is_a?(Rubydex::KeywordParameter) }
    assert(keyword_params.any? { |c| c.name == "name" })

    # Everything expression_completion provides, with the correct self/nesting split:
    methods = candidates.select { |c| c.is_a?(Rubydex::Method) }
    method_names = methods.map(&:name)
    assert_includes(method_names, "Outer::Foo::<Foo>#helper()")
    assert_includes(method_names, "Outer::Foo::<Foo>#bar()")

    ivars = candidates.select { |c| c.is_a?(Rubydex::InstanceVariable) }
    assert(ivars.any? { |c| c.name == "Outer::Foo::<Foo>\#@ivar" })

    # Class variables follow lexical scope [Outer], not self (Foo::<Foo>)
    cvars = candidates.select { |c| c.is_a?(Rubydex::ClassVariable) }
    cvar_names = cvars.map(&:name)
    assert_includes(cvar_names, "Outer\#@@outer_cvar")
    refute_includes(cvar_names, "Outer::Foo\#@@foo_cvar")

    declarations = candidates.select { |c| c.is_a?(Rubydex::Declaration) }
    decl_names = declarations.map(&:name)
    assert_includes(decl_names, "Outer::OUTER_CONST")
    refute_includes(decl_names, "Outer::Foo::FOO_CONST")
  end

  def test_complete_expression_raises_on_empty_self_receiver
    graph = Rubydex::Graph.new
    assert_raises(ArgumentError) do
      graph.complete_expression(["Foo"], self_receiver: "")
    end
  end

  def test_complete_method_argument_raises_on_empty_self_receiver
    graph = Rubydex::Graph.new
    assert_raises(ArgumentError) do
      graph.complete_method_argument("Foo#bar()", ["Foo"], self_receiver: "")
    end
  end

  def test_complete_expression_raises_when_self_receiver_kwarg_missing
    graph = Rubydex::Graph.new
    assert_raises(ArgumentError) { graph.complete_expression(["Foo"]) }
  end

  def test_complete_expression_with_nil_self_receiver_skips_self_members
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", <<~RUBY, "ruby")
      class Foo
        CONST = 1
        @ivar = 2
        def instance_m; end
      end
    RUBY
    graph.resolve

    candidates = graph.complete_expression(["Foo"], self_receiver: nil)
    names = candidates.map(&:name)

    assert_includes(names, "Foo::CONST")
    refute_includes(names, "Foo#instance_m()")
    refute_includes(names, "Foo::<Foo>\#@ivar")
  end

  def test_complete_expression_raises_on_nonexistent_self_receiver
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", "class Foo; end", "ruby")
    graph.resolve

    assert_raises(ArgumentError) do
      graph.complete_expression(["Foo"], self_receiver: "Nonexistent")
    end
  end

  def test_complete_expression_raises_on_non_namespace_self_receiver
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", <<~RUBY, "ruby")
      class Foo
        def bar; end
      end
    RUBY
    graph.resolve

    # `Foo#bar()` is a Method declaration, not a Namespace and not a ConstantAlias.
    assert_raises(ArgumentError) do
      graph.complete_expression(["Foo"], self_receiver: "Foo#bar()")
    end
  end

  def test_complete_expression_raises_on_wrong_type_self_receiver
    graph = Rubydex::Graph.new
    assert_raises(TypeError) do
      graph.complete_expression(["Foo"], self_receiver: 42)
    end
  end

  def test_complete_method_argument_raises_on_wrong_type_self_receiver
    graph = Rubydex::Graph.new
    assert_raises(TypeError) do
      graph.complete_method_argument("Foo#bar()", ["Foo"], self_receiver: 42)
    end
  end

  def test_complete_expression_with_empty_nesting
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", "class Object; end\nclass Foo; end", "ruby")
    graph.resolve

    candidates = graph.complete_expression([], self_receiver: "Object")

    # Top-level constants should be reachable (Object context)
    constants = candidates.select { |c| c.is_a?(Rubydex::Declaration) }
    assert(constants.any? { |c| c.name == "Foo" })

    # Keywords should still be present
    keywords = candidates.select { |c| c.is_a?(Rubydex::Keyword) }
    assert(keywords.any? { |c| c.name == "if" })
  end

  def test_complete_expression_for_non_namespace_nesting
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", <<~RUBY, "ruby")
      class Foo
        def bar
        end
      end
    RUBY
    graph.resolve

    assert_raises(ArgumentError) do
      graph.complete_expression(["Foo#bar()"], self_receiver: nil)
    end
  end

  def test_complete_namespace_access
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", <<~RUBY, "ruby")
      class Foo
        CONST = 1

        class << self
          def bar; end
        end
      end
    RUBY
    graph.resolve

    candidates = graph.complete_namespace_access("Foo", self_receiver: nil)

    # All candidates should be Declaration subclasses (no keywords)
    candidates.each { |c| assert_kind_of(Rubydex::Declaration, c) }

    assert(candidates.any? { |c| c.is_a?(Rubydex::Constant) && c.name == "Foo::CONST" })
    assert(candidates.any? { |c| c.is_a?(Rubydex::Method) && c.name == "Foo::<Foo>#bar()" })
  end

  def test_completion_declaration_lookups_accept_leading_double_colon
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", <<~RUBY, "ruby")
      class Foo
        CONST = 1

        def instance_method; end
        def self.helper(name:); end
      end
    RUBY
    graph.resolve

    namespace_candidates = graph.complete_namespace_access("::Foo", self_receiver: "::Foo")
    assert(namespace_candidates.any? { |c| c.name == "Foo::CONST" })
    assert(namespace_candidates.any? { |c| c.name == "Foo::<Foo>#helper()" })

    method_candidates = graph.complete_method_call("::Foo", self_receiver: "::Foo").map(&:name)
    assert_includes(method_candidates, "Foo#instance_method()")

    expression_candidates = graph.complete_expression(["Foo"], self_receiver: "::Foo").map(&:name)
    assert_includes(expression_candidates, "Foo#instance_method()")

    argument_candidates = graph.complete_method_argument(
      "::Foo::<Foo>#helper()",
      ["Foo"],
      self_receiver: "::Foo::<Foo>",
    ).map(&:name)
    assert_includes(argument_candidates, "name")
  end

  def test_complete_namespace_access_for_non_namespace
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", <<~RUBY, "ruby")
      class Foo
        def bar
        end
      end
    RUBY
    graph.resolve

    assert_raises(ArgumentError) do
      graph.complete_namespace_access("Foo#bar()", self_receiver: nil)
    end
  end

  def test_complete_method_call
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", "class Foo\n  def bar; end\n  def baz; end\nend", "ruby")
    graph.resolve

    candidates = graph.complete_method_call("Foo", self_receiver: nil)

    # All candidates should be Method instances
    candidates.each { |c| assert_kind_of(Rubydex::Method, c) }

    method_names = candidates.map(&:name)
    assert_includes(method_names, "Foo#bar()")
    assert_includes(method_names, "Foo#baz()")
  end

  def test_complete_method_call_for_non_namespace
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", <<~RUBY, "ruby")
      class Foo
        def bar
        end
      end
    RUBY
    graph.resolve

    assert_raises(ArgumentError) do
      graph.complete_method_call("Foo#bar()", self_receiver: nil)
    end
  end

  def test_complete_method_call_excludes_private_method_from_external_context
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", <<~RUBY, "ruby")
      class Foo
        def public_one; end

        private

        def secret; end
      end
    RUBY
    graph.resolve

    external = graph.complete_method_call("Foo", self_receiver: nil).map(&:name)
    assert_includes(external, "Foo#public_one()")
    refute_includes(external, "Foo#secret()")

    internal = graph.complete_method_call("Foo", self_receiver: "Foo").map(&:name)
    assert_includes(internal, "Foo#public_one()")
    assert_includes(internal, "Foo#secret()")
  end

  def test_complete_method_call_includes_protected_method_when_caller_shares_class
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", <<~RUBY, "ruby")
      class Foo
        protected

        def shielded; end
      end

      class Bar < Foo
      end

      class Other
      end
    RUBY
    graph.resolve

    # Same class as receiver: protected access allowed.
    same_class = graph.complete_method_call("Foo", self_receiver: "Foo").map(&:name)
    assert_includes(same_class, "Foo#shielded()")

    # Subclass calling on its parent: caller and receiver both descend from Foo.
    subclass = graph.complete_method_call("Foo", self_receiver: "Bar").map(&:name)
    assert_includes(subclass, "Foo#shielded()")

    # Unrelated class: protected access denied.
    unrelated = graph.complete_method_call("Foo", self_receiver: "Other").map(&:name)
    refute_includes(unrelated, "Foo#shielded()")

    # External context: protected access denied.
    external = graph.complete_method_call("Foo", self_receiver: nil).map(&:name)
    refute_includes(external, "Foo#shielded()")
  end

  def test_complete_namespace_access_excludes_private_constant
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", <<~RUBY, "ruby")
      class Foo
        PUBLIC_CONST = 1
        SECRET = 2
        private_constant :SECRET
      end
    RUBY
    graph.resolve

    candidates = graph.complete_namespace_access("Foo", self_receiver: nil).map(&:name)
    assert_includes(candidates, "Foo::PUBLIC_CONST")
    refute_includes(candidates, "Foo::SECRET")
  end

  def test_complete_method_argument
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", "class Foo\n  def bar(name:); end\nend", "ruby")
    graph.resolve

    candidates = graph.complete_method_argument("Foo#bar()", ["Foo"], self_receiver: "Foo")

    # Method candidates
    methods = candidates.select { |c| c.is_a?(Rubydex::Method) }
    assert(methods.any? { |c| c.name == "Foo#bar()" })

    # Keyword candidates
    keywords = candidates.select { |c| c.is_a?(Rubydex::Keyword) }
    assert(keywords.any? { |c| c.name == "if" })

    # KeywordParameter candidates
    keyword_params = candidates.select { |c| c.is_a?(Rubydex::KeywordParameter) }
    assert(keyword_params.any? { |c| c.name == "name" })
  end

  def test_complete_method_argument_for_non_namespace_nesting
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", <<~RUBY, "ruby")
      class Foo
        def bar(name:)
        end
      end
    RUBY
    graph.resolve

    assert_raises(ArgumentError) do
      graph.complete_method_argument("Foo#bar()", ["Foo#bar()"], self_receiver: nil)
    end
  end

  def test_complete_expression_inside_class_body
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", <<~RUBY, "ruby")
      class Bar
        def self.baz; end
      end

      class Foo < Bar
        @class_level_ivar = 1
        @@class_var = 2

        def instance_method
          @instance_level_ivar = 3
        end
      end
    RUBY
    graph.resolve

    # Lexical scope is Foo, but the self type is Foo::<Foo> because it's within the body
    candidates = graph.complete_expression(["Foo"], self_receiver: "Foo::<Foo>").map(&:name)

    assert_includes(candidates, "Foo::<Foo>\#@class_level_ivar")
    assert_includes(candidates, "Foo\#@@class_var")
    assert_includes(candidates, "Bar::<Bar>#baz()")
    refute_includes(candidates, "Foo\#@instance_level_ivar")
  end

  def test_complete_expression_inside_singleton_class_block
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", <<~RUBY, "ruby")
      class Foo
        @class_level_ivar = 1

        class << self
          @singleton_level_ivar = 2
        end
      end
    RUBY
    graph.resolve

    candidates = graph.complete_expression(["Foo", "<Foo>"], self_receiver: "Foo::<Foo>::<<Foo>>").map(&:name)

    assert_includes(candidates, "Foo::<Foo>::<<Foo>>\#@singleton_level_ivar")
    refute_includes(candidates, "Foo::<Foo>\#@class_level_ivar")
  end

  def test_complete_method_call_for_singleton_method_from_class_body
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", <<~RUBY, "ruby")
      class Foo
        def self.helper
        end
      end
    RUBY
    graph.resolve

    # Calling `Foo.helper` from anywhere — we're querying methods on the singleton class <Foo>.
    candidates = graph.complete_method_call("Foo::<Foo>", self_receiver: nil)

    methods = candidates.select { |c| c.is_a?(Rubydex::Method) }
    assert(methods.any? { |c| c.name == "Foo::<Foo>#helper()" })
  end

  def test_complete_method_argument_inside_class_body
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", <<~RUBY, "ruby")
      class Foo
        @class_level_ivar = 1

        def self.helper(name:)
        end

        helper()
      end
    RUBY
    graph.resolve

    candidates = graph.complete_method_argument("Foo::<Foo>#helper()", ["Foo"], self_receiver: "Foo::<Foo>").map(&:name)

    assert_includes(candidates, "Foo::<Foo>\#@class_level_ivar")
    assert_includes(candidates, "Foo::<Foo>#helper()")
    assert_includes(candidates, "name")
    assert_includes(candidates, "if")
  end

  def test_complete_expression_raises_with_wrong_types
    graph = Rubydex::Graph.new
    assert_raises(TypeError) { graph.complete_expression("not an array", self_receiver: nil) }
    assert_raises(TypeError) { graph.complete_expression([123], self_receiver: nil) }
  end

  def test_complete_namespace_access_raises_with_wrong_types
    graph = Rubydex::Graph.new
    assert_raises(TypeError) { graph.complete_namespace_access(123) }
  end

  def test_complete_method_call_raises_with_wrong_types
    graph = Rubydex::Graph.new
    assert_raises(TypeError) { graph.complete_method_call(123) }
  end

  def test_complete_method_argument_raises_with_wrong_types
    graph = Rubydex::Graph.new
    assert_raises(TypeError) { graph.complete_method_argument(123, [], self_receiver: nil) }
    assert_raises(TypeError) { graph.complete_method_argument("Foo#bar()", "not an array", self_receiver: nil) }
    assert_raises(TypeError) { graph.complete_method_argument("Foo#bar()", [123], self_receiver: nil) }
  end

  def test_completion_returns_empty_for_non_existent_declarations
    graph = Rubydex::Graph.new
    graph.resolve

    assert_equal([], graph.complete_namespace_access("DoesNotExist", self_receiver: nil))
    assert_equal([], graph.complete_method_call("DoesNotExist", self_receiver: nil))
  end

  def test_complete_expression_for_non_existent_nesting
    graph = Rubydex::Graph.new
    graph.resolve

    assert_raises(ArgumentError) do
      graph.complete_expression(["NonExistent"], self_receiver: nil)
    end
  end

  def test_complete_expression_on_unresolved_graph
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", "class Foo; end", "ruby")

    # Nesting with a name that exists but hasn't been resolved
    assert_raises(ArgumentError) do
      graph.complete_expression(["Foo"], self_receiver: nil)
    end
  end

  def test_exclude_patterns_filters_nested_directories
    with_context do |context|
      context.write!("vendor/gems/foo.rb", "class Foo; end")
      context.write!("vendor/bundle/bar.rb", "class Bar; end")

      graph = Rubydex::Graph.new(workspace_path: context.absolute_path)
      graph.exclude_patterns(["vendor/bundle"])

      assert_includes(graph.excluded_patterns, context.absolute_path_to("vendor/bundle"))

      # vendor itself should be included since only vendor/bundle is excluded
      paths = graph.workspace_paths
      assert_includes(paths, context.absolute_path_to("vendor"))

      # But when we index, files inside vendor/bundle should be skipped
      graph.index_all(paths)
      graph.resolve

      refute_nil(graph["Foo"])
      assert_nil(graph["Bar"])
    end
  end

  def test_exclude_patterns_with_invalid_arguments
    graph = Rubydex::Graph.new

    assert_raises(TypeError) do
      graph.exclude_patterns(123)
    end

    assert_raises(TypeError) do
      graph.exclude_patterns(["/valid/path", 456])
    end
  end

  def test_default_ignored_directories_are_excluded
    with_context do |context|
      context.write!("node_modules/pkg/in_node_modules.rb", "class InNodeModules; end")
      context.write!("tmp/in_tmp.rb", "class InTmp; end")
      context.write!("lib/foo.rb", "class Foo; end")

      graph = Rubydex::Graph.new(workspace_path: context.absolute_path)
      graph.index_all(graph.workspace_paths)
      graph.resolve

      refute_nil(graph["Foo"])
      assert_nil(graph["InNodeModules"])
      assert_nil(graph["InTmp"])
    end
  end

  def test_accessing_keyword_information
    graph = Rubydex::Graph.new
    keyword = graph.keyword("break")

    assert_equal("break", keyword.name)
    assert_equal("Exits from a loop or block, optionally returning a value. Syntax: `break` or `break value`.", keyword.documentation)
  end

  def test_keyword_returns_nil_for_non_keyword
    graph = Rubydex::Graph.new
    assert_nil(graph.keyword("not_a_keyword"))
  end

  def test_keyword_returns_nil_for_empty_string
    graph = Rubydex::Graph.new
    assert_nil(graph.keyword(""))
  end

  def test_keyword_raises_type_error_for_non_string
    graph = Rubydex::Graph.new
    assert_raises(TypeError) { graph.keyword(123) }
    assert_raises(TypeError) { graph.keyword(nil) }
  end

  def test_document_returns_specific_document
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", <<~RUBY, "ruby")
      class Foo
      end
    RUBY

    document = graph.document("file:///foo.rb")
    assert_instance_of(Rubydex::Document, document)
    assert_equal(1, document.definitions.count)
  end

  def test_document_returns_nil_for_non_existing_uri
    graph = Rubydex::Graph.new
    assert_nil(graph.document("file:///non_existing.rb"))
  end

  def test_document_with_invalid_argument
    graph = Rubydex::Graph.new
    assert_raises(TypeError) { graph.document(123) }
  end

  def test_document_returns_nil_after_delete_document
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", "class Foo; end", "ruby")

    refute_nil(graph.document("file:///foo.rb"))

    graph.delete_document("file:///foo.rb")
    assert_nil(graph.document("file:///foo.rb"))
  end

  def test_document_returns_correct_document_with_multiple_documents
    graph = Rubydex::Graph.new
    graph.index_source("file:///foo.rb", "class Foo; end", "ruby")
    graph.index_source("file:///bar.rb", "class Bar; end", "ruby")
    graph.index_source("file:///baz.rb", "class Baz; end", "ruby")

    document = graph.document("file:///bar.rb")
    assert_instance_of(Rubydex::Document, document)
    assert_equal("file:///bar.rb", document.uri)
  end

  private

  def assert_diagnostics(expected, actual)
    assert_equal(
      expected,
      actual.sort_by { |d| [d.location, d.message] }
        .map { |d| { rule: d.rule, path: File.basename(d.location.to_file_path), message: d.message } },
    )
  end
end
