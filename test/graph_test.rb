# frozen_string_literal: true

require "test_helper"
require "helpers/context"

class GraphTest < Minitest::Test
  include Test::Helpers::WithContext

  def test_indexing_empty_context
    with_context do |context|
      graph = Rubydex::Graph.new
      assert_nil(graph.index_all(context.glob("**/*.rb")))
    end
  end

  def test_indexing_context_files
    with_context do |context|
      context.write!("foo.rb", "class Foo; end")
      context.write!("bar.rb", "class Bar; end")

      graph = Rubydex::Graph.new
      assert_nil(graph.index_all(context.glob("**/*.rb")))
    end
  end

  def test_indexing_invalid_file_paths
    graph = Rubydex::Graph.new

    error = assert_raises(Rubydex::IndexingError) do
      graph.index_all(["not_found.rb"])
    end

    assert_kind_of(Rubydex::Error, error)
    assert_match(/FileError: Path `.*not_found.rb` does not exist/, error.message)
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

  def test_graph_set_encoding
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

      foo = graph["Foo::@叫聲😍x"].definitions.first

      # UTF-8: code units => number of bytes
      loc = foo.location
      assert_equal(3, loc.start_line)
      assert_equal(5, loc.start_column)
      assert_equal(3, loc.end_line)
      assert_equal(17, loc.end_column)

      # UTF-16: code units => 1 for 1,2 byte characters, 2 for 3,4 byte characters
      graph.set_encoding("utf16")
      loc = foo.location
      assert_equal(3, loc.start_line)
      assert_equal(5, loc.start_column)
      assert_equal(3, loc.end_line)
      assert_equal(11, loc.end_column)

      # UTF-32: code units => 1 for all characters
      graph.set_encoding("utf32")
      loc = foo.location
      assert_equal(3, loc.start_line)
      assert_equal(5, loc.start_column)
      assert_equal(3, loc.end_line)
      assert_equal(10, loc.end_column)
    end
  end

  def test_graph_set_encoding_with_invalid_value
    graph = Rubydex::Graph.new

    error = assert_raises(ArgumentError) do
      graph.set_encoding("invalid-encoding")
    end

    assert_match(/invalid encoding `invalid-encoding` \(should be utf8, utf16 or utf32\)/, error.message)

    assert_raises(TypeError) do
      graph.set_encoding(123)
    end
  end

  def test_add_method_creates_synthetic_method
    with_context do |context|
      context.write!("post.rb", "class Post; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      # Post starts with 0 members
      post = graph["Post"]
      assert_equal(0, post.members.count)

      # Add synthetic method
      result = graph.add_method(
        owner: "Post",
        name: "author",
        file_path: context.absolute_path_to("post.rb"),
        line: 1,
        column: 0,
      )
      assert(result)

      graph.resolve

      # Now Post has 1 member
      assert_equal(1, post.members.count)
      assert_equal("author", post.members.first.unqualified_name)
    end
  end

  def test_add_method_getter_and_setter
    with_context do |context|
      context.write!("post.rb", "class Post; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      file_path = context.absolute_path_to("post.rb")
      graph.add_method(owner: "Post", name: "author", file_path: file_path, line: 1, column: 0)
      graph.add_method(owner: "Post", name: "author=", file_path: file_path, line: 1, column: 0)
      graph.resolve

      post = graph["Post"]
      member_names = post.members.map(&:unqualified_name)
      assert_equal(2, member_names.size)
      assert_includes(member_names, "author")
      assert_includes(member_names, "author=")
    end
  end

  def test_add_method_to_nonexistent_owner_returns_false
    with_context do |context|
      context.write!("post.rb", "class Post; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      result = graph.add_method(
        owner: "NonExistent",
        name: "foo",
        file_path: context.absolute_path_to("post.rb"),
        line: 1,
        column: 0,
      )
      refute(result)
    end
  end

  def test_add_class_creates_synthetic_class
    with_context do |context|
      context.write!("spec.rb", "# RSpec file")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      # Class doesn't exist yet
      assert_nil(graph["RSpec::ExampleGroups::Calculator"])

      result = graph.add_class(
        name: "RSpec::ExampleGroups::Calculator",
        file_path: context.absolute_path_to("spec.rb"),
        line: 1,
        column: 0,
      )
      assert(result)

      graph.resolve

      # Now it exists
      calc = graph["RSpec::ExampleGroups::Calculator"]
      refute_nil(calc)
      assert_equal("Calculator", calc.unqualified_name)
    end
  end

  def test_add_class_with_parent
    with_context do |context|
      context.write!("spec.rb", "class RSpec::Core::ExampleGroup; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      graph.add_class(
        name: "RSpec::ExampleGroups::Calculator",
        parent: "RSpec::Core::ExampleGroup",
        file_path: context.absolute_path_to("spec.rb"),
        line: 1,
        column: 0,
      )
      graph.resolve

      calc = graph["RSpec::ExampleGroups::Calculator"]
      refute_nil(calc)
      assert_equal("Calculator", calc.unqualified_name)
    end
  end

  def test_add_module_creates_synthetic_module
    with_context do |context|
      context.write!("concern.rb", "module MyConcern; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      # Module doesn't exist yet
      assert_nil(graph["MyConcern::ClassMethods"])

      result = graph.add_module(
        name: "MyConcern::ClassMethods",
        file_path: context.absolute_path_to("concern.rb"),
        line: 1,
        column: 0,
      )
      assert(result)

      graph.resolve

      # Now it exists
      class_methods = graph["MyConcern::ClassMethods"]
      refute_nil(class_methods)
      assert_equal("ClassMethods", class_methods.unqualified_name)
    end
  end

  def test_add_module_can_have_methods_added
    with_context do |context|
      context.write!("concern.rb", "module MyConcern; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      file_path = context.absolute_path_to("concern.rb")
      graph.add_module(name: "MyConcern::ClassMethods", file_path: file_path, line: 1, column: 0)
      graph.resolve

      # Add method to the synthetic module
      graph.add_method(owner: "MyConcern::ClassMethods", name: "foo", file_path: file_path, line: 1, column: 0)
      graph.resolve

      class_methods = graph["MyConcern::ClassMethods"]
      assert_equal(1, class_methods.members.count)
      assert_equal("foo", class_methods.members.first.unqualified_name)
    end
  end

  def test_add_mixin_extend
    with_context do |context|
      context.write!("post.rb", <<~RUBY)
        module Concern; end
        module Concern::ClassMethods
          def foo; end
        end
        class Post; end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      result = graph.add_mixin(
        target: "Post",
        module_name: "Concern::ClassMethods",
        type: :extend,
      )
      assert(result)
    end
  end

  def test_add_mixin_include
    with_context do |context|
      context.write!("post.rb", <<~RUBY)
        module Validations
          def validate; end
        end
        class Post; end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      result = graph.add_mixin(
        target: "Post",
        module_name: "Validations",
        type: :include,
      )
      assert(result)
    end
  end

  def test_add_mixin_to_nonexistent_target_returns_false
    with_context do |context|
      context.write!("post.rb", "module Foo; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      result = graph.add_mixin(
        target: "NonExistent",
        module_name: "Foo",
        type: :include,
      )
      refute(result)
    end
  end

  def test_register_included_hook
    with_context do |context|
      context.write!("concern.rb", <<~RUBY)
        module MyConcern; end
        class Post
          include MyConcern
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      file_path = context.absolute_path_to("concern.rb")

      # Add ClassMethods module with a method
      graph.add_module(name: "MyConcern::ClassMethods", file_path: file_path, line: 1, column: 0)
      graph.resolve

      graph.add_method(owner: "MyConcern::ClassMethods", name: "foo", file_path: file_path, line: 1, column: 0)

      # Register hook: when MyConcern is included, extend ClassMethods
      result = graph.register_included_hook(
        module_name: "MyConcern",
        extend_module: "MyConcern::ClassMethods",
      )
      assert(result)

      graph.resolve

      # Verify ClassMethods exists with foo
      class_methods = graph["MyConcern::ClassMethods"]
      refute_nil(class_methods)
      assert_equal(1, class_methods.members.count)
    end
  end

  def test_register_included_hook_returns_true
    with_context do |context|
      context.write!("concern.rb", "module MyConcern; end")

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      result = graph.register_included_hook(
        module_name: "MyConcern",
        extend_module: "MyConcern::ClassMethods",
      )
      assert(result)
    end
  end

  def test_integration_belongs_to_plugin_simulation
    with_context do |context|
      context.write!("post.rb", <<~RUBY)
        class Post
          belongs_to :author
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      # Post starts with 0 members (belongs_to is not a method definition)
      post = graph["Post"]
      assert_equal(0, post.members.count)

      # Simulate plugin adding synthetic methods for belongs_to :author
      file_path = context.absolute_path_to("post.rb")
      graph.add_method(owner: "Post", name: "author", file_path: file_path, line: 2, column: 2)
      graph.add_method(owner: "Post", name: "author=", file_path: file_path, line: 2, column: 2)
      graph.resolve

      # Now Post has 2 members
      member_names = post.members.map(&:unqualified_name)
      assert_equal(2, member_names.size)
      assert_includes(member_names, "author")
      assert_includes(member_names, "author=")
    end
  end

  def test_integration_class_methods_plugin_simulation
    with_context do |context|
      context.write!("concern.rb", <<~RUBY)
        module MyConcern
          extend ActiveSupport::Concern

          class_methods do
            def foo; end
          end
        end

        class Post
          include MyConcern
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      # MyConcern::ClassMethods doesn't exist initially
      assert_nil(graph["MyConcern::ClassMethods"])

      file_path = context.absolute_path_to("concern.rb")

      # Simulate what a plugin would do when seeing `class_methods do ... end`
      # 1. Create ClassMethods module
      graph.add_module(name: "MyConcern::ClassMethods", file_path: file_path, line: 4, column: 2)
      graph.resolve

      # 2. Add foo method to ClassMethods
      graph.add_method(owner: "MyConcern::ClassMethods", name: "foo", file_path: file_path, line: 5, column: 4)

      # 3. Register included hook
      graph.register_included_hook(module_name: "MyConcern", extend_module: "MyConcern::ClassMethods")

      graph.resolve

      # Verify ClassMethods exists with foo method
      class_methods = graph["MyConcern::ClassMethods"]
      refute_nil(class_methods)
      assert_equal(1, class_methods.members.count)
      assert_equal("foo", class_methods.members.first.unqualified_name)

      # Post still exists
      post = graph["Post"]
      refute_nil(post)
    end
  end

  def test_integration_rspec_plugin_simulation
    with_context do |context|
      context.write!("spec.rb", <<~RUBY)
        RSpec.describe "Calculator" do
          subject { Calculator.new }
          let(:value) { 42 }

          context "when adding" do
            let(:other) { 10 }
          end
        end
      RUBY

      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve

      # No synthetic classes exist yet
      assert_nil(graph["RSpec::ExampleGroups::Calculator"])

      file_path = context.absolute_path_to("spec.rb")

      # Simulate what a plugin would do for RSpec.describe
      # 1. Create example group class for "Calculator"
      graph.add_class(name: "RSpec::ExampleGroups::Calculator", file_path: file_path, line: 1, column: 0)
      graph.resolve

      # 2. Add subject and let methods
      graph.add_method(owner: "RSpec::ExampleGroups::Calculator", name: "subject", file_path: file_path, line: 2, column: 2)
      graph.add_method(owner: "RSpec::ExampleGroups::Calculator", name: "value", file_path: file_path, line: 3, column: 2)

      # 3. Create nested context class
      graph.add_class(
        name: "RSpec::ExampleGroups::Calculator::WhenAdding",
        parent: "RSpec::ExampleGroups::Calculator",
        file_path: file_path,
        line: 5,
        column: 2,
      )
      graph.resolve

      # 4. Add let method to nested context
      graph.add_method(owner: "RSpec::ExampleGroups::Calculator::WhenAdding", name: "other", file_path: file_path, line: 6, column: 4)

      graph.resolve

      # Verify Calculator class has subject, value, and nested WhenAdding
      calc = graph["RSpec::ExampleGroups::Calculator"]
      refute_nil(calc)
      calc_members = calc.members.map(&:unqualified_name)
      assert_equal(3, calc_members.size)
      assert_includes(calc_members, "subject")
      assert_includes(calc_members, "value")
      assert_includes(calc_members, "WhenAdding")

      # Verify WhenAdding class has other
      when_adding = graph["RSpec::ExampleGroups::Calculator::WhenAdding"]
      refute_nil(when_adding)
      assert_equal(1, when_adding.members.count)
      assert_equal("other", when_adding.members.first.unqualified_name)
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
