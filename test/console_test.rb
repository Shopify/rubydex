# frozen_string_literal: true

require "test_helper"
require "helpers/context"
require "json"
require "rubydex/console"

class ConsoleTest < Minitest::Test
  include Test::Helpers::WithContext

  def teardown
    Rubydex::Console.graph = nil
  end

  def test_run_returns_rich_objects_against_the_console_graph
    with_graph("class Animal; end\nclass Dog < Animal; end\n") do
      rows = Rubydex::Console.run("MATCH (c:Class)-[:HAS_PARENT]->(p) WHERE c.name = 'Dog' RETURN c, p")

      assert_kind_of(Array, rows)
      assert_equal(1, rows.length)
      # Node columns come back as live handles that can be navigated/assigned.
      assert_kind_of(Rubydex::Declaration, rows.first["c"])
      assert_equal("Dog", rows.first["c"].name)
      assert_equal("Animal", rows.first["p"].name)
    end
  end

  def test_run_raises_on_invalid_query
    with_graph("class Dog; end\n") do
      error = assert_raises(ArgumentError) { Rubydex::Console.run("MATCH (c RETURN c") }
      assert_match(/Cypher syntax error/, error.message)
    end
  end

  def test_render_returns_formatted_output_for_the_query_command
    with_graph("class Animal; end\nclass Dog < Animal; end\n") do
      output = Rubydex::Console.render("MATCH (c:Class)-[:HAS_PARENT]->(p) WHERE c.name = 'Dog' RETURN p.name")

      assert_match(/p\.name/, output)
      assert_match(/Animal/, output)
    end
  end

  def test_render_supports_json_format
    with_graph("class Dog; end\n") do
      output = Rubydex::Console.render("MATCH (c:Class {name: 'Dog'}) RETURN c.name", format: :json)

      assert_equal("[{\"c.name\":\"Dog\"}]", output)
    end
  end

  def test_describe_schema_lists_relationships
    output = Rubydex::Console.describe_schema(format: :json)
    parsed = JSON.parse(output)

    assert(parsed["relationships"].any? { |r| r["type"] == "HAS_PARENT" })
  end

  def test_commands_and_helpers_are_registered
    assert_includes(IRB::Command.commands.keys, :query)
    assert_includes(IRB::Command.commands.keys, :schema)

    # `run` is a helper method rather than a command, so it stays an ordinary expression whose
    # value can be assigned and navigated at the prompt.
    assert_includes(IRB::HelperMethod.helper_methods.keys, :run)
  end

  def test_run_helper_delegates_to_the_console_graph
    with_graph("class Dog; end\n") do
      helper = Rubydex::Console::RunHelper.instance

      assert_equal([{ "c.name" => "Dog" }], helper.execute("MATCH (c:Class {name: 'Dog'}) RETURN c.name"))
    end
  end

  def test_session_workspace_evaluates_at_top_level
    # `IRB::WorkSpace` reads IRB.conf[:CONTEXT_MODE] to build its binding; `Console.start` calls
    # `IRB.setup` before it does.
    IRB.setup(nil) unless IRB.conf[:CONTEXT_MODE]
    workspace = Rubydex::Console.send(:workspace_for, :sentinel)

    # `self` must be `main`, as in a stock IRB session — not `Rubydex::Console`.
    assert_same(TOPLEVEL_BINDING.receiver, workspace.binding.receiver)

    # `graph` is exposed, and nothing else is inherited from the caller's scope. A local named
    # `query` or `schema` would shadow the IRB command of the same name.
    assert_same(:sentinel, workspace.binding.eval("graph"))
    refute_includes(workspace.binding.local_variables, :query)
    refute_includes(workspace.binding.local_variables, :schema)

    # A class defined at the prompt must land at top level, not under the singleton class of
    # Rubydex::Console (which would name it "#<Class:Rubydex::Console>::ConsoleWorkspaceProbe").
    workspace.binding.eval("class ConsoleWorkspaceProbe; end")

    assert(Object.const_defined?(:ConsoleWorkspaceProbe, false))
    assert_equal("ConsoleWorkspaceProbe", workspace.binding.eval("ConsoleWorkspaceProbe").name)
  ensure
    Object.send(:remove_const, :ConsoleWorkspaceProbe) if Object.const_defined?(:ConsoleWorkspaceProbe, false)
  end

  private

  def with_graph(source)
    with_context do |context|
      context.write!("zoo.rb", source)
      graph = Rubydex::Graph.new
      graph.index_all(context.glob("**/*.rb"))
      graph.resolve
      Rubydex::Console.graph = graph
      yield
    end
  end
end
