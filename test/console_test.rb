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

  def test_run_query_renders_table_against_the_console_graph
    with_graph("class Animal; end\nclass Dog < Animal; end\n") do
      output = Rubydex::Console.run_query("MATCH (c:Class)-[:INHERITS]->(p) WHERE c.name = 'Dog' RETURN p.name")

      assert_match(/p\.name/, output)
      assert_match(/Animal/, output)
    end
  end

  def test_run_query_supports_json_format
    with_graph("class Dog; end\n") do
      output = Rubydex::Console.run_query("MATCH (c:Class {name: 'Dog'}) RETURN c.name", format: :json)

      assert_equal("[{\"c.name\":\"Dog\"}]", output)
    end
  end

  def test_run_query_returns_usage_for_blank_input
    with_graph("class Dog; end\n") do
      assert_equal("Usage: query <CYPHER>", Rubydex::Console.run_query("   "))
    end
  end

  def test_run_query_returns_error_message_on_invalid_query
    with_graph("class Dog; end\n") do
      output = Rubydex::Console.run_query("MATCH (c RETURN c")

      assert_match(/Cypher syntax error/, output)
    end
  end

  def test_describe_schema_lists_relationships
    output = Rubydex::Console.describe_schema(format: :json)
    parsed = JSON.parse(output)

    assert(parsed["relationships"].any? { |r| r["type"] == "INHERITS" })
  end

  def test_commands_are_registered_on_modern_irb
    names = IRB::Command.commands.keys

    assert_includes(names, :query)
    assert_includes(names, :schema)
  end

  def test_query_runs_without_the_command_api
    # The core query path must not depend on IRB's command registration, so the console still works
    # (minus the `query`/`schema` shortcuts) on older IRB versions.
    with_graph("class Dog; end\n") do
      assert_equal("[{\"c.name\":\"Dog\"}]", Rubydex::Console.run_query("MATCH (c:Class {name: 'Dog'}) RETURN c.name", format: :json))
    end
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
