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

  def test_commands_are_registered_on_modern_irb
    names = IRB::Command.commands.keys

    assert_includes(names, :query)
    assert_includes(names, :schema)
  end

  def test_run_works_without_the_command_api
    # `run` must not depend on IRB's command registration, so the console still works (minus the
    # `query`/`schema` shortcuts) on older IRB versions.
    with_graph("class Dog; end\n") do
      rows = Rubydex::Console.run("MATCH (c:Class {name: 'Dog'}) RETURN c.name")
      assert_equal([{ "c.name" => "Dog" }], rows)
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
