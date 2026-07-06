# frozen_string_literal: true

require "test_helper"
require "helpers/context"
require "json"
require "rubydex/mcp_server"

class MCPServerToolsTest < Minitest::Test
  include Test::Helpers::WithContext

  def test_search_declarations_tool
    with_mcp_server do |server|
      exact = call_tool(server, "search_declarations", query: "Dog", match_mode: "exact", kind: "Class")

      assert_equal(1, exact.fetch("total"))
      assert_equal(["Dog"], exact.fetch("results").map { |entry| entry.fetch("name") })

      paginated = call_tool(server, "search_declarations", query: "Dog", match_mode: "exact", limit: 1)

      assert_equal(3, paginated.fetch("total"))
      assert_equal(1, paginated.fetch("results").length)

      invalid = call_tool(server, "search_declarations", query: "Dog", match_mode: "contains")

      assert_equal("invalid_match_mode", invalid.fetch("error"))
      assert_match(/Invalid match_mode/, invalid.fetch("message"))
    end
  end

  def test_get_declaration_tool
    with_mcp_server do |server|
      declaration = call_tool(server, "get_declaration", name: "Dog")

      assert_equal("Dog", declaration.fetch("name"))
      assert_equal("Class", declaration.fetch("kind"))
      assert_has_value(declaration.fetch("definitions"), "app.rb", "Dog definitions", key: "path")
      assert_has_value(declaration.fetch("ancestors"), "Animal", "Dog ancestors")
      assert_has_value(declaration.fetch("members"), "Dog#speak()", "Dog members")
      assert_has_value(declaration.fetch("members"), "Dog::BREED", "Dog members")

      not_found = call_tool(server, "get_declaration", name: "Missing")

      assert_equal("not_found", not_found.fetch("error"))
      assert_match(/search_declarations/, not_found.fetch("suggestion"))
    end
  end

  def test_get_descendants_tool
    with_mcp_server do |server|
      descendants = call_tool(server, "get_descendants", name: "Animal")

      assert_equal("Animal", descendants.fetch("name"))
      assert_equal(3, descendants.fetch("total"))
      assert_has_value(descendants.fetch("descendants"), "Animal", "Animal descendants")
      assert_has_value(descendants.fetch("descendants"), "Dog", "Animal descendants")
      assert_has_value(descendants.fetch("descendants"), "Cat", "Animal descendants")

      page = call_tool(server, "get_descendants", name: "Animal", limit: 1, offset: 1)

      assert_equal(3, page.fetch("total"))
      assert_equal(1, page.fetch("descendants").length)

      invalid = call_tool(server, "get_descendants", name: "Dog::BREED")

      assert_equal("invalid_kind", invalid.fetch("error"))
    end
  end

  def test_find_constant_references_tool
    with_mcp_server do |server|
      references = call_tool(server, "find_constant_references", name: "Animal")

      assert_equal("Animal", references.fetch("name"))
      assert_operator(references.fetch("total"), :>=, 3)
      assert(references.fetch("references").all? { |entry| entry.key?("path") && entry.key?("line") && entry.key?("column") })
      assert(references.fetch("references").any? { |entry| entry.fetch("path") == "app.rb" })
      assert(references.fetch("references").any? { |entry| entry.fetch("path") == "cat.rb" })

      page = call_tool(server, "find_constant_references", name: "Animal", limit: 1, offset: -10)

      assert_equal(references.fetch("total"), page.fetch("total"))
      assert_equal(1, page.fetch("references").length)

      method_references = call_tool(server, "find_constant_references", name: "Dog#speak()")

      assert_equal("Dog#speak()", method_references.fetch("name"))
      assert_equal(0, method_references.fetch("total"))
      assert_empty(method_references.fetch("references"))
    end
  end

  def test_get_file_declarations_tool
    with_mcp_server do |server|
      file = call_tool(server, "get_file_declarations", file_path: "app.rb")

      assert_equal("app.rb", file.fetch("file"))
      assert_has_value(file.fetch("declarations"), "Animal", "app.rb declarations")
      assert_has_value(file.fetch("declarations"), "Animal::KIND", "app.rb declarations")
      assert_has_value(file.fetch("declarations"), "Dog", "app.rb declarations")
      assert_has_value(file.fetch("declarations"), "Dog#speak()", "app.rb declarations")

      missing = call_tool(server, "get_file_declarations", file_path: "missing.rb")

      assert_equal("not_found", missing.fetch("error"))
    end
  end

  def test_get_file_declarations_decodes_file_uri_paths
    with_context do |context|
      context.write!("my app.rb", "class SpacedFile; end")
      server, errors = indexed_server(context.absolute_path, [context.absolute_path])

      assert_empty(errors)

      file = call_tool(server, "get_file_declarations", file_path: "my app.rb")

      assert_equal("my app.rb", file.fetch("file"))
      assert_has_value(file.fetch("declarations"), "SpacedFile", "my app.rb declarations")
    end
  end

  def test_todo_declaration_uses_graph_kind_string
    with_mcp_server do |server|
      declaration = call_tool(server, "get_declaration", name: "MissingParent")
      stats = call_tool(server, "codebase_stats")

      assert_equal("<TODO>", declaration.fetch("kind"))
      assert_operator(stats.fetch("breakdown_by_kind").fetch("<TODO>"), :>=, 1)
    end
  end

  def test_codebase_stats_tool
    with_mcp_server do |server|
      stats = call_tool(server, "codebase_stats")

      assert_equal(3, stats.fetch("files"))
      assert_operator(stats.fetch("declarations"), :>, 0)
      assert_operator(stats.fetch("definitions"), :>, 0)
      assert_operator(stats.fetch("constant_references"), :>, 0)
      assert_operator(stats.fetch("method_references"), :>, 0)
      assert_operator(stats.fetch("breakdown_by_kind").fetch("Class"), :>=, 3)
      assert_operator(stats.fetch("breakdown_by_kind").fetch("Method"), :>=, 3)
      assert_operator(stats.fetch("breakdown_by_kind").fetch("Constant"), :>=, 2)
    end
  end

  private

  def with_mcp_server
    with_context do |context|
      write_fixture(context)
      server, errors = indexed_server(context.absolute_path, [context.absolute_path])

      assert_empty(errors)

      yield server
    end
  end

  def indexed_server(root_path, paths)
    graph = Rubydex::Graph.new(workspace_path: root_path)
    errors = graph.index_all(paths)
    graph.resolve

    server = Rubydex::MCPServer::Server.new(root_path: root_path)
    server.instance_variable_set(:@graph, graph)
    server.instance_variable_set(:@index_finished, true)

    [server, errors]
  end

  def write_fixture(context)
    context.write!("app.rb", <<~RUBY)
      class Animal
        KIND = "animal"

        def speak
          "..."
        end
      end

      class Dog < Animal
        BREED = "unknown"

        def speak
          Animal::KIND
        end
      end
    RUBY

    context.write!("cat.rb", <<~RUBY)
      class Cat < Animal
      end

      class Kennel
        def build
          Animal.new
        end
      end

      class MissingParent::Child
      end
    RUBY
  end

  def call_tool(server, tool_name, arguments = {})
    response = server.handle(
      {
        jsonrpc: "2.0",
        id: 1,
        method: "tools/call",
        params: {
          name: tool_name,
          arguments: arguments,
        },
      },
    )

    JSON.parse(response.fetch(:result).fetch(:content)[0].fetch(:text))
  end

  def assert_has_value(entries, expected_value, context, key: "name")
    values = entries.map { |entry| entry.fetch(key) }
    assert_includes(values, expected_value, "Expected #{context} to include #{expected_value}, got: #{values.inspect}")
  end
end
