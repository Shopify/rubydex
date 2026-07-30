# frozen_string_literal: true

require "test_helper"
require "helpers/context"
require "helpers/executable"

class DotCLIIntegrationTest < Minitest::Test
  include Test::Helpers::WithContext
  include Test::Helpers::WithExecutable

  def test_executable_outputs_graphviz
    with_context do |context|
      context.write!("simple.rb", "class SimpleClass; end")

      stdout, stderr, status = run_executable("dot", context.absolute_path)

      assert_predicate(status, :success?, stderr)
      assert_includes(stderr, "Indexing workspace")
      assert_includes(stderr, "Resolving graph")
      assert_includes(stdout, "digraph rubydex")
      assert_includes(stdout, "SimpleClass")
      refute_includes(stdout, "rubydex:built-in")

      stdout, stderr, status = run_executable("dot", "--show-builtins", context.absolute_path)

      assert_predicate(status, :success?, stderr)
      assert_includes(stdout, "rubydex:built-in")
    end
  end
end
