# frozen_string_literal: true

require "test_helper"
require "helpers/context"

# Reproduction for the built-in core declarations carrying no members.
#
# `add_built_in_data` indexes an RBS snippet that declares `BasicObject`,
# `Kernel`, `Object`, `Module` and `Class` with empty bodies. That is enough for
# ancestor linearization, which is what it was written for, but it leaves every
# core method missing from the graph. A consumer asking "does this receiver
# respond to `new`?" gets the same answer for `Class#new` as for a name that
# does not exist anywhere, and cannot tell the two apart.
class BuiltInMembersTest < Minitest::Test
  include Test::Helpers::WithContext

  def test_class_new_is_a_member_of_class
    with_context do |context|
      context.write!("report.rb", <<~RUBY)
        class Report
          def initialize(source, format); end
        end
      RUBY

      graph = index(context)

      refute_nil(graph["Class"].member("new()"), "Class#new is missing from the built-in declarations")
    end
  end

  def test_module_reflection_methods_are_members_of_module
    with_context do |context|
      context.write!("noop.rb", "class Noop; end")

      graph = index(context)
      module_declaration = graph["Module"]

      ["const_get()", "const_defined?", "name()"].each do |method|
        refute_nil(module_declaration.member(method), "Module##{method} is missing from the built-in declarations")
      end
    end
  end

  # The consequence: `Report.new` walks the singleton ancestry looking for
  # `new`, passes `Class` (empty) and reaches `Object`'s instance methods, which
  # in Ruby sit below `Class#new` in the method resolution order and are never
  # what `Report.new` calls. A single top-level `def new` anywhere in a project
  # is enough to answer every constructor lookup in it.
  def test_singleton_lookup_stops_at_class_new
    with_context do |context|
      # A bare `def` at the top level of a block-based DSL file, and anything at
      # all defined on `Object`'s singleton so the lookup has a scope to start from.
      context.write!("dsl.rb", "def new; end\n")
      context.write!("core_ext.rb", "class Object\n  def self.helper; end\nend\n")
      context.write!("report.rb", <<~RUBY)
        class Report
          def initialize(source, format); end
        end
      RUBY

      graph = index(context)
      found = singleton_member(graph, graph["Report"], "new()")

      assert_equal("Class#new()", found&.name, "expected the lookup to stop at Class#new")
    end
  end

  private

  def index(context)
    graph = Rubydex::Graph.new
    graph.index_all([context.absolute_path])
    graph.resolve
    graph
  end

  # The lookup `RuboCop::Cop::ProjectIndexHelp#indexed_singleton_member` performs:
  # the first ancestor that has a singleton declaration answers for the whole chain.
  def singleton_member(graph, declaration, member_name)
    declaration.ancestors.each do |ancestor|
      singleton = graph["#{ancestor.name}::<#{ancestor.name.split("::").last}>"]
      return singleton.find_member(member_name) if singleton
    end

    nil
  end
end
