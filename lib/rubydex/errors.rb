# frozen_string_literal: true

module Rubydex
  class Error < StandardError; end

  # Raised when `MethodAliasDefinition#target` walks an alias chain that loops back on itself.
  class AliasCycleError < Error; end

  # Raised by `Config.load` when the workspace does not exist, or when its config file cannot be read or is malformed.
  # A workspace with no config file at all is not an error.
  class ConfigError < Error; end

  # Raised by `Skill.load` when the file cannot be read, is missing frontmatter, has malformed YAML, or lacks the
  # required `name` or `description` fields.
  # Every failure to produce a Skill is a `SkillError`, so a caller rescues one class instead
  # of the union of what `File.read` and Psych happen to raise.
  class SkillError < Error; end

  # Raised by `SkillRegistry#fetch` when the library has no skill under the requested id.
  class UnknownSkillError < SkillError; end

  # Raised by `SkillRegistry.load` when the skill directory doesn't exist.
  class UnknownSkillDirectoryError < SkillError; end

  # Raised when a Cypher query cannot be parsed or cannot run. `Rubydex::Query` raises one of its
  # subclasses; rescue this class to catch either.
  class QueryError < Error; end

  # Raised by `Query.parse` when the query is not valid Cypher.
  class QuerySyntaxError < QueryError; end

  # Raised by `Query#run` when a parsed query fails while it runs against a graph, for example
  # because it names an unknown property or relationship type.
  class QueryExecutionError < QueryError; end

  # Raised when a query result names a node that the graph no longer holds, because the graph
  # changed after the query ran. Reading the rows would silently turn that column from a
  # `Declaration`, `Definition`, or `Document` handle into a plain String, so it raises instead.
  # `render`, `columns`, `size`, and `empty?` still work, because they read the executed result set
  # and never touch the graph.
  #
  # The check runs while a row is built, so it covers the rows that a walk has not reached yet. Two
  # cases fall outside it:
  #
  # - A handle that a walk already handed out. Such a handle resolves against the graph on each
  #   call, so a later change to the graph can make it stale. Handles from `Graph#[]` share that
  #   property. This error says nothing about them.
  # - A re-index that keeps the ids. A declaration id comes from the name, so a file indexed again
  #   under the same names still resolves, and this error does not fire, even though the
  #   definitions and ancestors behind that name may differ.
  class StaleQueryResultError < QueryError; end
end
