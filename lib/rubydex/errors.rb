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
end
