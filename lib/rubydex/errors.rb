# frozen_string_literal: true

module Rubydex
  class Error < StandardError; end

  # Raised when `MethodAliasDefinition#target` walks an alias chain that loops back on itself.
  class AliasCycleError < Error; end

  # Raised by `Config.load` when the workspace does not exist, or when its config file cannot be read or is malformed.
  # A workspace with no config file at all is not an error.
  class ConfigError < Error; end
end
