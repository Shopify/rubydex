# frozen_string_literal: true

module Rubydex
  class Error < StandardError; end

  # Raised when `MethodAliasDefinition#target` walks an alias chain that loops back on itself.
  class AliasCycleError < Error; end

  # Raised by `Graph#load_config` when the requested config file does not exist, cannot be read, or is malformed
  class ConfigError < Error; end
end
