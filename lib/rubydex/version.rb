# frozen_string_literal: true

module Rubydex
  # The gem and the crates are released under one version number, so it lives in rust/Cargo.toml under
  # [workspace.package] and is read from there rather than declared here. Cargo requires its version to be a literal,
  # so the manifest is the only place that can hold the single source of truth.
  VERSION = File.read(File.expand_path("../../rust/Cargo.toml", __dir__))[/^version = "(.+)"/, 1]
    .sub(/-beta\.(\d+)\z/) { ".beta#{Regexp.last_match(1)}" }
end
