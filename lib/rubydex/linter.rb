# frozen_string_literal: true

require "rubydex"
require "rubydex/linter/helpers/path_helpers"
require "rubydex/linter/helpers/source_access_helpers"
require "rubydex/linter/result"
require "rubydex/linter/custom_rule"

Dir.glob(File.expand_path("../rubydex_linter/rules/**/*.rb", __dir__)).each do |rule_file|
  require rule_file
end

require "rubydex/linter/rule_loader"
require "rubydex/linter/runner"

module Rubydex
  # Framework for running semantic lint rules against a resolved Rubydex graph.
  module Linter
  end
end
