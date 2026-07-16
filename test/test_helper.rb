# frozen_string_literal: true

require "minitest/autorun"
Warning[:experimental] = false

$LOAD_PATH.unshift(File.expand_path("../lib", __dir__))
require "rubydex"
