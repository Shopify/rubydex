# frozen_string_literal: true

require "bundler"
require "uri"

module Rubydex
  class Error < StandardError; end
end

require "rubydex/version"

begin
  # Load the precompiled version of the library
  ruby_version = /(\d+\.\d+)/.match(RUBY_VERSION)
  require "rubydex/#{ruby_version}/rubydex"
rescue LoadError
  # It's important to leave for users that can not or don't want to use the gem with precompiled binaries.
  require "rubydex/rubydex"
end

require "rubydex/location"
require "rubydex/comment"
require "rubydex/diagnostic"
require "rubydex/graph"
