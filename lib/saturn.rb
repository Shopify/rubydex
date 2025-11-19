# frozen_string_literal: true

require "bundler"
require "uri"

require "saturn/version"
begin
  # Load the precompiled version of the library
  ruby_version = /(\d+\.\d+)/.match(RUBY_VERSION)
  require "saturn/#{ruby_version}/saturn"
rescue LoadError
  # It's important to leave for users that can not or don't want to use the gem with precompiled binaries.
  require "saturn/saturn"
end

require "saturn/location"
require "saturn/comment"
require "saturn/graph"

module Saturn
  class Error < StandardError; end
end
