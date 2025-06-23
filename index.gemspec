# frozen_string_literal: true

require_relative "lib/index/version"

Gem::Specification.new do |spec|
  spec.name = "index"
  spec.version = Index::VERSION
  spec.authors = ["Shopify"]
  spec.email = ["ruby@shopify.com"]

  spec.summary = "A high performance static analysis suite for Ruby"
  spec.description = "A high performance static analysis suite for Ruby, built in Rust with Ruby APIs"
  spec.homepage = "https://github.com/Shopify/index"
  spec.required_ruby_version = ">= 3.1.0"
  spec.required_rubygems_version = ">= 3.3.11"

  spec.metadata["allowed_push_host"] = "https://rubygems.org"
  spec.metadata["homepage_uri"] = spec.homepage
  spec.metadata["source_code_uri"] = spec.homepage
  spec.metadata["changelog_uri"] = "#{spec.homepage}/releases"

  spec.files = Dir.glob("lib/**/*.rb") + ["README.md", "LICENSE.txt"]
  spec.bindir = "exe"
  spec.executables = Dir.glob("exe/*").map { |f| File.basename(f) }
  spec.require_paths = ["lib"]
  spec.extensions = ["ext/index/extconf.rb"]
end
