# frozen_string_literal: true

require_relative "lib/rubydex/version"

Gem::Specification.new do |spec|
  spec.name = "rubydex"
  spec.version = Rubydex::VERSION
  spec.authors = ["Shopify"]
  spec.email = ["ruby@shopify.com"]
  spec.licenses = ["MIT"]

  spec.summary = "A high performance static analysis suite for Ruby"
  spec.description = "A high performance static analysis suite for Ruby, built in Rust with Ruby APIs"
  spec.homepage = "https://github.com/Shopify/rubydex"
  spec.required_ruby_version = ">= 3.2.0"
  spec.required_rubygems_version = ">= 3.3.11"

  spec.metadata["allowed_push_host"] = "https://rubygems.org"
  spec.metadata["homepage_uri"] = spec.homepage
  spec.metadata["source_code_uri"] = spec.homepage
  spec.metadata["changelog_uri"] = "#{spec.homepage}/releases"

  spec.files = ["README.md", "LICENSE.txt"] +
    Dir.glob("lib/**/*.rb") +
    Dir.glob("ext/rubydex/**/*.{c,h}") +
    Dir.glob("rust/**/*.{rs,toml,lock,hbs}").reject { |f| f.start_with?("rust/target") }

  if ENV["RELEASE"]
    spec.files << "THIRD_PARTY_LICENSES.html"
    if RUBY_PLATFORM.include?("darwin")
      spec.files << "lib/rubydex/librubydex_sys.dylib"
    elsif RUBY_PLATFORM.include?("linux")
      spec.files << "lib/rubydex/librubydex_sys.so"
    end
  end

  spec.bindir = "exe"
  spec.executables = Dir.glob("exe/*").map { |f| File.basename(f) }
  spec.require_paths = ["lib"]
  spec.extensions = ["ext/rubydex/extconf.rb"]
end
