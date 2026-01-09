# frozen_string_literal: true

source "https://rubygems.org"

# Specify your gem's dependencies in rubydex.gemspec
gemspec

gem "rake", "~> 13.0"
gem "rake-compiler"
gem "minitest"
gem "rubocop"
gem "rubocop-shopify"
gem "extconf_compile_commands_json"

# Gems that aren't supported on Windows
platforms :ruby do
  gem "ruby_memcheck"
end
