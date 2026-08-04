# frozen_string_literal: true

source "https://rubygems.org"

# Specify your gem's dependencies in rubydex.gemspec
gemspec

gem "rake", "~> 13.4"
gem "rake-compiler"
gem "minitest"
gem "rdoc"
gem "mocha"
gem "rubocop"
gem "rubocop-shopify"
gem "extconf_compile_commands_json"
gem "rbs"
gem "irb"
# `irb` loads `reline`, which requires `fiddle` on Windows but does not declare it. `fiddle` is a
# bundled gem from Ruby 4.0 on, so the bundle has to supply it.
gem "fiddle"

# Gems that aren't supported on Windows
platforms :ruby do
  gem "ruby_memcheck"
end
