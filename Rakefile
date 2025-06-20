# frozen_string_literal: true

require "bundler/gem_tasks"
require "rubocop/rake_task"
require "rake/extensiontask"
require "rake/testtask"

GEMSPEC = Gem::Specification.load("index.gemspec")

desc "Clean Rust build artifacts"
task :clean_rust do
  sh "cargo clean"
end

Rake::ExtensionTask.new("index", GEMSPEC) do |ext|
  ext.lib_dir = "lib/index"
end

Rake::TestTask.new(test: :compile) do |t|
  t.libs << "test"
  t.libs << "lib"
  t.ruby_opts << ["--enable=frozen_string_literal"]
  t.test_files = FileList["test/**/*_test.rb"]
end

RuboCop::RakeTask.new

# Enhance the clean task to also clean Rust artifacts
Rake::Task[:clean].enhance([:clean_rust])

task default: [:rubocop, :test]
