# frozen_string_literal: true

require "bundler/gem_tasks"
require "rubocop/rake_task"
require "rake/extensiontask"
require "rake/testtask"

GEMSPEC = Gem::Specification.load("index.gemspec")

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

task :lint do
  puts "******** Linting ********\n"
  Rake::Task["rubocop"].invoke
  Rake::Task["lint_rust"].invoke
end

task :format do
  puts "******** Formatting ********\n"
  Rake::Task["rubocop:autocorrect"].invoke
  Rake::Task["format_rust"].invoke
end

# Enhance the clean task to also clean Rust artifacts
Rake::Task[:clean].enhance([:clean_rust])

task compile_release: :clean do
  ENV["RELEASE"] = "true"
  Rake::Task[:compile].invoke
end

task default: [:lint, :cargo_test, :test]
