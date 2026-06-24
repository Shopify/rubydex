# frozen_string_literal: true

require "bundler/gem_tasks"
require "rubocop/rake_task"
require "rake/extensiontask"
require "rake/testtask"
require "rdoc/task"

GEMSPEC = Gem::Specification.load("rubydex.gemspec")

Rake::ExtensionTask.new("rubydex", GEMSPEC) do |ext|
  ext.lib_dir = "lib/rubydex"
end

test_config = lambda do |t|
  t.libs << "test"
  t.libs << "lib"
  t.ruby_opts << ["--enable=frozen_string_literal"]
  t.test_files = FileList["test/**/*_test.rb"]
end
Rake::TestTask.new(ruby_test: :compile, &test_config)

begin
  require "ruby_memcheck"
  namespace(:ruby_test) { RubyMemcheck::TestTask.new(valgrind: :compile, &test_config) }
rescue LoadError
  # ruby_memcheck is not available on Windows
end

RuboCop::RakeTask.new

RDoc::Task.new do |doc|
  doc.rdoc_dir = "_site"
end

namespace :rdoc do
  desc "Verify the generated RDoc site includes the public Ruby and native extension APIs"
  task :verify do
    Rake::Task["rdoc"].invoke

    html_files = FileList["_site/**/*.html"]
    abort "Generated RDoc site is empty" if html_files.empty?

    expected_pages = {
      "_site/Rubydex/Graph.html" => [
        'id="class-rubydex-graph"',
        'id="method-i-index_workspace"',
        'id="method-i-workspace_paths"',
        'id="method-i-index_all"',
        'id="method-i-index_source"',
        'index_source(uri, source, language_id)',
        'id="method-i-keyword"',
        'id="method-i-exclude_paths"',
        'id="method-i-resolve_constant"',
      ],
      "_site/Rubydex/Declaration.html" => [
        'id="class-rubydex-declaration"',
      ],
      "_site/Rubydex/Signature.html" => [
        'id="class-rubydex-signature"',
      ],
      "_site/Rubydex/Document.html" => [
        'id="class-rubydex-document"',
        'id="method-i-uri"',
        'id="method-i-definitions"',
      ],
      "_site/Rubydex/Definition.html" => [
        'id="class-rubydex-definition"',
        'id="method-i-location"',
        'id="method-i-comments"',
      ],
      "_site/Rubydex/ResolvedConstantReference.html" => [
        'id="class-rubydex-resolvedconstantreference"',
        'id="method-i-declaration"',
      ],
    }

    missing = expected_pages.flat_map do |file, entries|
      unless File.exist?(file)
        next ["#{file} was not generated"]
      end

      content = File.read(file)
      entries.reject { |entry| content.include?(entry) }.map { |entry| "#{file}: #{entry}" }
    end

    abort "Generated RDoc site is missing expected API entries:\n  - #{missing.join("\n  - ")}" unless missing.empty?
  end
end

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

task test: [:cargo_test, :ruby_test]
task check: [:lint, :test]

task default: :check
