# frozen_string_literal: true

require "json"
require "open3"

cargo_args = []

if Gem.win_platform?
  cargo_args << "--target x86_64-pc-windows-gnu"
  ENV["RUSTFLAGS"] = "-C target-feature=+crt-static"
end

desc "Run Rust tests"
task :cargo_test do
  puts "\n******** Running cargo tests ********\n"
  sh "cargo test #{cargo_args.join(" ")}".strip, chdir: "rust"
end

desc "Clean Rust build artifacts"
task :clean_rust do
  sh "cargo clean", chdir: "rust"
end

desc "Lint Rust code"
task :lint_rust do
  sh "cargo clippy --all-targets --all-features -- -D warnings", chdir: "rust"
  sh "cargo fmt --check", chdir: "rust"
end

desc "Format and auto fix violations for Rust code"
task :format_rust do
  sh "cargo clippy --all-targets --all-features --fix --allow-dirty", chdir: "rust"
  sh "cargo fmt", chdir: "rust"
end

desc "Sync the gem version and crate dependency with the Cargo workspace version"
task :sync_versions do
  metadata, status = Open3.capture2(
    "cargo", "metadata", "--no-deps", "--offline", "--format-version=1", "--manifest-path=rust/Cargo.toml"
  )
  abort "Could not read the Cargo workspace version" unless status.success?

  packages = JSON.parse(metadata).fetch("packages")
  rubydex_package = packages.find { |package| package.fetch("name") == "rubydex" }
  abort "Could not find rubydex in Cargo metadata" unless rubydex_package

  cargo_version = rubydex_package.fetch("version")
  gem_version = cargo_version.sub(/-beta\.(\d+)\z/) { ".beta#{Regexp.last_match(1)}" }

  replacements = {
    "lib/rubydex/version.rb" => [/^  VERSION = "[^"]+"$/, "  VERSION = #{gem_version.inspect}"],
    "rust/rubydex-sys/Cargo.toml" => [/^rubydex = \{ version = "[^"]+"/, "rubydex = { version = \"=#{cargo_version}\""],
  }

  updated_files = replacements.to_h do |path, (pattern, replacement)|
    contents = File.read(path)
    abort("Could not find the version in #{path}") unless contents.sub!(pattern, replacement)

    [path, contents]
  end

  updated_files.each do |path, contents|
    File.write(path, contents)
  end
end
