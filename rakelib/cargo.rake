# frozen_string_literal: true

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
