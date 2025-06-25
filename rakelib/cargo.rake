# frozen_string_literal: true

cargo_args = []

if Gem.win_platform?
  cargo_args << "--target x86_64-pc-windows-gnu"
  ENV["RUSTFLAGS"] = "-C target-feature=+crt-static"
end

desc "Compile Rust in dev mode"
task :compile_rust do
  sh "cargo build #{cargo_args.join(" ")}".strip
end

desc "Compile Rust in release mode"
task :compile_rust_release do
  sh "cargo build --release #{cargo_args.join(" ")}".strip
end

desc "Run Rust tests"
task :cargo_test do
  puts "\n******** Running cargo tests ********\n"
  sh "cargo test #{cargo_args.join(" ")}".strip
end

desc "Clean Rust build artifacts"
task :clean_rust do
  sh "cargo clean"
end

desc "Lint Rust code"
task :lint_rust do
  sh "cargo clippy --all-targets --all-features"
  sh "rustfmt --check **/*.rs"
end

desc "Format and auto fix violations for Rust code"
task :format_rust do
  sh "cargo clippy --all-targets --all-features --fix --allow-dirty"
  sh "rustfmt **/*.rs"
end
