# Contributing

## Project structure

This project is structured in 3 parts:

- `rust/rubydex`: the Rust crate implementing all of the core logic for statically analyzing Ruby code
- `rust/rubydex-sys`: the Rust crate for FFI bindings that allow using the main crate from C code
- the top level of the repository is a Ruby gem with a native extension, which uses C code to link against the Rust
crate and provide a Ruby API for the Rust backed implementation

## Architecture, concepts and analysis

To understand how the analysis is structured, please see [the architecture docs](docs/architecture.md). We also have
documentation for [Ruby concepts and behavior](docs/ruby-behaviors.md).

## Practical development tips

In general, we strive for consistency in our development environments and techniques. For example, recommended
extensions and settings for VS Code are already provided in the `.vscode` directory.

Instructions for AI models and agents are in the [agents file](AGENTS.md).

### Rust

The `rust` directory is a worskpace, where all common cargo commands can be used to build, lint or run tests.

- Testing: `cargo test`
- Linting: `cargo clippy`
- Formatting: `rustfmt`
- Compiling: `cargo build`

With the recommended extensions, it's possible to use Rust Analyzer's debug code lens actions to interactively
debug tests through VS Code.

We try to be on the latest version of Rust and CI always runs against the latest.

### Ruby

- Compiling: `bundle exec rake compile` (triggers the compilation of the Rust crates too)
- Testing: `bundle exec rake ruby_test`
- Linting: `bundle exec rubocop`
- Formatting: `bundle exec rubocop -a`
