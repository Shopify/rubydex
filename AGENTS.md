# Instructions

Use the available subagents to help the user accomplish the requested task:

- Ruby VM expert: can assist with tasks related to Ruby programming, its virtual machine and C API
- Rust expert: can assist with Rust and systems programming tasks
- Type checker architect: can assist with static analysis, type checking and type systems related tasks

## Project Overview

This Ruby gem and companion Rust crate provide a modern, high performance and low memory usage code indexing and
static analysis tools for hyper scale Ruby projects. The Rust crate is a library that implements all of the indexing
and static analysis logic. The Ruby gem is a native extension written in C that connects to that Rust library and
exposes a Ruby level API to interact with the logic.

Both the Ruby gem and Rust crate support Linux, MacOS and Windows.

## Documentation

- `docs/ruby-behaviors.md`: Comprehensive documentation of Ruby language behaviors that the indexer must handle correctly. This includes lexical scoping, constant resolution, method parameters, attribute methods, variable scoping, and namespace qualification. When working on the codebase:
  - **Reference this document** to understand Ruby behaviors that affect indexing
  - **Verify new Ruby behavior documentation** against this document when you encounter comments explaining Ruby semantics in the codebase
  - **Update this document** when discovering new Ruby behaviors or when existing documentation is incomplete or incorrect

## Ruby gem

The Ruby gem implements the API accessible to Ruby projects. A part of this is the native extension that connects
the Ruby VM to the Rust crate logic.

### Structure

- `ext/rubydex`: The C native extension that connects the Ruby VM with the Rust crate logic through FFI
- `lib`: The rest of the Ruby code
- `test`: Ruby test files

### Commands

When necessary, commands can be executed for the Ruby code.

- `bundle exec rake compile`: compiles both the Rust crate and C extension
- `bundle exec rake lint`: lints both the Ruby and Rust code
- `bundle exec rake format`: auto formats both the Ruby and Rust code
- `bundle exec rake test`: runs the Ruby and Rust test suites
- `bundle exec rake ruby_test`: runs all automated Ruby tests
- `bundle exec ruby -Itest test/specific_test.rb`: runs a specific test file

## Rust workspace

The Rust workspace under the `rust` directory contains two crates:

- `rubydex`: this crate implements the entire indexing and static analysis logic. The implementation aims to be optimized
to achieve maximum performance in super large codebases while maintaining memory usage to a minimum
- `rubydex-sys`: this crate provides bindings for C, so that the logic from `rubydex` can be called through FFI

The workspace's goal is to provide all indexing and static analysis capabilities to power tools such as language servers,
type checkers, linting and other code analysis features.

### Key files

- `rust/rubydex/src/model/graph.rs`: the Graph representation of the codebase. Read more about the architecture of the graph
in `docs/architecture.md`
- `rust/rubydex/src/indexing/ruby_indexer.rs`: the visitor that extracts definition information from the
AST to save in the graph
- `rust/rubydex/src/indexing.rs`: the parallel implementation of indexing a list of documents
- `rust/rubydex/src/resolution.rs`: the Resolution stage that computes fully qualified names, creates declarations,
resolves constant references, and linearizes ancestor chains

### Commands

When necessary, commands can be executed for the Rust code.

- `cargo build`: compiles the Rust code
- `cargo run -- <directory>`: runs the indexer on the specified directory (must use absolute paths or $HOME, not ~)
- `cargo run -- <directory> --stats`: runs the indexer with detailed performance breakdown
- `cargo run -- <directory> --stop-after <stage>`: stops after the specified stage (Listing, Indexing, or Resolution)
- `cargo run -- <directory> --visualize`: generates a DOT visualization of the graph
- `cargo test`: runs Rust tests
- `cargo test test_name`: runs a specific tests example
- `cargo fmt`: auto formats the Rust code
- `cargo clippy`: lints the Rust code
- `bundle exec rake lint_rust`: lints the Rust code
- `bundle exec rake format_rust`: auto formats the Rust code
