# Instructions

## Project Overview

This Ruby gem and companion Rust crate provide a modern, high performance and low memory usage code indexing and
static analysis tools for hyper scale Ruby projects. The Rust crate is a library that implements all of the indexing
and static analysis logic. The Ruby gem is a native extension written in C that connects to that Rust library and
exposes a Ruby level API to interact with the logic.

Both the Ruby gem and Rust crate support Linux, MacOS and Windows.

## Ruby gem

The Ruby gem implements the API accessible to Ruby projects. A part of this is the native extension that connects
the Ruby VM to the Rust crate logic.

### Structure

- `ext/index`: The C native extension that connects the Ruby VM with the Rust crate logic through FFI
- `lib`: The rest of the Ruby code
- `test`: Ruby test files

### Commands

When necessary, commands can be executed for the Ruby code.

- `bundle exec rake compile`: compiles both the Rust crate and C extension
- `bundle exec rake lint`: lints both the Ruby and Rust code
- `bundle exec rake format`: auto formats both the Ruby and Rust code
- `bundle exec rake test`: runs all automated Ruby tests
- `bundle exec ruby -Itest test/specific_test.rb`: runs a specific test file

## Rust crate

The entire indexing and static analysis logic implementation. This crate aims to be optimized to achieve maximum
performance in large codebases while maintaining memory usage to a minimum.

The crate's goal is to provide all indexing and static analysis capabilities to power tools such as language servers,
type checkers, linting and other code analysis features.

### Commands

When necessary, commands can be executed for the Ruby code.

- `cargo build`: compiles the Rust code
- `cargo test`: runs Rust tests
- `cargo test test_name `: runs a specific tests example
- `bundle exec rake lint_rust`: lints the Rust code
- `bundle exec rake format_rust`: auto formats the Rust code
