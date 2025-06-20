# Index

This repository contains a Rust implementation of a Ruby code indexer.

## Project Structure
The Rust implementation of the Ruby indexer is located within the `src/` directory. Rust code is exposed to C via the file `src/c_interface.rs` and has bindings generated using CBindgen, which is configured via `build.rs`.

The native extension within `ext/` contains C code that interacts with Ruby via the Ruby C API. The file `ext/index/index.c` makes use of the generated Rust bindings to interact with Rust code, bridging the gap between Rust and Ruby.

Finally, the `lib/` directory contains a thin Ruby wrapper for the native extension.

## Development
### Compiling the Rust and C code
The command `bundle exec rake compile` can be used to build both the Rust and C libraries. To clean generated files, simply run `bundle exec rake clean`.

### Running tests
Simply use the command `bundle exec rake test`, and the project will be built and tests located within `test/` will be run.
