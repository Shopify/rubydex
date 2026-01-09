# Rubydex

TODO

## Documentation

- [Ruby Language Behaviors](docs/ruby-behaviors.md) - Comprehensive documentation of Ruby language behaviors that the indexer handles, including lexical scoping, constant resolution, method parameters, attribute methods, and more. This document is useful for understanding the nuances of Ruby that affect indexing.

## Usage

TODO

## Development

TODO: move to a proper contributing file.

### Compiling the Rust and C code

The command `bundle exec rake compile` can be used to build both the Rust and C libraries. To clean generated files, simply run `bundle exec rake clean`.

### Running tests

Run `bundle exec rake test` to execute both the Ruby and Rust test suites. To run only the Ruby tests, use `bundle exec rake ruby_test`. The combined lint-and-test flow is available through `bundle exec rake check`.

### Running memory leak and other sanitization checks

We have two layers of sanitization checks:

- [ruby_memcheck](https://github.com/Shopify/ruby_memcheck): uses Valgrind to find memory leaks by running our Ruby test suite (only supported for Linux)
- Rust sanitizers for valid memory addresses, leaks and thread data races

Here's how to run the checks locally:

```shell
# Run ruby_memcheck in combination with a Rust sanitization mode (Linux only)
SANITIZER=leak bundle exec rake ruby_test:valgrind

# Run only ruby_memcheck (Linux only)
bundle exec rake ruby_test:valgrind

# Run Rust sanitization only
SANITIZER=leak bundle exec rake ruby_test
```
