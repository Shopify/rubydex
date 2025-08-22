# Index

TODO

## Usage

TODO

## Development

TODO: move to a proper contributing file.

### Compiling the Rust and C code

The command `bundle exec rake compile` can be used to build both the Rust and C libraries. To clean generated files, simply run `bundle exec rake clean`.

### Running tests

Simply use the command `bundle exec rake test`, and the project will be built and tests located within `test/` will be run.

### Running memory leak and other sanitization checks

We have two layers of sanitization checks:

- [ruby_memcheck](https://github.com/Shopify/ruby_memcheck): uses Valgrind to find memory leaks by running our Ruby test suite (only supported for Linux)
- Rust sanitizers for valid memory addresses, leaks and thread data races

Here's how to run the checks locally:

```shell
# Run ruby_memcheck in combination with a Rust sanitization mode (Linux only)
SANITIZER=leak bundle exec rake test:valgrind

# Run only ruby_memcheck (Linux only)
bundle exec rake test:valgrind

# Run Rust sanitization only
SANITIZER=leak bundle exec rake test
```
