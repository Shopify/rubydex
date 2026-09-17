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

## Releasing

Releases are cut by maintainers from `main`. GitHub Actions builds and publishes
precompiled gems to RubyGems and publishes the crates to crates.io. Do not run
`rake release` or `cargo publish` locally.

To cut a new release:

1. Check out `main` and make sure it is current:

   ```sh
   git checkout main
   git pull --ff-only
   ```

2. Set the release version in `rust/Cargo.toml`. Both Rust crates inherit this
   value:

   ```toml
   [workspace.package]
   version = "X.Y.Z"
   ```

   For a beta release, use `X.Y.Z-beta.N` in this file.

3. Run the synchronization task to update `lib/rubydex/version.rb` and the
   `rubydex` dependency version in `rust/rubydex-sys/Cargo.toml`. Do not edit
   those versions manually:

   ```sh
   bundle exec rake sync_versions
   ```

4. Refresh `rust/Cargo.lock` and `Gemfile.lock` to record the new crate and gem
   versions:

   ```sh
   cargo check --manifest-path rust/Cargo.toml
   bundle lock --local
   ```

5. Run the local validation suite:

   ```sh
   bundle exec rake check
   bundle exec rake compile_release
   ```

   `compile_release` builds with `RELEASE=true`, which verifies the packaging
   path for the precompiled native extension, the `rubydex_mcp` binary, and
   bundled third-party license output.

6. Commit the version bump directly on `main`:

   ```sh
   git add lib/rubydex/version.rb rust/Cargo.toml rust/rubydex-sys/Cargo.toml rust/Cargo.lock Gemfile.lock
   git commit -m "Bump version to vX.Y.Z"
   git push origin main
   ```

7. Tag the same commit using `v` followed by the generated Ruby version, then
   push the tag:

   ```sh
   git tag vX.Y.Z
   git push origin vX.Y.Z
   ```

   For example, Cargo version `1.2.3-beta.1` generates Ruby version
   `1.2.3.beta1`. Use `v1.2.3.beta1` in both tag commands for that release.

Pushing a tag matching `vX.Y.Z` or `vX.Y.Z.betaN` triggers the release workflow
in `.github/workflows/release.yml`, which publishes the gem to RubyGems and the
crates to crates.io. Workflow dispatch can be used for a dry run; only tag pushes
publish a release.
