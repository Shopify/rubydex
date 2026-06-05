# rubydex-sys

`rubydex-sys` exposes the Rubydex Rust library through a C FFI boundary.

This crate is primarily consumed by the Rubydex Ruby native extension. It
builds static and dynamic libraries plus generated C bindings for integrating
the Rust indexer with the Ruby VM.

Most users should depend on the `rubydex` Ruby gem or the `rubydex` Rust crate
instead of using this crate directly.
