extern crate cbindgen;

fn main() {
    // Existing cbindgen build
    cbindgen::generate(&std::env::var("CARGO_MANIFEST_DIR").unwrap())
        .expect("Unable to generate bindings")
        .write_to_file("ext/index/rustbindings.h");

    // Add prost build for Protocol Buffers
    prost_build::compile_protos(&["proto/comments.proto"], &["proto/"]).unwrap();
}
