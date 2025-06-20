extern crate cbindgen;

fn main() {
    cbindgen::generate(".")
        .expect("Unable to generate bindings")
        .write_to_file("ext/index/rustbindings.h");
}
