extern crate cbindgen;

fn main() {
    cbindgen::generate(".")
        .expect("Unable to generate bindings")
        .write_to_file("rustbindings.h");

    // Set the install name for macOS dylibs so they can be found via @rpath at runtime.
    // This works for both native and cross-compilation scenarios.
    let target = std::env::var("TARGET").unwrap_or_default();
    if target.contains("apple") {
        println!("cargo::rustc-cdylib-link-arg=-Wl,-install_name,@rpath/librubydex_sys.dylib");
    }
}
