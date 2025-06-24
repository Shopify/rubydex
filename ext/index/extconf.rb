# frozen_string_literal: true

require "mkmf"

# Get the absolute path to the target directory
root_dir = File.expand_path("../..", __dir__)
release = ENV["RELEASE"]
target_dir = Pathname.new(root_dir).join("target")
target_dir = target_dir.join(release ? "release" : "debug")

# Build Rust library before compiling C extension
puts "Building Rust library..."

rust_flags = []
rust_flags << "--release" if release

if Gem.win_platform?
  ENV["RUSTFLAGS"] = "-C target-feature=+crt-static"
  rust_flags << "--target x86_64-pc-windows-gnu"
end

system("cargo build #{rust_flags.join(" ")}".strip, chdir: root_dir) or abort("Rust build failed")

if Gem.win_platform?
  append_ldflags(target_dir.join("index.lib").to_s)

  $LDFLAGS << " -Wl,-Bstatic"
  $LDFLAGS << " -static-libgcc -static-libstdc++"

  # These libraries are the ones informed by `cargo rustc -- --print native-static-libs`
  windows_libraries = ["kernel32", "ntdll", "userenv", "ws2_32", "dbghelp", "msvcrt"]
  windows_libraries.each { |lib| $LDFLAGS << " -l#{lib}" }

  $LDFLAGS << " -Wl,-Bdynamic"
else
  append_ldflags("-Wl,-rpath,#{target_dir}")
  append_ldflags("-L#{target_dir} -lindex")
end

create_makefile("index/index")
