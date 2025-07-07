# frozen_string_literal: true

require "mkmf"
require "pathname"

release = ENV["RELEASE"]
root_dir = Pathname.new("../..").expand_path(__dir__)
target_dir = root_dir.join("target")
target_dir = target_dir.join("x86_64-pc-windows-gnu") if Gem.win_platform?
target_dir = target_dir.join(release ? "release" : "debug")

cargo_args = ["--manifest-path #{root_dir.join("Cargo.toml")}"]
cargo_args << "--release" if release

if Gem.win_platform?
  cargo_args << "--target x86_64-pc-windows-gnu"
  ENV["RUSTFLAGS"] = "-C target-feature=+crt-static"
end

if Gem.win_platform?
  $LDFLAGS << " #{target_dir.join("libindex.a")}"

  # On Windows, statically link system libraries to avoid having to distribute and load DLLs
  #
  # These libraries are the ones informed by `cargo rustc -- --print native-static-libs`, which displays the
  # libraries necessary for statically linking the Rust code on the current platform
  ["kernel32", "ntdll", "userenv", "ws2_32", "dbghelp", "msvcrt"].each do |lib|
    $LDFLAGS << " -l#{lib}"
  end
else
  append_ldflags("-Wl,-rpath,#{target_dir}")
  # We cannot use append_ldflags here because the Rust code is only compiled later. If it's not compiled yet, this will
  # fail and the flag will not be added
  $LDFLAGS << " -L#{target_dir} -lindex"
end

create_makefile("index/index")
cargo_command = "cargo build #{cargo_args.join(" ")}".strip

rust_srcs = Dir.glob("#{root_dir}/src/**/*.rs")

makefile = File.read("Makefile")
new_makefile = makefile.gsub("$(OBJS): $(HDRS) $(ruby_headers)", <<~MAKEFILE.chomp)
  .PHONY: compile_rust
  RUST_SRCS = #{File.expand_path("Cargo.toml", root_dir)} #{File.expand_path("Cargo.lock", root_dir)} #{rust_srcs.join(" ")}

  .rust_built: $(RUST_SRCS)
  \t#{cargo_command} || (echo "Compiling Rust failed" && exit 1)
  \ttouch $@

  compile_rust: .rust_built

  $(OBJS): $(HDRS) $(ruby_headers) .rust_built
MAKEFILE

new_makefile.gsub!("$(Q) $(POSTLINK)", <<~MAKEFILE.chomp)
  $(Q) $(POSTLINK)
  \t$(Q)$(RM) .rust_built
MAKEFILE
File.write("Makefile", new_makefile)
