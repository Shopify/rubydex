# frozen_string_literal: true

require "mkmf"
require "pathname"

unless system("cargo", "--version", out: File::NULL, err: File::NULL)
  abort "Installing Rubydex requires Cargo, the Rust package manager for platforms that we do not precompile binaries."
end

gem_dir = Pathname.new("../..").expand_path(__dir__)
release = ENV["RELEASE"] || !gem_dir.join(".git").exist?
root_dir = gem_dir.join("rust")
target_dir = root_dir.join("target")
target_dir = target_dir.join("x86_64-pc-windows-gnu") if Gem.win_platform?
target_dir = target_dir.join(release ? "release" : "debug")

bindings_path = root_dir.join("rubydex-sys").join("rustbindings.h")

cargo_args = ["--manifest-path #{root_dir.join("Cargo.toml")}"]
cargo_args << "--release" if release

if Gem.win_platform?
  cargo_args << "--target x86_64-pc-windows-gnu"
  ENV["RUSTFLAGS"] = "-C target-feature=+crt-static"
end

append_cflags("-Werror=unused-but-set-variable")
append_cflags("-Werror=implicit-function-declaration")

if Gem.win_platform?
  $LDFLAGS << " #{target_dir.join("librubydex_sys.a")}"

  # On Windows, statically link system libraries to avoid having to distribute and load DLLs
  #
  # These libraries are the ones informed by `cargo rustc -- --print native-static-libs`, which displays the
  # libraries necessary for statically linking the Rust code on the current platform
  ["kernel32", "ntdll", "userenv", "ws2_32", "dbghelp", "msvcrt"].each do |lib|
    $LDFLAGS << " -l#{lib}"
  end
else
  if RUBY_PLATFORM.include?("darwin")
    # On the precompiled version of the gem, the `dylib` is one folder above the `.bundle/.so` file. For on machine
    # compilation, they are at the same level
    append_ldflags("-Wl,-rpath,@loader_path")
    append_ldflags("-Wl,-rpath,@loader_path/..")
  else
    $LDFLAGS << " -Wl,-rpath,\\$$ORIGIN"
    $LDFLAGS << " -Wl,-rpath,\\$$ORIGIN/.."
  end

  # We cannot use append_ldflags here because the Rust code is only compiled later. If it's not compiled yet, this will
  # fail and the flag will not be added
  $LDFLAGS << " -L#{target_dir} -lrubydex_sys"
end

create_makefile("rubydex/rubydex")

cargo_command = if ENV["SANITIZER"]
  ENV["RUSTFLAGS"] = "-Zsanitizer=#{ENV["SANITIZER"]} -C target-cpu=x86-64-v3"
  "cargo +nightly build -Zbuild-std #{cargo_args.join(" ")}".strip
else
  "cargo build #{cargo_args.join(" ")}".strip
end

lib_dir = gem_dir.join("lib").join("rubydex")

copy_dylib_commands = if Gem.win_platform?
  ""
elsif RUBY_PLATFORM.include?("darwin")
  src_dylib = target_dir.join("librubydex_sys.dylib")
  "\t$(COPY) #{src_dylib} #{lib_dir}"
else
  # Linux
  src_dylib = target_dir.join("librubydex_sys.so")
  "\t$(COPY) #{src_dylib} #{lib_dir}"
end

rust_srcs = Dir.glob("#{root_dir}/**/*.rs").reject { |path| path.include?("rust/target") }
makefile = File.read("Makefile")

new_makefile = makefile.gsub("$(OBJS): $(HDRS) $(ruby_headers)", <<~MAKEFILE.chomp)
  .PHONY: compile_rust
  RUST_SRCS = #{File.expand_path("Cargo.toml", root_dir)} #{File.expand_path("Cargo.lock", root_dir)} #{rust_srcs.join(" ")}

  .rust_built: $(RUST_SRCS)
  \t#{cargo_command} || (echo "Compiling Rust failed" && exit 1)
  \t$(COPY) #{bindings_path} #{__dir__}
  \ttouch $@

  compile_rust: .rust_built

  $(OBJS): $(HDRS) $(ruby_headers) .rust_built
MAKEFILE

new_makefile.gsub!(/(\$\(Q\) \$\(LDSHARED\) .*)/, <<~MAKEFILE.chomp)
  \\1
  #{copy_dylib_commands}
  \t$(Q)$(RM) .rust_built
MAKEFILE

# Bundle all dependency licenses when building a release version of the gem. This only has to happen on CI where we
# precompile binaries. Oherwise, we're not redistributing the Rust dependencies as they are getting downloaded, compiled
# and linked on the user's machine
if release && system("cargo", "about", "--version", out: File::NULL, err: File::NULL)
  licenses_file = root_dir.join("THIRD_PARTY_LICENSES.html")
  about_config = root_dir.join("about.toml")
  about_template = root_dir.join("about.hbs")

  new_makefile.gsub!(".rust_built: $(RUST_SRCS)", <<~MAKEFILE.chomp)
    #{licenses_file}: #{about_config} #{about_template}
    \t$(Q)$(RM) #{licenses_file}
    \tcargo about generate #{about_template} --manifest-path #{root_dir.join("Cargo.toml")} --workspace > #{licenses_file}
    \t$(COPY) #{licenses_file} #{gem_dir}

    .rust_built: $(RUST_SRCS) #{licenses_file}
  MAKEFILE
end

File.write("Makefile", new_makefile)

begin
  require "extconf_compile_commands_json"

  ExtconfCompileCommandsJson.generate!
  ExtconfCompileCommandsJson.symlink!
rescue LoadError # rubocop:disable Lint/SuppressedException
end
