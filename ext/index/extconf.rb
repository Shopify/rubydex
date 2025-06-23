# frozen_string_literal: true

require "mkmf"

# Get the absolute path to the target directory
root_dir = File.expand_path("../..", __dir__)
release = ENV["RELEASE"]
target_dir = if release
  File.join(root_dir, "target", "release")
else
  File.join(root_dir, "target", "debug")
end

# Build Rust library before compiling C extension
puts "Building Rust library..."
system("cargo build #{release ? '--release' : ''}".strip, chdir: root_dir) or abort("Rust build failed")

if Gem.win_platform?
  # On Windows, we need to ensure the DLL is copied to the correct location
  require "fileutils"
  dll_path = File.join(target_dir, "index.dll")
  FileUtils.cp(dll_path, "index.#{RbConfig::CONFIG["DLEXT"]}")
  FileUtils.cp(dll_path, File.join(root_dir, "lib", "index", "index.dll"))
else
  append_ldflags("-Wl,-rpath,#{target_dir}")
end

append_ldflags("-L#{target_dir} -lindex")
create_makefile("index/index")
