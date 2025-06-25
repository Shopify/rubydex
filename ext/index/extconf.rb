# frozen_string_literal: true

require "mkmf"

release = ENV["RELEASE"]
root_dir = Pathname.new("../..").expand_path(__dir__)
target_dir = root_dir.join("target")
target_dir = target_dir.join("x86_64-pc-windows-gnu") if Gem.win_platform?
target_dir = target_dir.join(release ? "release" : "debug")

abort("Target directory \"#{target_dir}\" does not exist") unless target_dir.exist?

if Gem.win_platform?
  object_path = target_dir.join("libindex.a")
  abort("Object file \"#{object_path}\" does not exist") unless object_path.exist?
  append_ldflags(object_path.to_s)

  # On Windows, statically link system libraries to avoid having to distribute and load DLLs
  #
  # These libraries are the ones informed by `cargo rustc -- --print native-static-libs`, which displays the
  # libraries necessary for statically linking the Rust code on the current platform
  ["kernel32", "ntdll", "userenv", "ws2_32", "dbghelp", "msvcrt"].each do |lib|
    append_ldflags("-l#{lib}")
  end
else
  append_ldflags("-Wl,-rpath,#{target_dir}")
  append_ldflags("-L#{target_dir} -lindex")
end

create_makefile("index/index")
