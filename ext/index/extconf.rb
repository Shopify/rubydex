# frozen_string_literal: true

require "mkmf"

release = ENV["RELEASE"]
root_dir = Pathname.new("../..").expand_path(__dir__)
target_dir = root_dir.join("target")
target_dir = target_dir.join(release ? "release" : "debug")

append_ldflags("-Wl,-rpath,#{target_dir}")
append_ldflags("-L#{target_dir} -lindex")
create_makefile("index/index")
