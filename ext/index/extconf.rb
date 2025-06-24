require "mkmf"

# Get the absolute path to the target directory
root_dir = File.expand_path("../..", __dir__)
release = ENV["RELEASE"]
if release
  build_dir = File.join(root_dir, "target", "release")
else
  build_dir = File.join(root_dir, "target", "debug")
end

# Build Rust library before compiling C extension
puts "Building Rust library..."
system("cd #{root_dir} && cargo build #{release ? '--release' : ''}") or abort("Rust build failed")

# Add the Rust target directory to the library search path
$LOCAL_LIBS << " -L#{build_dir} -lindex"

create_makefile("index/index")
