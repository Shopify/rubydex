require "mkmf"

# Get the absolute path to the target directory
root_dir = File.expand_path("../..", __dir__)
target_dir = File.join(root_dir, "target", "release")

# Build Rust library before compiling C extension
puts "Building Rust library..."
system("cd #{root_dir} && cargo build --release") or abort("Rust build failed")

# Add the Rust target directory to the library search path
$LOCAL_LIBS << " -L#{target_dir} -lindex"

create_makefile("index/index")
