cargo build --release

utils/memory_check.sh target/release/index --mode base $1
utils/memory_check.sh target/release/index --mode data $1
utils/memory_check.sh target/release/index --mode enum $1