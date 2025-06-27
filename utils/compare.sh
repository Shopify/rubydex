cargo build --release

utils/memory_check.sh target/release/index --mode base $1
echo -e "\n"
utils/memory_check.sh target/release/index --mode data $1
echo -e "\n"
utils/memory_check.sh target/release/index --mode enum $1