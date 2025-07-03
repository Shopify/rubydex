cargo build --release

utils/memory_check.sh target/release/index --mode data $@
echo -e "\n"
utils/memory_check.sh target/release/index --mode data $@ --simulate-cache
echo -e "\n"
utils/memory_check.sh target/release/index --mode enum $@