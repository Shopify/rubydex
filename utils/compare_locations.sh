cargo build --release

utils/memory_check.sh target/release/index --mode locations ../corpus/tiny
echo -e "\n"
utils/memory_check.sh target/release/index --mode locations ../corpus/small
echo -e "\n"
utils/memory_check.sh target/release/index --mode locations ../corpus/medium
echo -e "\n"
utils/memory_check.sh target/release/index --mode locations ../corpus/large
echo -e "\n"
utils/memory_check.sh target/release/index --mode locations ../corpus/huge