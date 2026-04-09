#!/usr/bin/env bash
set -euo pipefail

cd /Users/stock/src/github.com/Shopify/rubydex_2/rust

# Pre-check: compile first so we don't measure compile time
cargo build --release --bin rubydex_cli 2>&1 | tail -1

# Run 3 times and take the median
declare -a times
declare -a mems
file_count=0

for i in 1 2 3; do
  output=$(cargo run --release --bin rubydex_cli -- \
    /Users/stock/world/trees/root/src/areas/core/shopify \
    --stop-after=listing --stats 2>&1)

  t=$(echo "$output" | grep "Listing" | awk '{print $2}' | sed 's/s//')
  m=$(echo "$output" | grep "Maximum RSS" | awk -F'[()]' '{print $2}' | awk '{print $1}')
  times+=("$t")
  mems+=("$m")

  # Count files from a single run
  if [ "$i" -eq 1 ]; then
    # The listing run doesn't print file count with --stop-after=listing
    # We'll get it from a separate quick run
    :
  fi
done

# Sort times and take median (2nd of 3)
IFS=$'\n' sorted_times=($(sort -n <<<"${times[*]}")); unset IFS
IFS=$'\n' sorted_mems=($(sort -n <<<"${mems[*]}")); unset IFS

median_time="${sorted_times[1]}"
median_mem="${sorted_mems[1]}"

echo "METRIC listing_time=${median_time}"
echo "METRIC memory_mb=${median_mem}"
