#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RUST_DIR="$SCRIPT_DIR/rust"
TARGET="/Users/hung-wulo/world/trees/root/src/areas/core/shopify/"
BINARY="$RUST_DIR/target/profiling/rubydex_cli"

# Pre-check: quick compile
cd "$RUST_DIR"
if ! cargo build --profile profiling 2>&1 | tail -3; then
    echo "METRIC compile_error=1"
    exit 1
fi
cd "$SCRIPT_DIR"

# Single run (3x median is too slow for an experiment loop — takes 3+ min per iteration)
TEMP_TIME=$(mktemp)
TEMP_OUT=$(mktemp)

/usr/bin/time -l "$BINARY" "$TARGET" --stats > "$TEMP_OUT" 2> "$TEMP_TIME"

# Parse from --stats output
stats_rss=$(grep "Maximum RSS:" "$TEMP_OUT" | awk '{print $3}')
total_time=$(grep "Total:" "$TEMP_OUT" | awk '{print $2}' | sed 's/s//')
resolution_time=$(grep "Resolution" "$TEMP_OUT" | awk '{print $2}' | sed 's/s//')
indexing_time=$(grep "Indexing" "$TEMP_OUT" | awk '{print $2}' | sed 's/s//')
declaration_count=$(grep "Total declarations:" "$TEMP_OUT" | awk '{print $3}')
definition_count=$(grep "TOTAL" "$TEMP_OUT" | awk '{print $2}')

# Parse from /usr/bin/time
peak_fp=$(grep "peak memory footprint" "$TEMP_TIME" | awk '{print $1}')

# Convert to MB
rss_mb=$(echo "scale=2; $stats_rss / 1024 / 1024" | bc -l)
fp_mb=$(echo "scale=2; $peak_fp / 1024 / 1024" | bc -l)

echo "METRIC rss_mb=$rss_mb"
echo "METRIC peak_footprint_mb=$fp_mb"
echo "METRIC total_time_s=$total_time"
echo "METRIC resolution_time_s=$resolution_time"
echo "METRIC indexing_time_s=$indexing_time"
echo "METRIC declaration_count=$declaration_count"
echo "METRIC definition_count=$definition_count"

rm -f "$TEMP_TIME" "$TEMP_OUT"
