#!/bin/bash

# Check if command was provided
if [ $# -eq 0 ]; then
    echo "Usage: $0 <command> [args...]"
    echo "Example: $0 cargo run ../rbi"
    exit 1
fi

# Create temporary file for time output
temp_file=$(mktemp)

# Run the command with time measurement
echo "Running: $*"
echo "----------------------------------------"

# Run command and capture time output to temp file
/usr/bin/time -l "$@" 2> "$temp_file"
exit_code=$?

echo "----------------------------------------"

# Extract memory statistics
mrss=$(grep "maximum resident set size" "$temp_file" | awk '{print $1}')
peak=$(grep "peak memory footprint" "$temp_file" | awk '{print $1}')

# Convert bytes to MB for readability
if [ -n "$mrss" ]; then
    mrss_mb=$(echo "scale=2; $mrss / 1024 / 1024" | bc -l 2>/dev/null || echo "$(($mrss / 1024 / 1024))")
    echo "Maximum Resident Set Size: $mrss bytes (${mrss_mb} MB)"
fi

if [ -n "$peak" ]; then
    peak_mb=$(echo "scale=2; $peak / 1024 / 1024" | bc -l 2>/dev/null || echo "$(($peak / 1024 / 1024))")
    echo "Peak Memory Footprint:     $peak bytes (${peak_mb} MB)"
fi

# Also show execution time
real_time=$(grep "real" "$temp_file" | awk '{print $1}')
if [ -n "$real_time" ]; then
    echo "Execution Time:            ${real_time} seconds"
fi

# Clean up
rm "$temp_file"

# Exit with the same code as the original command
exit $exit_code