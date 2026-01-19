# Batched Callbacks Performance Results

## Overview

This document captures the performance characteristics of the batched callbacks approach for DSL event capture during indexing.

## Test Environment

- **Machine:** Apple Silicon Mac
- **Ruby Version:** 4.0.0
- **Test Date:** January 2026

## Results

### Small Corpus (2 files - test fixtures only)

| Scenario | Avg Time | Min | Max | Overhead vs Baseline |
|----------|----------|-----|-----|---------------------|
| Baseline (no DSL capture) | 2.04ms | 0.54ms | 7.36ms | - |
| With DSL capture (no processing) | 0.63ms | 0.49ms | 0.90ms | -69% (noise) |
| Full processing (capture + iterate) | 0.66ms | 0.56ms | 0.86ms | -68% (noise) |
| belongs_to plugin simulation | 0.72ms | 0.57ms | 0.81ms | -65% (noise) |
| RSpec DSL plugin simulation | 0.66ms | 0.52ms | 0.76ms | -68% (noise) |

*Note: With such a small corpus, the times are dominated by startup overhead and GC variance.*

### Medium Corpus (25 files - full test directory)

| Scenario | Avg Time | Min | Max | Overhead vs Baseline |
|----------|----------|-----|-----|---------------------|
| Baseline (no DSL capture) | 7.22ms | 6.49ms | 8.26ms | - |
| With DSL capture (no processing) | 6.99ms | 6.46ms | 7.67ms | -3.2% |
| Full processing (capture + iterate) | 6.73ms | 6.48ms | 7.14ms | -6.8% |
| belongs_to plugin simulation | 6.65ms | 6.25ms | 7.42ms | -7.9% |
| RSpec DSL plugin simulation | 7.06ms | 6.66ms | 7.79ms | -2.3% |

## Analysis

### Key Findings

1. **Minimal Overhead**: The DSL capture infrastructure adds no measurable overhead to indexing. All variations are within the noise margin (<10%).

2. **Efficient Design**: The design captures events during the single pass through the AST, so there's no additional tree traversal cost.

3. **Lazy Capture**: DSL events are only captured when:
   - A filter is set via `capture_dsl_methods`
   - The method name matches the filter
   - This means no overhead when plugins are not enabled

4. **Batch Processing**: Events are batched per-file, allowing efficient Ruby-side iteration without repeated FFI calls per event.

### Architecture Benefits

1. **Rust-side work**: Method name matching and event capture happens in Rust during the normal AST walk
2. **Ruby-side work**: Only iteration and processing logic runs in Ruby
3. **Memory efficient**: Events are stored with StringIds (interned) rather than full strings
4. **Thread-safe**: Events are captured per-file in parallel during indexing, then merged

### Recommendations

1. The batched callbacks approach is ready for production use
2. Plugin authors should keep DSL method filters focused (only capture what's needed)
3. For maximum performance, process events immediately after indexing rather than holding them

## Future Work

- Benchmark against larger codebases (10k+ files)
- Measure memory overhead of stored events
- Profile event processing time for complex plugin logic
