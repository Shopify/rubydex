#!/usr/bin/env ruby
# frozen_string_literal: true

require "benchmark"
require_relative "../lib/index"

puts "Function Call Boundary Overhead Benchmark"
puts "=" * 50

# Test different iteration counts to see scaling
iteration_counts = [1_000, 10_000, 100_000, 1_000_000]

iteration_counts.each do |n|
  puts "\nTesting with #{n.to_s.reverse.gsub(/(\d{3})(?=\d)/, '\\1,').reverse} iterations:"
  puts "-" * 50

  Benchmark.bm(25) do |x|
    x.report("Ruby constant (42):") do
      n.times { 42 }
    end

    x.report("Native constant:") do
      n.times { Index.get_constant_number }
    end

    x.report("Ruby increment (100+1):") do
      n.times { 100 + 1 }
    end

    x.report("Native increment:") do
      n.times { Index.increment_number(100) }
    end
  end

  # Calculate and display overhead
  puts "\nOverhead Analysis:"

  # Measure operations for overhead calculation
  ruby_constant_time = Benchmark.measure { n.times { 42 } }.real
  native_constant_time = Benchmark.measure { n.times { Index.get_constant_number } }.real

  ruby_increment_time = Benchmark.measure { n.times { 100 + 1 } }.real
  native_increment_time = Benchmark.measure { n.times { Index.increment_number(100) } }.real

  # Calculate overhead per call
  constant_overhead = (native_constant_time - ruby_constant_time) / n
  increment_overhead = (native_increment_time - ruby_increment_time) / n

  puts "  Function call overhead:      #{(constant_overhead * 1_000_000).round(2)}μs per call"
  puts "  Parameter passing overhead:  #{(increment_overhead * 1_000_000).round(2)}μs per call"

  puts "\nSlowdown factors:"
  puts "  Native vs Ruby (constants):  #{(native_constant_time / ruby_constant_time).round(2)}x"
  puts "  Native vs Ruby (increments): #{(native_increment_time / ruby_increment_time).round(2)}x"
end

puts "\n" + "=" * 50
puts "Summary:"
puts "This benchmark measures the base overhead of crossing the Ruby VM boundary:"
puts "  - Simple function calls (no parameters)"
puts "  - Parameter passing (one u32 parameter)"
puts ""
puts "These represent the minimum overhead for native function calls."
puts "More complex operations must exceed this overhead to be worthwhile."
