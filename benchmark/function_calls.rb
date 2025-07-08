#!/usr/bin/env ruby
# frozen_string_literal: true

require "benchmark"
require_relative "../lib/index"

puts "Function Call Boundary Overhead Benchmark"
puts "=" * 50

# Test with 100k iterations
ITERATIONS = 100_000

puts "\nTesting with #{ITERATIONS.to_s.reverse.gsub(/(\d{3})(?=\d)/, '\\1,').reverse} iterations:"
puts "-" * 50

Benchmark.bm(25) do |x|
  x.report("Ruby constant (42):") do
    ITERATIONS.times { 42 }
  end

  x.report("Native constant:") do
    ITERATIONS.times { Index.get_constant_number }
  end

  x.report("Ruby increment (100+1):") do
    ITERATIONS.times { 100 + 1 }
  end

  x.report("Native increment:") do
    ITERATIONS.times { Index.increment_number(100) }
  end
end

# Calculate and display overhead
puts "\nOverhead Analysis:"

# Measure operations for overhead calculation
ruby_constant_time = Benchmark.measure { ITERATIONS.times { 42 } }.real
native_constant_time = Benchmark.measure { ITERATIONS.times { Index.get_constant_number } }.real

ruby_increment_time = Benchmark.measure { ITERATIONS.times { 100 + 1 } }.real
native_increment_time = Benchmark.measure { ITERATIONS.times { Index.increment_number(100) } }.real

# Calculate overhead per call
constant_overhead = (native_constant_time - ruby_constant_time) / ITERATIONS
increment_overhead = (native_increment_time - ruby_increment_time) / ITERATIONS

puts "  Function call overhead:      #{(constant_overhead * 1_000_000).round(2)}μs per call"
puts "  Parameter passing overhead:  #{(increment_overhead * 1_000_000).round(2)}μs per call"

puts "\nSlowdown factors:"
puts "  Native vs Ruby (constants):  #{(native_constant_time / ruby_constant_time).round(2)}x"
puts "  Native vs Ruby (increments): #{(native_increment_time / ruby_increment_time).round(2)}x"

puts "\n" + "=" * 50
