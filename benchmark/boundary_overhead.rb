#!/usr/bin/env ruby
# frozen_string_literal: true

require "benchmark"
require_relative "../lib/index"

puts "Ruby VM Boundary Overhead Benchmark"
puts "=" * 40

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

  # Measure pure Ruby operations
  ruby_constant_time = Benchmark.measure { n.times { 42 } }.real
  ruby_increment_time = Benchmark.measure { n.times { 100 + 1 } }.real

  # Measure native operations
  native_constant_time = Benchmark.measure { n.times { Index.get_constant_number } }.real
  native_increment_time = Benchmark.measure { n.times { Index.increment_number(100) } }.real

  # Calculate overhead per call
  constant_overhead = (native_constant_time - ruby_constant_time) / n
  increment_overhead = (native_increment_time - ruby_increment_time) / n

  puts "  Constant call overhead: #{(constant_overhead * 1_000_000).round(2)}μs per call"
  puts "  Increment call overhead: #{(increment_overhead * 1_000_000).round(2)}μs per call"
  puts "  Native vs Ruby slowdown: #{(native_constant_time / ruby_constant_time).round(2)}x for constants"
  puts "  Native vs Ruby slowdown: #{(native_increment_time / ruby_increment_time).round(2)}x for increments"
end

puts "\n" + "=" * 40
puts "Summary:"
puts "This benchmark measures the pure overhead of crossing the Ruby VM boundary."
puts "The 'constant' functions test call overhead with no parameters."
puts "The 'increment' functions test call overhead with one parameter and return value."
puts "Higher overhead indicates more expensive boundary crossing."
