#!/usr/bin/env ruby
# frozen_string_literal: true

require "benchmark"
require_relative "../lib/index"

# Simple Ruby Point class for comparison
class RubyPoint
  attr_reader :x, :y

  def initialize(x, y)
    @x = x
    @y = y
  end
end

puts "Ruby VM Boundary Overhead Benchmark"
puts "=" * 40

# Test different iteration counts to see scaling
iteration_counts = [1_000, 10_000, 100_000]

iteration_counts.each do |n|
  puts "\nTesting with #{n.to_s.reverse.gsub(/(\d{3})(?=\d)/, '\\1,').reverse} iterations:"
  puts "-" * 60

  Benchmark.bm(30) do |x|
    # Integer operations
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

    # Object creation
    x.report("Ruby Point creation:") do
      n.times { RubyPoint.new(10, 20) }
    end

    x.report("Native Point creation:") do
      n.times { Index::Point.new(10, 20) }
    end

    # Object creation + access
    x.report("Ruby Point create+access:") do
      n.times do
        point = RubyPoint.new(10, 20)
        point.x + point.y
      end
    end

    x.report("Native Point create+access:") do
      n.times do
        point = Index::Point.new(10, 20)
        point.x + point.y
      end
    end
  end

  # Calculate and display overhead
  puts "\nOverhead Analysis:"

  # Measure operations for overhead calculation
  ruby_constant_time = Benchmark.measure { n.times { 42 } }.real
  native_constant_time = Benchmark.measure { n.times { Index.get_constant_number } }.real

  ruby_increment_time = Benchmark.measure { n.times { 100 + 1 } }.real
  native_increment_time = Benchmark.measure { n.times { Index.increment_number(100) } }.real

  ruby_point_time = Benchmark.measure { n.times { RubyPoint.new(10, 20) } }.real
  native_point_time = Benchmark.measure { n.times { Index::Point.new(10, 20) } }.real

  ruby_point_access_time = Benchmark.measure do
    n.times do
      point = RubyPoint.new(10, 20)
      point.x + point.y
    end
  end.real

  native_point_access_time = Benchmark.measure do
    n.times do
      point = Index::Point.new(10, 20)
      point.x + point.y
    end
  end.real

  # Calculate overhead per call
  constant_overhead = (native_constant_time - ruby_constant_time) / n
  increment_overhead = (native_increment_time - ruby_increment_time) / n
  point_overhead = (native_point_time - ruby_point_time) / n
  point_access_overhead = (native_point_access_time - ruby_point_access_time) / n

  puts "  Function call overhead:        #{(constant_overhead * 1_000_000).round(2)}μs per call"
  puts "  Parameter passing overhead:    #{(increment_overhead * 1_000_000).round(2)}μs per call"
  puts "  Object creation overhead:      #{(point_overhead * 1_000_000).round(2)}μs per call"
  puts "  Object creation+access overhead: #{(point_access_overhead * 1_000_000).round(2)}μs per call"

  puts "\nSlowdown factors:"
  puts "  Native vs Ruby (constants):    #{(native_constant_time / ruby_constant_time).round(2)}x"
  puts "  Native vs Ruby (increments):   #{(native_increment_time / ruby_increment_time).round(2)}x"
  puts "  Native vs Ruby (objects):      #{(native_point_time / ruby_point_time).round(2)}x"
  puts "  Native vs Ruby (obj+access):   #{(native_point_access_time / ruby_point_access_time).round(2)}x"
end

puts "\n" + "=" * 60
puts "Summary:"
puts "This benchmark measures the overhead of crossing the Ruby VM boundary for:"
puts "  - Simple function calls (constants)"
puts "  - Parameter passing (increments)"
puts "  - Object creation (Points)"
puts "  - Object creation + field access"
puts ""
puts "Generally, native extensions provide benefits when the work-to-boundary-crossing"
puts "ratio is high. Simple operations like these will typically show overhead."
