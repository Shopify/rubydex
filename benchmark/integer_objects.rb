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

puts "Integer Object Creation Benchmark"
puts "=" * 40

# Test different iteration counts to see scaling
iteration_counts = [1_000, 10_000, 100_000]

iteration_counts.each do |n|
  puts "\nTesting with #{n.to_s.reverse.gsub(/(\d{3})(?=\d)/, '\\1,').reverse} iterations:"
  puts "-" * 40

  Benchmark.bm(30) do |x|
    x.report("Ruby Point creation:") do
      n.times { RubyPoint.new(10, 20) }
    end

    x.report("Native Point creation:") do
      n.times { Index::Point.new(10, 20) }
    end

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

    x.report("Ruby Point access only:") do
      point = RubyPoint.new(10, 20)
      n.times { point.x + point.y }
    end

    x.report("Native Point access only:") do
      point = Index::Point.new(10, 20)
      n.times { point.x + point.y }
    end
  end

  # Calculate and display overhead
  puts "\nOverhead Analysis:"

  ruby_creation_time = Benchmark.measure { n.times { RubyPoint.new(10, 20) } }.real
  native_creation_time = Benchmark.measure { n.times { Index::Point.new(10, 20) } }.real

  ruby_create_access_time = Benchmark.measure do
    n.times do
      point = RubyPoint.new(10, 20)
      point.x + point.y
    end
  end.real

  native_create_access_time = Benchmark.measure do
    n.times do
      point = Index::Point.new(10, 20)
      point.x + point.y
    end
  end.real

  # Pre-create objects for access-only test
  ruby_point = RubyPoint.new(10, 20)
  native_point = Index::Point.new(10, 20)

  ruby_access_time = Benchmark.measure { n.times { ruby_point.x + ruby_point.y } }.real
  native_access_time = Benchmark.measure { n.times { native_point.x + native_point.y } }.real

  # Calculate overhead per call
  creation_overhead = (native_creation_time - ruby_creation_time) / n
  create_access_overhead = (native_create_access_time - ruby_create_access_time) / n
  access_overhead = (native_access_time - ruby_access_time) / n

  puts "  Object creation overhead:        #{(creation_overhead * 1_000_000).round(2)}μs per call"
  puts "  Object creation+access overhead: #{(create_access_overhead * 1_000_000).round(2)}μs per call"
  puts "  Object access overhead:          #{(access_overhead * 1_000_000).round(2)}μs per call"

  puts "\nSlowdown factors:"
  puts "  Native vs Ruby (creation):        #{(native_creation_time / ruby_creation_time).round(2)}x"
  puts "  Native vs Ruby (creation+access): #{(native_create_access_time / ruby_create_access_time).round(2)}x"
  puts "  Native vs Ruby (access only):     #{(native_access_time / ruby_access_time).round(2)}x"
end

puts "\n" + "=" * 40
puts "Summary:"
puts "This benchmark measures the overhead of integer object operations:"
puts "  - Object creation (Point with two u32 fields)"
puts "  - Object creation + field access"
puts "  - Field access on existing objects"
puts ""
puts "Integer objects have minimal marshaling overhead but still show"
puts "boundary crossing costs. Consider batching operations or using"
puts "native objects for computation-heavy scenarios."
