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

# Test with 100k iterations
ITERATIONS = 100_000

puts "\nTesting with #{ITERATIONS.to_s.reverse.gsub(/(\d{3})(?=\d)/, '\\1,').reverse} iterations:"
puts "-" * 40

Benchmark.bm(30) do |x|
  x.report("Ruby Point creation:") do
    ITERATIONS.times { RubyPoint.new(10, 20) }
  end

  x.report("Native Point creation:") do
    ITERATIONS.times { Index::Point.new(10, 20) }
  end

  x.report("Ruby Point create+access:") do
    ITERATIONS.times do
      point = RubyPoint.new(10, 20)
      point.x + point.y
    end
  end

  x.report("Native Point create+access:") do
    ITERATIONS.times do
      point = Index::Point.new(10, 20)
      point.x + point.y
    end
  end

  x.report("Ruby Point access only:") do
    point = RubyPoint.new(10, 20)
    ITERATIONS.times { point.x + point.y }
  end

  x.report("Native Point access only:") do
    point = Index::Point.new(10, 20)
    ITERATIONS.times { point.x + point.y }
  end
end

# Calculate and display overhead
puts "\nOverhead Analysis:"

ruby_creation_time = Benchmark.measure { ITERATIONS.times { RubyPoint.new(10, 20) } }.real
native_creation_time = Benchmark.measure { ITERATIONS.times { Index::Point.new(10, 20) } }.real

ruby_create_access_time = Benchmark.measure do
  ITERATIONS.times do
    point = RubyPoint.new(10, 20)
    point.x + point.y
  end
end.real

native_create_access_time = Benchmark.measure do
  ITERATIONS.times do
    point = Index::Point.new(10, 20)
    point.x + point.y
  end
end.real

# Pre-create objects for access-only test
ruby_point = RubyPoint.new(10, 20)
native_point = Index::Point.new(10, 20)

ruby_access_time = Benchmark.measure { ITERATIONS.times { ruby_point.x + ruby_point.y } }.real
native_access_time = Benchmark.measure { ITERATIONS.times { native_point.x + native_point.y } }.real

# Calculate overhead per call
creation_overhead = (native_creation_time - ruby_creation_time) / ITERATIONS
create_access_overhead = (native_create_access_time - ruby_create_access_time) / ITERATIONS
access_overhead = (native_access_time - ruby_access_time) / ITERATIONS

puts "  Object creation overhead:        #{(creation_overhead * 1_000_000).round(2)}μs per call"
puts "  Object creation+access overhead: #{(create_access_overhead * 1_000_000).round(2)}μs per call"
puts "  Object access overhead:          #{(access_overhead * 1_000_000).round(2)}μs per call"

puts "\nSlowdown factors:"
puts "  Native vs Ruby (creation):        #{(native_creation_time / ruby_creation_time).round(2)}x"
puts "  Native vs Ruby (creation+access): #{(native_create_access_time / ruby_create_access_time).round(2)}x"
puts "  Native vs Ruby (access only):     #{(native_access_time / ruby_access_time).round(2)}x"

puts "\n" + "=" * 40
