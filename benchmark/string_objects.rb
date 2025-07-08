#!/usr/bin/env ruby
# frozen_string_literal: true

require "benchmark"
require_relative "../lib/index"

# Simple Ruby Message class for comparison
class RubyMessage
  attr_reader :content

  def initialize(content)
    @content = content
  end
end

puts "String Object Creation Benchmark"
puts "=" * 40

# Test strings of different sizes
test_strings = {
  "tiny" => "Hi!",
  "short" => "Hello, World!",
  "medium" => "This is a medium-length message for testing boundary overhead with string marshaling. " * 2,
  "long" => "This is a much longer string that we'll use to test how string marshaling overhead scales with string length. " * 5,
  "very_long" => "This is an extremely long string to test the upper bounds of string marshaling performance across the Ruby VM boundary. " * 20,
}

# Test with 100k iterations
ITERATIONS = 100_000

puts "\nTesting with #{ITERATIONS.to_s.reverse.gsub(/(\d{3})(?=\d)/, '\\1,').reverse} iterations:"
puts "-" * 50

Benchmark.bm(35) do |x|
  test_strings.each do |size_name, str|
    x.report("Ruby #{size_name} (#{str.length} chars):") do
      ITERATIONS.times { RubyMessage.new(str) }
    end

    x.report("Native #{size_name} (#{str.length} chars):") do
      ITERATIONS.times { Index::Message.new(str) }
    end
  end

  puts "\n" + "-" * 50
  puts "Creation + Access Tests:"
  puts "-" * 50

  test_strings.each do |size_name, str|
    x.report("Ruby #{size_name} create+access:") do
      ITERATIONS.times do
        msg = RubyMessage.new(str)
        msg.content.length
      end
    end

    x.report("Native #{size_name} create+access:") do
      ITERATIONS.times do
        msg = Index::Message.new(str)
        msg.content.length
      end
    end
  end

  puts "\n" + "-" * 50
  puts "Access Only Tests:"
  puts "-" * 50

  test_strings.each do |size_name, str|
    ruby_msg = RubyMessage.new(str)
    native_msg = Index::Message.new(str)

    x.report("Ruby #{size_name} access only:") do
      ITERATIONS.times { ruby_msg.content.length }
    end

    x.report("Native #{size_name} access only:") do
      ITERATIONS.times { native_msg.content.length }
    end
  end
end

# Calculate and display overhead analysis
puts "\nString Size Impact Analysis:"
puts "-" * 50

test_strings.each do |size_name, str|
  ruby_creation_time = Benchmark.measure { ITERATIONS.times { RubyMessage.new(str) } }.real
  native_creation_time = Benchmark.measure { ITERATIONS.times { Index::Message.new(str) } }.real

  ruby_create_access_time = Benchmark.measure do
    ITERATIONS.times do
      msg = RubyMessage.new(str)
      msg.content.length
    end
  end.real

  native_create_access_time = Benchmark.measure do
    ITERATIONS.times do
      msg = Index::Message.new(str)
      msg.content.length
    end
  end.real

  ruby_msg = RubyMessage.new(str)
  native_msg = Index::Message.new(str)

  ruby_access_time = Benchmark.measure { ITERATIONS.times { ruby_msg.content.length } }.real
  native_access_time = Benchmark.measure { ITERATIONS.times { native_msg.content.length } }.real

  creation_overhead = (native_creation_time - ruby_creation_time) / ITERATIONS
  create_access_overhead = (native_create_access_time - ruby_create_access_time) / ITERATIONS
  access_overhead = (native_access_time - ruby_access_time) / ITERATIONS

  puts "\n#{size_name.capitalize} string (#{str.length} characters):"
  puts "  Creation overhead:        #{(creation_overhead * 1_000_000).round(2)}μs per call"
  puts "  Creation+access overhead: #{(create_access_overhead * 1_000_000).round(2)}μs per call"
  puts "  Access overhead:          #{(access_overhead * 1_000_000).round(2)}μs per call"
  puts "  Creation slowdown:        #{(native_creation_time / ruby_creation_time).round(2)}x"
  puts "  Creation+access slowdown: #{(native_create_access_time / ruby_create_access_time).round(2)}x"
  puts "  Access slowdown:          #{(native_access_time / ruby_access_time).round(2)}x"
end

puts "\n" + "=" * 50
