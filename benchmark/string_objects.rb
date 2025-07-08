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

# Test different iteration counts
iteration_counts = [1_000, 10_000, 100_000]

iteration_counts.each do |n|
  puts "\nTesting with #{n.to_s.reverse.gsub(/(\d{3})(?=\d)/, '\\1,').reverse} iterations:"
  puts "-" * 50

  Benchmark.bm(35) do |x|
    test_strings.each do |size_name, str|
      x.report("Ruby #{size_name} (#{str.length} chars):") do
        n.times { RubyMessage.new(str) }
      end

      x.report("Native #{size_name} (#{str.length} chars):") do
        n.times { Index::Message.new(str) }
      end
    end

    puts "\n" + "-" * 50
    puts "Creation + Access Tests:"
    puts "-" * 50

    test_strings.each do |size_name, str|
      x.report("Ruby #{size_name} create+access:") do
        n.times do
          msg = RubyMessage.new(str)
          msg.content.length
        end
      end

      x.report("Native #{size_name} create+access:") do
        n.times do
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
        n.times { ruby_msg.content.length }
      end

      x.report("Native #{size_name} access only:") do
        n.times { native_msg.content.length }
      end
    end
  end

  # Calculate and display overhead analysis
  puts "\nString Size Impact Analysis:"
  puts "-" * 50

  test_strings.each do |size_name, str|
    ruby_creation_time = Benchmark.measure { n.times { RubyMessage.new(str) } }.real
    native_creation_time = Benchmark.measure { n.times { Index::Message.new(str) } }.real

    ruby_create_access_time = Benchmark.measure do
      n.times do
        msg = RubyMessage.new(str)
        msg.content.length
      end
    end.real

    native_create_access_time = Benchmark.measure do
      n.times do
        msg = Index::Message.new(str)
        msg.content.length
      end
    end.real

    ruby_msg = RubyMessage.new(str)
    native_msg = Index::Message.new(str)

    ruby_access_time = Benchmark.measure { n.times { ruby_msg.content.length } }.real
    native_access_time = Benchmark.measure { n.times { native_msg.content.length } }.real

    creation_overhead = (native_creation_time - ruby_creation_time) / n
    create_access_overhead = (native_create_access_time - ruby_create_access_time) / n
    access_overhead = (native_access_time - ruby_access_time) / n

    puts "\n#{size_name.capitalize} string (#{str.length} characters):"
    puts "  Creation overhead:        #{(creation_overhead * 1_000_000).round(2)}μs per call"
    puts "  Creation+access overhead: #{(create_access_overhead * 1_000_000).round(2)}μs per call"
    puts "  Access overhead:          #{(access_overhead * 1_000_000).round(2)}μs per call"
    puts "  Creation slowdown:        #{(native_creation_time / ruby_creation_time).round(2)}x"
    puts "  Creation+access slowdown: #{(native_create_access_time / ruby_create_access_time).round(2)}x"
    puts "  Access slowdown:          #{(native_access_time / ruby_access_time).round(2)}x"
  end
end

puts "\n" + "=" * 50
puts "Summary:"
puts "This benchmark measures string object operations with various string sizes:"
puts "  - Object creation (Message with string content)"
puts "  - Object creation + content access"
puts "  - Content access on existing objects"
puts ""
puts "String marshaling overhead typically increases with string length due to:"
puts "  - UTF-8 validation and conversion"
puts "  - Memory allocation for string copies"
puts "  - Length-dependent copying costs"
puts ""
puts "For string-heavy operations, consider:"
puts "  - Batching multiple operations"
puts "  - Processing strings in native code"
puts "  - Using native string manipulation functions"
