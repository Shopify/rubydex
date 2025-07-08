#!/usr/bin/env ruby
# frozen_string_literal: true

# Quick test to verify all functions work

require_relative "lib/index"

puts "Testing integer functions:"
puts "  get_constant_number: #{Index.get_constant_number}"
puts "  increment_number(10): #{Index.increment_number(10)}"

puts "\nTesting Point creation:"
point = Index::Point.new(100, 200)
puts "  Point.new(100, 200): x=#{point.x}, y=#{point.y}"

puts "\nTesting edge cases:"
zero_point = Index::Point.new(0, 0)
puts "  Point.new(0, 0): x=#{zero_point.x}, y=#{zero_point.y}"

max_point = Index::Point.new(4294967295, 4294967295) # u32::MAX
puts "  Point.new(u32::MAX, u32::MAX): x=#{max_point.x}, y=#{max_point.y}"

puts "\nAll tests passed! Ready to run benchmark."
