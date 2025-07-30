#!/usr/bin/env ruby
# frozen_string_literal: true

require_relative "lib/index"

puts "Creating repository..."
repository = Index::Repository.new

puts "Adding entries..."
repository.add_entry("key1", "value1")
repository.add_entry("key2", "value2")

puts "Verifying entries..."
entry1 = repository.get_entry("key1")
entry2 = repository.get_entry("key2")
puts "Entry1: #{entry1.name} = #{entry1.value}"
puts "Entry2: #{entry2.name} = #{entry2.value}"

puts "Creating tmp directory if it doesn't exist..."
Dir.mkdir("tmp") unless Dir.exist?("tmp")

puts "Calling dump_to_cache..."
repository.dump_to_cache

puts "This line should not be reached due to exit(0) in dump_to_cache"
