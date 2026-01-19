# frozen_string_literal: true

require_relative "../lib/rubydex"

# Use a realistic test corpus - index test fixtures by default
TEST_CORPUS = ENV.fetch("BENCHMARK_CORPUS", File.expand_path("../test/fixtures", __dir__))

def run_benchmark(label, iterations: 5, &block)
  puts "\n#{label}"
  puts "-" * 40

  times = iterations.times.map do
    GC.start
    start_time = Process.clock_gettime(Process::CLOCK_MONOTONIC)
    block.call
    Process.clock_gettime(Process::CLOCK_MONOTONIC) - start_time
  end

  avg = times.sum / times.size
  min = times.min
  max = times.max

  puts "  Avg: #{(avg * 1000).round(2)}ms"
  puts "  Min: #{(min * 1000).round(2)}ms"
  puts "  Max: #{(max * 1000).round(2)}ms"

  avg
end

results = {}
files = Dir.glob("#{TEST_CORPUS}/**/*.rb")
puts "Benchmarking with #{files.size} files from #{TEST_CORPUS}"

# 1. Baseline: Current indexing without DSL capture enabled
results[:baseline] = run_benchmark("Baseline (no DSL capture)") do
  graph = Rubydex::Graph.new
  graph.index_all(files)
  graph.resolve
end

# 2. Infrastructure only: With DSL event capture but not processing events
results[:infra_only] = run_benchmark("With DSL capture (no processing)") do
  graph = Rubydex::Graph.new
  graph.capture_dsl_methods(["belongs_to", "has_many", "has_one", "describe", "context", "let", "let!", "subject"])
  graph.index_all(files)
  # DSL events captured but not processed
  graph.resolve
end

# 3. Full processing: Capture and iterate through all events
results[:full_processing] = run_benchmark("Full processing (capture + iterate)") do
  graph = Rubydex::Graph.new
  graph.capture_dsl_methods(["belongs_to", "has_many", "has_one", "describe", "context", "let", "let!", "subject"])
  graph.index_all(files)

  # Process all DSL events
  event_count = 0
  graph.each_dsl_file do |_file_path, events|
    events.each do |event|
      event_count += 1
      # Access all event attributes (simulating plugin processing)
      event.method_name
      event.arguments
      event.parent_id
      event.has_block
      event.offset
    end
  end

  graph.resolve
end

# 4. Scenario: belongs_to plugin simulation
results[:belongs_to_plugin] = run_benchmark("Scenario: belongs_to plugin") do
  graph = Rubydex::Graph.new
  graph.capture_dsl_methods(["belongs_to", "has_many", "has_one"])
  graph.index_all(files)

  graph.each_dsl_file do |file_path, events|
    events.each do |event|
      case event.method_name
      when "belongs_to", "has_one"
        # Would add getter and setter methods
        entity = event.arguments.first
        next unless entity
        # graph.add_method(owner:, name: entity, file_path:, line:, column:)
        # graph.add_method(owner:, name: "#{entity}=", file_path:, line:, column:)
      when "has_many"
        # Would add collection methods
        entity = event.arguments.first
        next unless entity
        # graph.add_method(owner:, name: entity, file_path:, line:, column:)
      end
    end
  end

  graph.resolve
end

# 5. Scenario: RSpec DSL plugin simulation
results[:rspec_plugin] = run_benchmark("Scenario: RSpec DSL plugin") do
  graph = Rubydex::Graph.new
  graph.capture_dsl_methods(["describe", "context", "let", "let!", "subject"])
  graph.index_all(files)

  context_stack = []
  graph.each_dsl_file do |file_path, events|
    events.each do |event|
      case event.method_name
      when "describe", "context"
        # Would create example group class
        context_stack.push("RSpec::ExampleGroups::#{event.id}")
      when "let", "let!", "subject"
        # Would add method to current context
        _method_name = event.arguments.first || "subject"
        _owner = context_stack.last
        # graph.add_method(owner:, name: method_name, file_path:, line:, column:)
      end
    end
    context_stack.clear
  end

  graph.resolve
end

# Print summary
puts "\n" + "=" * 40
puts "SUMMARY: Overhead vs Baseline"
puts "=" * 40
baseline = results[:baseline]
results.each do |key, time|
  next if key == :baseline

  overhead = ((time - baseline) / baseline * 100).round(2)
  overhead_str = overhead > 0 ? "+#{overhead}%" : "#{overhead}%"
  puts "  #{key}: #{overhead_str} (#{(time * 1000).round(2)}ms)"
end

puts "\nBaseline: #{(baseline * 1000).round(2)}ms"
