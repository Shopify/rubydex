# frozen_string_literal: true

$LOAD_PATH.unshift(File.expand_path("lib", __dir__))

require "benchmark"
require "index"

all_core_files = Dir.glob("#{Dir.home}/world/trees/root/src/areas/core/shopify/**/*.rb")
repo = Index::Repository.new

rt = Benchmark.realtime do
  repo.index_all(all_core_files)
end

puts "Took #{rt} to index #{repo.size} declarations"
