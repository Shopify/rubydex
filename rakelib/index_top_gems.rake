# frozen_string_literal: true

desc "Index the top 100 gems from rubygems.org"
task index_top_gems: :compile_release do
  $LOAD_PATH.unshift(File.expand_path("../lib", __dir__))
  require "net/http"
  require "rubygems/package"
  require "fileutils"
  require "rubydex"
  require "yaml"
  require "json"

  class GemIndexer
    attr_reader :errors

    def initialize(gems, gems_dir)
      @gems = gems
      @gems_dir = gems_dir
      @errors = []
      @mutex = Mutex.new
    end

    def run
      workers = (0...4).map do
        Thread.new do
          until @gems.empty?
            gem = @gems.shift

            # Discover latest version of the gem
            uri = URI.parse("https://rubygems.org/api/v1/versions/#{gem}/latest.json")
            response = Net::HTTP.get_response(uri)
            raise gem unless response.is_a?(Net::HTTPSuccess)

            latest_version = JSON.parse(response.body)["version"]
            puts "Indexing #{gem}-#{latest_version}"

            # Download the gem's compressed file
            uri = URI.parse("https://rubygems.org/gems/#{gem}-#{latest_version}.gem")
            response = Net::HTTP.get_response(uri)
            raise gem unless response.is_a?(Net::HTTPSuccess)

            # Create the directory to extract the gem's files into
            gem_dir = File.join(@gems_dir, gem)
            FileUtils.mkdir(gem_dir)

            # Extract the gem's files and delete the compressed file
            filepath = File.join(@gems_dir, "#{gem}.gem")
            File.write(filepath, response.body)
            Gem::Package.new(filepath).extract_files(gem_dir, "**/*.rb")
            File.delete(filepath)

            # Index the gem's files and yield errors back to the main Ractor
            graph = Rubydex::Graph.new
            error = graph.index_all(Dir.glob("#{gem_dir}/**/*.rb"))
            next unless error

            @mutex.synchronize { @errors << "#{gem} => #{error}" }
          end
        end
      end

      workers.each(&:join)
    end
  end

  gems = YAML.safe_load_file(File.expand_path("top_gems.yml", __dir__))
  gems_dir = File.expand_path(File.join("..", "tmp", "top_gems"), __dir__)
  FileUtils.mkdir_p(gems_dir)

  begin
    indexer = GemIndexer.new(gems, gems_dir)
    indexer.run
  rescue => e
    abort("Failed to index top gems: #{e.full_message}")
  ensure
    FileUtils.rm_rf(gems_dir)
  end

  if indexer.errors.any?
    abort <<~MESSAGE
      Errors while indexing top gems

      #{indexer.errors.join("\n")}
    MESSAGE
  end

  puts "Indexed top gems successfully"
end
