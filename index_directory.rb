require "index"

class IndexDirectory
  class << self
    def run(path)
      Dir.chdir(path) do
        Index::Repository.index_all(Dir["**/*.rb"])
      end
    end
  end
end

IndexDirectory.run(ARGV[0])
