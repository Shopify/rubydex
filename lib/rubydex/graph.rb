# frozen_string_literal: true

module Rubydex
  # The global graph representing all declarations and their relationships for the workspace
  #
  # Note: this class is partially defined in C to integrate with the Rust backend
  class Graph
    IGNORED_DIRECTORIES = [
      ".bundle",
      ".git",
      ".github",
      "node_modules",
      "tmp",
    ].freeze

    #: String
    attr_accessor :workspace_path

    #: (?workspace_path: String) -> void
    def initialize(workspace_path: Dir.pwd)
      @workspace_path = workspace_path
    end

    # Index all files and dependencies of the workspace that exists in `@workspace_path`
    #: -> Array[String]
    def index_workspace
      index_all(workspace_paths)
    end

    # Returns all workspace paths that should be indexed, excluding directories that we don't need to descend into such
    # as `.git`, `node_modules`. Also includes any top level Ruby files
    #
    #: -> Array[String]
    def workspace_paths
      paths = []

      Dir.each_child(@workspace_path) do |entry|
        full_path = File.join(@workspace_path, entry)

        if File.directory?(full_path)
          paths << full_path unless IGNORED_DIRECTORIES.include?(entry)
        elsif File.extname(entry) == ".rb"
          paths << full_path
        end
      end

      add_workspace_dependency_paths(paths)
      add_core_rbs_definition_paths(paths)
      paths.uniq!
      paths
    end

    private

    # Gathers the paths we have to index for all workspace dependencies
    #: (Array[String]) -> void
    def add_workspace_dependency_paths(paths)
      specs = Bundler.locked_gems&.specs
      return unless specs

      specs.each do |lazy_spec|
        spec = Gem::Specification.find_by_name(lazy_spec.name)
        spec.require_paths.each do |path|
          # For native extensions, RubyGems inserts an absolute require path pointing to
          # `gems/some-gem-1.0.0/extensions`. Those paths don't actually include any Ruby files inside, so we can skip
          # descending them
          next if File.absolute_path?(path)

          paths << File.join(spec.full_gem_path, path)
        end
      rescue Gem::MissingSpecError
        nil
      end
    end

    # Searches for the latest installation of the `rbs` gem and adds the paths for the core and stdlib RBS definitions
    # to the list of paths. This method does not require `rbs` to be a part of the bundle. It searches for whatever
    # latest installation of `rbs` exists in the system and fails silently if we can't find one
    #
    #: (Array[String]) -> void
    def add_core_rbs_definition_paths(paths)
      rbs_gem_path = Gem.path
        .flat_map { |path| Dir.glob(File.join(path, "gems", "rbs-[0-9]*/")) }
        .max_by { |path| Gem::Version.new(File.basename(path).delete_prefix("rbs-")) }

      return unless rbs_gem_path

      paths << File.join(rbs_gem_path, "core")
      paths << File.join(rbs_gem_path, "stdlib")
    end
  end
end
