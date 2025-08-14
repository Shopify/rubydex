# frozen_string_literal: true

module Index
  # The global graph representing all declarations and their relationships for the workspace
  #
  # Note: this class is partially defined in C to integrate with the Rust backend
  class Graph
    def initialize(workspace_path: Dir.pwd)
      @workspace_path = workspace_path
    end

    # Index all files and dependencies of the workspace that exists in `@workspace_path`
    def index_workspace
      paths = workspace_dependency_paths
      paths.unshift(@workspace_path)
      index_all(paths)
    end

    private

    # Gathers the paths we have to index for all workspace dependencies
    def workspace_dependency_paths
      specs = Bundler.locked_gems&.specs
      return [] unless specs

      paths = specs.filter_map do |lazy_spec|
        spec = Gem::Specification.find_by_name(lazy_spec.name)
        spec.require_paths.map { |path| File.join(spec.full_gem_path, path) }
      rescue Gem::MissingSpecError
        nil
      end

      paths.flatten!
      paths
    end
  end
end
