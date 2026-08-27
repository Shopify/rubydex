# frozen_string_literal: true

module Rubydex
  # The global graph representing all declarations and their relationships for the workspace
  #
  # Note: this class is partially defined in C to integrate with the Rust backend
  class Graph
    INDEXABLE_EXTENSIONS = [".rb", ".rake", ".rbs", ".ru"].freeze

    class << self
      # Creates a new graph with the loaded configuration. For use cases where the graph must be shared between
      # different tools, do not use this. Create and own a `Config` object instead.
      #
      #: (String) -> instance
      def configure_for_workspace(workspace_path)
        graph = new
        graph.load_config(Config.load(workspace_path))
        graph
      end

      # Returns the paths for the core and standard library RBS definitions of the latest installation of the `rbs` gem,
      # which are the definitions for Ruby itself. Tools that build their own list of paths and index it with
      # `index_all` must append these paths, otherwise the graph will have no definitions for core classes like `Object`
      # or `Kernel`.
      #
      # This method does not require `rbs` to be a part of the bundle. It searches for whatever latest installation of
      # `rbs` exists in the system and returns an empty array if we can't find one
      #
      #: -> Array[String]
      def core_rbs_definition_paths
        rbs_gem_path = Gem.path
          .flat_map { |path| Dir.glob(File.join(path, "gems", "rbs-[0-9]*/")) }
          .max_by { |path| Gem::Version.new(File.basename(path).delete_prefix("rbs-")) }

        return [] unless rbs_gem_path

        [File.join(rbs_gem_path, "core"), File.join(rbs_gem_path, "stdlib")]
      end
    end

    # Index all files and dependencies of the workspace that exists in `workspace_path`
    #: -> Array[String]
    def index_workspace
      index_all(workspace_paths)
    end

    # Returns all workspace paths that should be indexed
    #
    #: -> Array[String]
    def workspace_paths
      paths = []
      root = workspace_path

      Dir.each_child(root) do |entry|
        full_path = File.join(root, entry)

        if File.directory?(full_path) || INDEXABLE_EXTENSIONS.include?(File.extname(entry))
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

    # Adds the paths for the core and stdlib RBS definitions to the list of paths. Fails silently if no installation of
    # the `rbs` gem can be found. See `Graph.core_rbs_definition_paths`
    #
    #: (Array[String]) -> void
    def add_core_rbs_definition_paths(paths)
      paths.concat(Graph.core_rbs_definition_paths)
    end
  end
end
