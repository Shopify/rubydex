# frozen_string_literal: true

module Rubydex
  # Builds the workspace candidate report shared by the CLI and MCP interfaces. Reference analysis still runs over
  # the whole graph, including dependencies; only the reported definitions are limited to workspace source files.
  module DeadCode
    PATH_FNMATCH_FLAGS = File::FNM_PATHNAME | File::FNM_EXTGLOB | File::FNM_DOTMATCH #: Integer

    #: type source_location = { path: String, line: Integer, column: Integer }
    #: type candidate = { name: String, kind: String, locations: Array[source_location] }

    class << self
      #: (Graph graph, ?path: String?) -> Array[candidate]
      def candidates(graph, path: nil)
        pattern = normalize_path_pattern(path) if path
        excluded_patterns = graph.dead_code_config.exclude_patterns.map { |entry| normalize_path_pattern(entry) }
        workspace = File.join(File.expand_path(graph.workspace_path), "")
        # Bundler may install dependencies under the workspace (for example, in vendor/bundle). Keep both spellings
        # when a gem home or workspace is symlinked, since indexed file URIs can retain a symlink within the workspace.
        dependency_paths = Gem.path.flat_map { |path| [File.expand_path(path), canonical_path(path)] }.uniq
          .map { |path| File.join(path, "") }
          .select { |path| path.start_with?(workspace) }

        graph.dead_code_candidates.filter_map do |declaration|
          name = declaration.name
          next unless name

          locations = declaration.definitions.filter_map do |definition|
            source_location(definition, workspace, dependency_paths)
          end
          locations.select! { |location| File.fnmatch?(pattern, location[:path], PATH_FNMATCH_FLAGS) } if pattern
          locations.reject! do |location|
            excluded_patterns.any? { |excluded| File.fnmatch?(excluded, location[:path], PATH_FNMATCH_FLAGS) }
          end
          next if locations.empty?

          locations.uniq!
          locations.sort_by! { |location| [location[:path], location[:line], location[:column]] }

          {
            name: name,
            kind: declaration.class.name.delete_prefix("Rubydex::"),
            locations: locations,
          }
        end.sort_by { |candidate| candidate[:name] }
      end

      private

      #: (String pattern) -> String
      def normalize_path_pattern(pattern)
        pattern = pattern.delete_prefix("./")
        pattern == "**" || pattern.end_with?("/**") ? "#{pattern}/*" : pattern
      end

      #: (String path) -> String
      def canonical_path(path)
        File.realpath(path)
      rescue Errno::ENOENT, Errno::ENOTDIR, Errno::EACCES
        File.expand_path(path)
      end

      #: (Definition definition, String workspace, Array[String] dependency_paths) -> source_location?
      def source_location(definition, workspace, dependency_paths)
        location = (definition.name_location || definition.location).to_display
        path = File.expand_path(location.to_file_path)
        return unless path.start_with?(workspace)
        return if dependency_paths.any? { |dependency| path.start_with?(dependency) }

        { path: path.delete_prefix(workspace), line: location.start_line, column: location.start_column }
      rescue Location::NotFileUriError
        nil
      end
    end
  end
end
