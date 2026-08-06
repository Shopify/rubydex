# frozen_string_literal: true

module Rubydex
  module Linter
    # Base class for semantic lint rules.
    # @abstract
    class Rule
      class << self
        #: () -> String
        def rule_name
          name #: as !nil
            .split("::").last #: as !nil
        end
      end

      #: Graph
      attr_reader :graph

      #: LinterConfig
      attr_reader :config

      #: Array[Diagnostic]
      attr_reader :diagnostics

      #: (Graph, config: LinterConfig) -> void
      def initialize(graph, config:)
        @graph = graph
        @config = config
        @diagnostics = [] #: Array[Diagnostic]
      end

      # @abstract
      #: () -> singleton(Severity::Base)
      def severity
        raise NotImplementedError, "Subclasses must implement the severity method"
      end

      #: () -> singleton(Severity::Base)
      def verified_severity
        config.severity_for(self.class, default: severity)
      end

      # @abstract
      #: () -> void
      def lint
        raise NotImplementedError, "Subclasses must implement the lint method"
      end

      # Anchors a diagnostic on a definition's name token, falling back to its full range when no name location exists.
      #: (Definition) -> Location
      def diagnostic_location(definition)
        definition.name_location || definition.location
      end

      #: (String) -> Location
      def file_location(uri)
        Location.new(uri: uri, start_line: 0, end_line: 0, start_column: 0, end_column: 0)
      end

      #: (String) -> String
      def path_for_uri(uri)
        parsed_uri = URI.parse(uri)
        if parsed_uri.scheme == "file"
          path = parsed_uri.path #: as !nil
          path.delete_prefix!("/") if Gem.win_platform?
          return path
        end

        uri
      end

      # Returns every class inheriting from +base_name+, excluding the base class itself.
      #: (String) -> Enumerable[Rubydex::Class]
      def child_classes(base_name)
        required_namespace(base_name).descendants.lazy.filter_map do |child_declaration|
          next unless child_declaration.is_a?(Rubydex::Class)
          next if child_declaration.name == base_name

          child_declaration
        end
      end

      #: (String) -> Namespace
      def required_namespace(name)
        declaration = graph[name]
        return declaration if declaration.is_a?(Namespace)

        raise MissingGraphDependencyError.new(rule_name, "`#{name}`")
      end

      #: (Namespace, String) -> Rubydex::Method
      def required_method(namespace, method_name)
        method = namespace.find_member(method_name)
        return method if method.is_a?(Rubydex::Method)

        raise MissingGraphDependencyError.new(rule_name, "`#{namespace.name}##{method_name}`")
      end

      #: () -> String
      def rule_name
        self.class.rule_name
      end

      protected

      #: (
      #|   String,
      #|   Location,
      #|   ?related_information: Array[RelatedInformation],
      #| ) -> void
      def add_diagnostic(message, location, related_information: [])
        @diagnostics << Diagnostic.new(
          rule: self.class.rule_name,
          message: message,
          location: location,
          severity: verified_severity,
          related_information: related_information,
        )
      end

      private

      #: (Location) -> String?
      def source_for_location(location)
        File.read(location.to_file_path)
      rescue Errno::ENOENT
        nil
      end
    end

    class MissingGraphDependencyError < StandardError
      #: (String, String) -> void
      def initialize(rule_name, dependency)
        super(
          "Rubydex linter rule `#{rule_name}` requires #{dependency} to exist in the Rubydex graph. " \
            "This is a rule setup error, not a clean lint result; ensure the source that defines " \
            "the dependency is indexed and Rubydex can resolve it.",
        )
      end
    end
  end
end
