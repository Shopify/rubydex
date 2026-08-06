# frozen_string_literal: true

require "fileutils"
require "tmpdir"
require "rubydex/linter/rule_test_case"

module Rubydex
  module Linter
    # Rule test case that writes relative fixtures to a temporary workspace before indexing them.
    class FileRuleTestCase < RuleTestCase
      #: String
      attr_reader :workspace_path

      #: -> void
      def setup
        super
        @workspace_path = File.realpath(Dir.mktmpdir("rubydex-rule-test-"))
      end

      #: -> void
      def teardown
        super
      ensure
        FileUtils.remove_entry(workspace_path)
      end

      # Registers sources that are included in every assertion in the current test.
      #: (Hash[String, String]) -> void
      def add_shared_source(sources)
        shared_sources.merge!(sources)
      end

      # Absolute or workspace-relative paths whose diagnostics should not fail assertions.
      #: -> Array[String]
      def ignored_diagnostic_files
        @ignored_diagnostic_files ||= [] #: Array[String]?
      end

      #: (
      #|   *(String | Hash[String | Symbol, String]),
      #|   ?config: LinterConfig,
      #| ) ?{ (Graph) -> Rule } -> void
      def assert_diagnostics(*args, config: default_rule_config, &rule_builder)
        with_rule_builder(rule_builder) { super(*args, config:) }
      end

      #: (
      #|   *(String | Hash[String | Symbol, String]),
      #|   ?config: LinterConfig,
      #| ) ?{ (Graph) -> Rule } -> Array[Diagnostic]
      def assert_no_diagnostics(*args, config: default_rule_config, &rule_builder)
        with_rule_builder(rule_builder) { super(*args, config:) }
      end

      #: (
      #|   String,
      #|   *(String | Hash[String | Symbol, String]),
      #|   ?after_excluding: Array[String],
      #|   ?config: LinterConfig,
      #| ) ?{ (Graph) -> Rule } -> void
      def assert_handles_missing_required_dependency(
        dependency,
        *args,
        after_excluding: [],
        config: default_rule_config,
        &rule_builder
      )
        with_rule_builder(rule_builder) do
          super(dependency, *args, after_excluding:, config:)
        end
      end

      # Includes path-based suppressions in the URI-based assertion machinery inherited from RuleTestCase.
      #: -> Array[String]
      def ignored_diagnostic_uris
        (super + ignored_diagnostic_files.map { |path| normalize_uri(path) }).uniq
      end

      private

      #: (Array[String | Hash[String | Symbol, String]]) -> Hash[String, document]
      def documents_with_inline(args)
        merged = documents.dup
        sources = validated_shared_sources.merge(normalize_sources(args))

        sources.each do |path, source|
          clean, annotations = parse_annotations(source)
          merged[normalize_uri(path)] = [clean, "ruby", annotations]
        end

        merged
      end

      #: (String) -> String
      def uri_for_path(path)
        absolute_path = File.expand_path(path, workspace_path)
        absolute_path.prepend("/") if Gem.win_platform?
        URI::File.build(path: absolute_path).to_s
      end

      #: (Hash[String, document], config: LinterConfig) -> Array[Diagnostic]
      def run_rule(documents, config:)
        indexed_files = [] #: Array[String]
        virtual_documents = [] #: Array[[String, String, String]]

        documents.each do |uri, (source, language_id, _annotations)|
          path = file_path_for_uri(uri)
          if path && path_in_workspace?(path)
            FileUtils.mkdir_p(File.dirname(path))
            File.write(path, source)
            indexed_files << path if Graph::INDEXABLE_EXTENSIONS.include?(File.extname(path))
          else
            virtual_documents << [uri, source, language_id]
          end
        end

        graph = Graph.configure_for_workspace(workspace_path)
        graph.index_all(indexed_files)
        virtual_documents.each do |uri, source, language_id|
          graph.index_source(uri, source, language_id)
        end
        graph.resolve

        rule = @rule_builder ? @rule_builder.call(graph) : rule_class.new(graph, config:)
        rule.lint
        rule.diagnostics
      end

      #: -> Hash[String, String]
      def shared_sources
        @shared_sources ||= {} #: Hash[String, String]?
      end

      #: -> Hash[String, String]
      def validated_shared_sources
        shared_sources.each do |path, source|
          _, annotations = parse_annotations(source)
          raise "Shared source #{path} must not contain caret annotations" unless annotations.empty?
        end

        shared_sources
      end

      #: (String) -> String?
      def file_path_for_uri(uri)
        return unless URI(uri).scheme == "file"

        location = Location.new(uri:, start_line: 0, end_line: 0, start_column: 0, end_column: 0)
        URI::RFC2396_PARSER.unescape(location.to_file_path)
      end

      #: (String) -> bool
      def path_in_workspace?(path)
        path == workspace_path || path.start_with?("#{workspace_path}#{File::SEPARATOR}")
      end

      #: [R] (^(Graph) -> Rule)? { -> R } -> R
      def with_rule_builder(rule_builder)
        @rule_builder = rule_builder
        yield
      ensure
        @rule_builder = nil
      end
    end
  end
end
