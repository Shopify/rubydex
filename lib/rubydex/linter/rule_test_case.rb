# frozen_string_literal: true

require "fileutils"
require "minitest/test"
require "pathname"
require "tmpdir"
require "uri"
require "rubydex/linter"

module Rubydex
  module Linter
    # Base test case for Rubydex linter rules.
    #
    # Add expected diagnostics below the source:
    #
    #     assert_diagnostics(<<~RUBY)
    #       FOO = 123
    #       ^^^ Failure: FOO
    #     RUBY
    #
    # The carets mark the diagnostic range. Use `^{}` for a zero-width range.
    # Pass a hash to test multiple files. Repeat an annotation line for each message line.
    # By default, `FooTest` tests the `Foo` rule.
    class RuleTestCase < Minitest::Test
      DEFAULT_FILE = "test.rb" #: String

      ANNOTATION_PATTERN = /\A(?<indent>\s*)(?<carets>(?<!\\)\^+|\^\{\})(?:\s(?<message>.*))?\z/ #: Regexp

      #: type annotation = [Integer, Integer, Integer, String]

      #: String
      attr_reader :workspace_path

      #: (String) -> void
      def initialize(name)
        super
        @workspace_path = File.realpath(Dir.mktmpdir("rubydex-rule-test-")) #: String
      end

      #: -> void
      def teardown
        super
      ensure
        FileUtils.remove_entry(workspace_path)
      end

      #: -> singleton(Rule)
      def rule_class
        @rule_class ||= begin
          name = self.class.to_s.delete_suffix("Test")
          if name == self.class.to_s
            raise "Could not infer rule class from #{self.class}; override `#rule_class`"
          end

          Object.const_get(name) #: as singleton(Rule)
        end #: singleton(Rule)?
      end

      # Registers sources that are loaded into the graph for every assertion.
      # Shared sources must not contain caret annotations. Test-specific
      # sources win on filename collision.
      #: (Hash[String, String]) -> void
      def add_shared_source(sources)
        shared_sources.merge!(sources)
      end

      # Returns absolute file paths whose diagnostics should be ignored.
      #: -> Array[String]
      def ignored_diagnostic_files
        @ignored_diagnostic_files ||= [] #: Array[String]?
      end

      #: -> LinterConfig
      def rule_config
        @rule_config ||= LinterConfig.new({}) #: LinterConfig?
      end

      # Runs the rule and compares every diagnostic location with the inline
      # annotations in the corresponding source.
      #: (*(String | Hash[String | Symbol, String])) ?{ (Graph) -> Rule } -> Array[Diagnostic]
      def assert_diagnostics(*args, &rule_builder)
        sources = validated_shared_source.merge(normalize_sources(args))
        clean_per_file, expected_per_file = write_sources(sources)

        diagnostics = run_rule(&rule_builder)
        actual_per_file = annotations_for_diagnostics(diagnostics, clean_per_file)

        surprise_files = actual_per_file.keys - clean_per_file.keys - ignored_diagnostic_files
        refute_predicate(
          surprise_files,
          :any?,
          "Diagnostics in unexpected files: #{surprise_files.inspect}",
        )

        clean_per_file.each do |filename, clean|
          expected = render_annotated(clean, expected_per_file[filename] || [])
          actual = render_annotated(clean, actual_per_file[filename] || [])
          assert_equal(expected, actual, "Mismatch in #{filename}")
        end

        diagnostics
      end

      # Runs the rule and asserts no diagnostics are reported in the provided
      # sources. Sources must not contain caret annotations.
      #: (*(String | Hash[String | Symbol, String])) ?{ (Graph) -> Rule } -> Array[Diagnostic]
      def assert_no_diagnostics(*args, &rule_builder)
        sources = validated_shared_source.merge(normalize_sources(args))
        write_sources(sources)

        diagnostics = run_rule(&rule_builder)
        ignored = ignored_diagnostic_files
        reported = diagnostics.reject { |diagnostic| ignored.include?(diagnostic.location.to_file_path) }
        assert_empty(reported, "Expected no diagnostics, got #{reported.map(&:message).inspect}")
        diagnostics
      end

      #: (
      #|   String,
      #|   *(String | Hash[String | Symbol, String]),
      #|   ?after_excluding: Array[String],
      #| ) ?{ (Graph) -> Rule } -> void
      def assert_handles_missing_required_dependency(dependency, *args, after_excluding: [], &rule_builder)
        sources = validated_shared_source.dup #: Hash[String, String]
        after_excluding.each { |filename| sources.delete(filename) }
        sources.merge!(normalize_sources(args))
        write_sources(sources)

        error = assert_raises(MissingGraphDependencyError) do
          run_rule(&rule_builder)
        end

        expected_message = MissingGraphDependencyError.new(rule_class.rule_name, dependency).message
        assert_equal(expected_message, error.message)
      end

      private

      #: (Hash[String, String]) -> [Hash[String, String], Hash[String, Array[annotation]]]
      def write_sources(sources)
        clean_per_file = {} #: Hash[String, String]
        expected_per_file = {} #: Hash[String, Array[annotation]]

        virtual_sources.clear

        sources.each do |filename, annotated|
          clean, annotations = parse_annotations(annotated)
          if Pathname(filename).absolute?
            absolute_path = filename
            virtual_sources[absolute_path] = clean
          else
            absolute_path = File.join(workspace_path, filename)
            FileUtils.mkdir_p(File.dirname(absolute_path))
            File.write(absolute_path, clean)
            absolute_path = File.realpath(absolute_path)
          end
          clean_per_file[absolute_path] = clean
          expected_per_file[absolute_path] = annotations
        end

        [clean_per_file, expected_per_file]
      end

      #: -> Hash[String, String]
      def shared_sources
        @shared_sources ||= {} #: Hash[String, String]?
      end

      #: -> Hash[String, String]
      def virtual_sources
        @virtual_sources ||= {} #: Hash[String, String]?
      end

      #: -> Hash[String, String]
      def validated_shared_source
        shared_sources.each do |filename, source|
          _, annotations = parse_annotations(source)
          raise "Shared source #{filename} must not contain caret annotations" unless annotations.empty?
        end
        shared_sources
      end

      #: (Array[String | Hash[String | Symbol, String]]) -> Hash[String, String]
      def normalize_sources(args)
        case args
        in []
          {}
        in [String => source]
          { DEFAULT_FILE => source }
        in [Hash => hash]
          hash.transform_keys(&:to_s)
        else
          raise ArgumentError, "expected a String or a { filename => source } Hash, got: #{args.inspect}"
        end
      end

      #: (String) -> [String, Array[annotation]]
      def parse_annotations(annotated_source)
        clean = [] #: Array[String]
        annotations = [] #: Array[annotation]

        annotated_source.each_line do |line|
          if (match = ANNOTATION_PATTERN.match(line.chomp))
            indent = match[:indent]&.length #: as !nil
            carets = match[:carets] #: as !nil
            start_column = indent
            end_column = if carets == "^{}"
              indent
            else
              indent + carets.length
            end

            target_line = clean.empty? ? 0 : clean.size - 1
            message = match[:message].to_s
            last = annotations.last
            if last && last[0] == target_line && last[1] == start_column && last[2] == end_column
              last[3] = "#{last[3]}\n#{message}"
            else
              annotations << [target_line, start_column, end_column, message]
            end
          else
            clean << line
          end
        end

        [clean.join, annotations]
      end

      #: ?{ (Graph) -> Rule } -> Array[Diagnostic]
      def run_rule(&rule_builder)
        graph = Graph.configure_for_workspace(workspace_path)
        file_paths = workspace_file_paths
        graph.index_all(file_paths.reject { |path| File.extname(path) == ".rake" })
        index_rake_files(graph, file_paths)

        virtual_sources.each do |path, source|
          graph.index_source(uri_for_path(path), source, source_id_for(path))
        end
        graph.resolve

        rule = rule_builder ? rule_builder.call(graph) : rule_class.new(graph, config: rule_config)
        rule.lint
        rule.diagnostics
      end

      #: (Graph, Array[String]) -> void
      def index_rake_files(graph, file_paths)
        file_paths.each do |path|
          next unless File.extname(path) == ".rake"

          graph.index_source(uri_for_path(path), File.read(path), "ruby")
        end
      end

      #: -> Array[String]
      def workspace_file_paths
        Dir.glob("**/*", base: workspace_path).sort.filter_map do |relative_path|
          absolute_path = File.join(workspace_path, relative_path)
          next unless File.file?(absolute_path)

          case File.extname(relative_path)
          when ".rb", ".rbi", ".rake", ".rbs"
            File.realpath(absolute_path)
          else
            raise "Unsupported file type: #{relative_path}"
          end
        end
      end

      #: (String) -> String
      def source_id_for(path)
        case File.extname(path)
        when ".rb", ".rbi", ".rake", ".ru"
          "ruby"
        when ".rbs"
          "rbs"
        else
          raise "Unsupported file type: #{path}"
        end
      end

      #: (Array[Diagnostic], Hash[String, String]) -> Hash[String, Array[annotation]]
      def annotations_for_diagnostics(diagnostics, sources)
        result = Hash.new { |hash, key| hash[key] = [] } #: Hash[String, Array[annotation]]

        diagnostics.each do |diagnostic|
          located_messages = [[diagnostic.location, diagnostic.message]] #: Array[[Location, String]]
          diagnostic.related_information.each do |related|
            located_messages << [related.location, related.message]
          end

          located_messages.each do |location, message|
            path = location.to_file_path
            line_index = location.start_line
            start_column = location.start_column
            end_column =
              if location.end_line == location.start_line
                location.end_column
              else
                clean = sources[path]
                line_text = clean ? (clean.each_line.to_a[line_index] || "").chomp : ""
                line_text.length
              end

            annotations = result[path] #: as !nil
            annotations << [line_index, start_column, end_column, message.chomp]
          end
        end

        result
      end

      #: (String, Array[annotation]) -> String
      def render_annotated(clean_source, annotations)
        lines = clean_source.each_line.to_a
        sorted = annotations.sort_by do |line_index, start_column, end_column, message|
          [line_index, start_column, end_column, message]
        end
        output = lines.dup

        sorted.reverse_each do |line_index, start_column, end_column, message|
          caret_count = end_column - start_column
          indentation = " " * start_column
          carets = caret_count.zero? ? "^{}" : "^" * caret_count
          markers = if message.empty?
            ["#{indentation}#{carets}\n"]
          else
            message.split("\n", -1).map { |line| "#{indentation}#{carets} #{line}\n" }
          end
          output[line_index + 1, 0] = markers
        end

        output.join
      end

      #: (String) -> String
      def uri_for_path(path)
        uri_path = Gem.win_platform? ? "/#{path}" : path
        URI::File.build(path: uri_path).to_s
      end
    end
  end
end
