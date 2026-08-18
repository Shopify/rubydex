# frozen_string_literal: true

require "rubydex/linter"

# This add-on is only supported by the beta version of the LSP. We don't want to keep showing window dialogs to users of
# the stable version until the v0.27 series is stable, so here we are hand-rolling our own `depend_on_ruby_lsp!` method
# to avoid notifying them.
lsp_version = Gem::Version.new(RubyLsp::VERSION)
return unless [">= 0.27.0.beta4", "< 0.28"].all? { |constraint| Gem::Requirement.new(constraint).satisfied_by?(lsp_version) }

module Rubydex
  module Linter
    module RubyLsp
      class Addon < ::RubyLsp::Addon
        CONFIGURATION_FILE = "rubydex.toml" #: String

        # @override
        #: () -> String
        def name
          "Rubydex Linter"
        end

        # @override
        #: () -> String
        def version
          ::Rubydex::VERSION
        end

        # @override
        #: (::RubyLsp::GlobalState, Thread::Queue) -> void
        def activate(global_state, outgoing_queue)
          @outgoing_queue = outgoing_queue #: Thread::Queue?
          @linter = Linter.new(global_state) #: Linter?

          global_state.register_formatter(
            "rubydex",
            @linter, #: as !nil
          )

          register_additional_file_watchers(global_state, outgoing_queue)
        end

        # @override
        #: () -> void
        def deactivate; end

        #: (Array[{ uri: String, type: Integer }]) -> void
        def workspace_did_change_watched_files(changes)
          return unless @linter && @outgoing_queue

          @linter.reload_configuration if changes.any? { |change| configuration_change?(change) }
          @linter.lint!

          @linter.diagnostics_to_clear.each do |uri|
            @outgoing_queue << ::RubyLsp::Notification.publish_diagnostics(uri, [])
          end

          @linter.current_diagnostics.each do |uri, diagnostics|
            @outgoing_queue << ::RubyLsp::Notification.publish_diagnostics(uri, diagnostics)
          end
        end

        private

        #: (::RubyLsp::GlobalState, Thread::Queue) -> void
        def register_additional_file_watchers(global_state, outgoing_queue)
          return unless global_state.client_capabilities.supports_watching_files

          outgoing_queue << ::RubyLsp::Request.new(
            id: "rubydex-linter-file-watcher",
            method: "client/registerCapability",
            params: ::RubyLsp::Interface::RegistrationParams.new(
              registrations: [
                ::RubyLsp::Interface::Registration.new(
                  id: "workspace/didChangeWatchedFilesRubydexLinter",
                  method: "workspace/didChangeWatchedFiles",
                  register_options: ::RubyLsp::Interface::DidChangeWatchedFilesRegistrationOptions.new(
                    watchers: [
                      ::RubyLsp::Interface::FileSystemWatcher.new(
                        glob_pattern: ::RubyLsp::Interface::RelativePattern.new(
                          base_uri: global_state.workspace_uri.to_s,
                          pattern: CONFIGURATION_FILE,
                        ),
                        kind: ::RubyLsp::Constant::WatchKind::CREATE | ::RubyLsp::Constant::WatchKind::CHANGE,
                      ),
                    ],
                  ),
                ),
              ],
            ),
          )
        end

        # The Ruby LSP forwards every watched file change to every add-on, including the ones registered by other add-ons,
        # so we have to check that the change is actually about our configuration file.
        #: ({ uri: String, type: Integer }) -> bool
        def configuration_change?(change)
          path = URI(change[:uri]).full_path
          return false unless path

          File.basename(path) == CONFIGURATION_FILE
        end
      end

      class Linter
        include ::RubyLsp::Requests::Support::Formatter

        DIAGNOSTIC_SEVERITIES = {
          ::Rubydex::Severity::Error => ::RubyLsp::Constant::DiagnosticSeverity::ERROR,
          ::Rubydex::Severity::Warning => ::RubyLsp::Constant::DiagnosticSeverity::WARNING,
          ::Rubydex::Severity::Information => ::RubyLsp::Constant::DiagnosticSeverity::INFORMATION,
          ::Rubydex::Severity::Hint => ::RubyLsp::Constant::DiagnosticSeverity::HINT,
        }.freeze #: Hash[singleton(::Rubydex::Severity::Base), Integer]

        #: Hash[String, Array[::RubyLsp::Interface::Diagnostic]]
        attr_reader :current_diagnostics

        #: Array[String]
        attr_reader :diagnostics_to_clear

        #: (::RubyLsp::GlobalState) -> void
        def initialize(global_state)
          @graph = global_state.graph #: ::Rubydex::Graph
          @workspace_path = global_state.workspace_path #: String
          ::Rubydex::Linter::RuleLoader.load(@workspace_path)
          @custom_rules = ::Rubydex::Linter::CustomRule.subclasses #: Array[singleton(::Rubydex::Linter::CustomRule)]
          @runner = build_runner #: ::Rubydex::Linter::Runner

          @current_diagnostics = {} #: Hash[String, Array[::RubyLsp::Interface::Diagnostic]]
          @diagnostics_to_clear = [] #: Array[String]
        end

        # @override
        #: (URI::Generic, ::RubyLsp::Document[untyped]) -> Array[::RubyLsp::Interface::Diagnostic]?
        def run_diagnostic(uri, _document)
          @current_diagnostics[uri.to_s]
        end

        # @override
        #: (URI::Generic, ::RubyLsp::RubyDocument) -> String?
        def run_formatting(uri, document); end

        # @override
        #: (URI::Generic, String, Integer) -> String?
        def run_range_formatting(uri, source, base_indentation); end

        #: () -> void
        def lint!
          @diagnostics_to_clear = @current_diagnostics.keys
          @current_diagnostics.clear

          @runner.run.diagnostics.each do |diagnostic|
            uri = diagnostic.location.uri
            (@current_diagnostics[uri] ||= []) << to_lsp_diagnostic(diagnostic)
          end

          @diagnostics_to_clear -= @current_diagnostics.keys
        end

        #: () -> void
        def reload_configuration
          @runner = build_runner
        end

        private

        #: () -> ::Rubydex::Linter::Runner
        def build_runner
          config = ::Rubydex::Config.load(@workspace_path)
          ::Rubydex::Linter::Runner.new(@graph, custom_rules: @custom_rules, config: config.linter)
        end

        #: (::Rubydex::Diagnostic) -> ::RubyLsp::Interface::Diagnostic
        def to_lsp_diagnostic(diagnostic)
          location = diagnostic.location

          ::RubyLsp::Interface::Diagnostic.new(
            message: diagnostic.message,
            source: "Rubydex",
            code: diagnostic.rule,
            severity: DIAGNOSTIC_SEVERITIES.fetch(diagnostic.severity),
            range: lsp_range(location),
            related_information: diagnostic.related_information.map do |information|
              ::RubyLsp::Interface::DiagnosticRelatedInformation.new(
                location: ::RubyLsp::Interface::Location.new(
                  uri: information.location.uri,
                  range: lsp_range(information.location),
                ),
                message: information.message,
              )
            end,
          )
        end

        #: (::Rubydex::Location) -> ::RubyLsp::Interface::Range
        def lsp_range(location)
          ::RubyLsp::Interface::Range.new(
            start: ::RubyLsp::Interface::Position.new(
              line: location.start_line,
              character: location.start_column,
            ),
            end: ::RubyLsp::Interface::Position.new(
              line: location.end_line,
              character: location.end_column,
            ),
          )
        end
      end
    end
  end
end
