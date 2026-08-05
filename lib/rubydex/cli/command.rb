# frozen_string_literal: true

require "optparse"

require "rubydex/progress"

module Rubydex
  module CLI
    # Base class for `rdx` subcommands. A subcommand parses its own options out of `argv` and, when
    # it needs one, builds the workspace graph through {#build_graph}.
    #
    # Subclasses live one per file in `lib/rubydex/cli/command/` and describe themselves with a
    # small DSL, which supplies both the name they are dispatched under and their entry in
    # `rdx help`:
    #
    #     class Query < Command
    #       command "query"
    #       arguments "<CYPHER>"
    #       summary "Run a Cypher query against the workspace graph and print the result."
    #     end
    #
    # There is no registration list to keep in step: {.all} loads every file in that directory and
    # discovers the commands through `Class#subclasses`.
    class Command
      class << self
        # The subcommand name declared by {.command}. `nil` for a class that declares none, which is
        # how an abstract intermediate class opts out of being dispatched.
        #: String?
        attr_reader :command_name

        # Declares the subcommand name this class is dispatched under.
        #
        # Deliberately not called `name`: that would shadow `Class#name`, which Ruby uses when it
        # builds error messages, so a `NoMethodError` on a command would report "an instance of
        # query" instead of naming the class.
        #: (String value) -> void
        def command(value)
          # Deliberately checked against {.declared} rather than {.find}: this runs while a command
          # file is still being loaded, and `find` would glob in every other command file first.
          clash = declared.find { |other| !other.equal?(self) && other.command_name == value }
          raise ArgumentError, "command `#{value}` is already declared by #{clash}" if clash

          @command_name = value #: String?
        end

        # Declares the argument spec shown after the command name in `rdx help`, e.g. `"<CYPHER>"`.
        # Called without an argument, returns the declared spec.
        #: (?String? spec) -> String?
        def arguments(spec = nil)
          return @arguments unless spec

          @arguments = spec #: String?
        end

        # Declares the description shown in `rdx help`. May span several lines, which are aligned
        # under the first when the command list is rendered. Called without an argument, returns the
        # declared description.
        #: (?String? text) -> String?
        def summary(text = nil)
          return @summary unless text

          @summary = text.strip #: String?
        end

        # How the command is spelled in a usage line, e.g. `"query <CYPHER>"`.
        #: -> String
        def usage_form
          [command_name, arguments].compact.join(" ")
        end

        # The subcommands that are currently loaded, in alphabetical order. Loads nothing itself, so
        # it is safe to call while a command file is still being evaluated.
        #: -> Array[singleton(Command)]
        def declared
          Command.subclasses.select(&:command_name).sort_by(&:command_name)
        end

        # Every subcommand, in alphabetical order. Loads the command files first, so the answer does
        # not depend on what happens to have been required already.
        #: -> Array[singleton(Command)]
        def all
          Dir.glob(File.expand_path("command/*.rb", __dir__)).sort.each { |file| require(file) }

          declared
        end

        # The subcommand declared under `name`, if any.
        #: (String? name) -> singleton(Command)?
        def find(name)
          all.find { |command| command.command_name == name }
        end
      end

      #: (Array[String] argv) -> void
      def initialize(argv)
        @argv = argv
      end

      #: -> void
      def run
        raise NotImplementedError, "#{self.class} must implement #run"
      end

      private

      #: Array[String]
      attr_reader :argv

      #: (String message) -> void
      def abort_with_usage(message)
        CLI.abort_with_usage(message)
      end

      # A command with subactions passes its own `banner`. Every other command builds one from its
      # declaration.
      #
      # A bad option reports the message and the usage text, so every subcommand rejects bad input
      # the same way.
      #: (?options: bool, ?banner: String?) ?{ (OptionParser parser) -> void } -> void
      def parse_options!(options: false, banner: nil)
        unless banner
          banner = +"Usage: rdx #{self.class.usage_form}"
          banner << " [options]" if options
        end

        parser = OptionParser.new do |p|
          p.banner = banner
          yield(p) if block_given?
          p.on("-h", "--help", "Show this help") do
            puts(p)
            exit
          end
        end

        parser.parse!(argv)
      rescue OptionParser::ParseError => e
        abort_with_usage(e.message)
      end

      # Builds the workspace graph, sending progress messages to `progress_io`.
      #: (IO progress_io) -> Rubydex::Graph
      def build_graph(progress_io)
        graph = Rubydex::Graph.configure_for_workspace(Dir.pwd)
        Progress.with_timer(progress_io, "Indexing workspace...") { graph.index_workspace }
        Progress.with_timer(progress_io, "Resolving graph...") { graph.resolve }
        graph
      end
    end
  end
end
