# frozen_string_literal: true

require "rubydex/cli/command"

module Rubydex
  module CLI
    # `rdx console` — opens an IRB session with a populated graph for the current workspace.
    class Command
      class Console < Command
        command "console"
        summary "Open an interactive session with a populated graph for the current workspace"

        #: -> void
        def run
          parse_options!

          start_irb(build_graph($stderr))
        end

        private

        # Starts IRB with `graph` in scope.
        #
        #: (Rubydex::Graph graph) -> void
        def start_irb(graph)
          require_irb

          IRB.setup(nil)
          IRB.conf[:IRB_NAME] = "rubydex"

          # The session uses IRB's own workspace rather than a binding captured here: `IRB::WorkSpace`
          # derives its binding from a copy of `IRB::TOPLEVEL_BINDING`, so `self` is `main` and
          # `class Foo; end` at the prompt defines `::Foo`, exactly as in a stock `irb`. A binding
          # captured inside this class would make `self` the command object instead.
          workspace = IRB::WorkSpace.new
          workspace.local_variable_set(:graph, graph)
          IRB::Irb.new(workspace).run(IRB.conf)
        end

        # `irb` is a development dependency rather than a runtime one, so a missing gem is reported
        # instead of raised. Only a missing `irb` is reported that way: IRB loads `reline`, which
        # needs `fiddle` on Windows, and that failure has to surface as itself rather than as a
        # missing `irb`.
        #: -> void
        def require_irb
          require "irb"
        rescue LoadError => e
          raise unless e.path == "irb"

          abort("Interactive mode requires `irb` to be in the bundle")
        end
      end
    end
  end
end
