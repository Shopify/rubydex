# frozen_string_literal: true

module Rubydex
  # The CLI and the server print the same progress lines, so the measurement is defined here, not
  # in either of them.
  module Progress
    class << self
      # The server passes a `nil` `io` when it has no log.
      #: (IO? io, String message) { -> void } -> void
      def with_timer(io, message)
        unless io
          yield
          return
        end

        io.print(message)
        start = Process.clock_gettime(Process::CLOCK_MONOTONIC, :float_millisecond)
        yield
        duration = Process.clock_gettime(Process::CLOCK_MONOTONIC, :float_millisecond) - start
        io.puts(" finished in #{duration.round(2)}ms")
      end
    end
  end
end
