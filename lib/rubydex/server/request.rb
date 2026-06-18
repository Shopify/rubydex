# frozen_string_literal: true

require "json"

module Rubydex
  module Server
    # Length-prefixed JSON framing for client/server messages (modeled on Spring's `send_json`).
    #
    # The wire format is a single decimal line containing the payload's byte size, followed by
    # exactly that many bytes of JSON. This avoids delimiter escaping and partial-read ambiguity.
    module Request
      class << self
        # Writes a framed JSON payload to `socket`.
        #: (IO socket, Hash[untyped, untyped] payload) -> void
        def write(socket, payload)
          data = JSON.dump(payload)
          socket.puts(data.bytesize)
          socket.write(data)
          socket.flush
        end

        # Reads a framed JSON payload from `socket`. Returns `nil` on EOF / malformed length.
        #: (IO socket) -> Hash[untyped, untyped]?
        def read(socket)
          length_line = socket.gets
          return unless length_line

          length = length_line.to_i
          return if length <= 0

          body = socket.read(length)
          return unless body

          JSON.parse(body)
        end

        # Reads a single line from `socket`, raising `ServerReadTimeout` if nothing arrives within
        # `timeout` seconds. Used for the handshake where a wedged server must never hang the client.
        #: (IO socket, Float timeout) -> String
        def read_line_with_timeout(socket, timeout)
          raise ServerReadTimeout if IO.select([socket], nil, nil, timeout).nil?

          line = socket.gets
          raise ServerReadTimeout unless line

          line
        end
      end
    end
  end
end
