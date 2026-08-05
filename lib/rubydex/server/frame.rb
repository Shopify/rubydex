# frozen_string_literal: true

require "json"

module Rubydex
  module Server
    # Length-prefixed JSON frames: one decimal line with the payload size, then that many bytes.
    #
    # The two directions carry opposite risks, so each has its own reader. `read_request` bounds the
    # whole frame, because a slow client must not hold the server's accept loop. `read_response`
    # bounds only the gap between chunks, because the server sends nothing until the query completes.
    module Frame
      class ReadTimeout < Error; end

      class Malformed < Error; end

      REQUEST_TIMEOUT = 10.0 #: Float

      IDLE_TIMEOUT = 10.0 #: Float

      # The length line and the version line are both short.
      MAX_LINE_BYTES = 1024 #: Integer

      MAX_REQUEST_BYTES = 1024 * 1024 #: Integer

      # A result is much larger than the query that asks for it, so the two caps differ.
      MAX_RESPONSE_BYTES = 512 * 1024 * 1024 #: Integer

      # How many bytes one read asks for. `IO#read_nonblock` allocates `maxlen` before it reads, so
      # a peer that declares 512 MiB and sends one byte would cost 512 MiB in one allocation.
      CHUNK_BYTES = 64 * 1024 #: Integer

      # A length line holds decimal digits and an optional newline. `String#to_i` would accept
      # `"100garbage"` and return `100`.
      LENGTH_LINE = /\A\d+\n?\z/ #: Regexp

      class << self
        #: (IO socket, Hash[untyped, untyped] payload) -> void
        def write(socket, payload)
          data = JSON.dump(payload)
          socket.puts(data.bytesize)
          socket.write(data)
          socket.flush
        end

        # `timeout` bounds the whole frame, not the gap between chunks: the server answers one
        # client at a time. Returns `nil` when the client closed before it sent a byte.
        #: (IO socket, ?timeout: Float) -> Hash[untyped, untyped]?
        def read_request(socket, timeout: REQUEST_TIMEOUT)
          read_frame(
            socket,
            max_bytes: MAX_REQUEST_BYTES,
            total_timeout: timeout,
            idle_timeout: nil,
            first_wait: nil,
          )
        end

        # The client waits for the first byte with no limit, because the query runs before it.
        # `idle_timeout` then bounds each gap, which frees the client if the server dies mid-answer.
        # A control command needs no work, so its caller passes `total_timeout` instead.
        # Returns `nil` when the server closed before it sent a byte.
        #: (IO socket, ?idle_timeout: Float, ?total_timeout: Float?) -> Hash[untyped, untyped]?
        def read_response(socket, idle_timeout: IDLE_TIMEOUT, total_timeout: nil)
          read_frame(
            socket,
            max_bytes: MAX_RESPONSE_BYTES,
            total_timeout: total_timeout,
            idle_timeout: idle_timeout,
            first_wait: nil,
          )
        end

        # Reads one line for the version handshake. `timeout` bounds the whole line.
        #: (IO socket, ?timeout: Float) -> String
        def read_line(socket, timeout: REQUEST_TIMEOUT)
          deadline = Process.clock_gettime(Process::CLOCK_MONOTONIC) + timeout
          line = read_line_until(socket, deadline: deadline, idle_timeout: nil, first_wait: nil)
          raise ReadTimeout, "the peer sent no line" unless line

          line
        end

        private

        # Every failure becomes `Malformed` or `ReadTimeout`. A `JSON::ParserError` never leaves
        # this module.
        #: (IO socket, max_bytes: Integer, total_timeout: Float?, idle_timeout: Float?, first_wait: Float?) -> Hash[untyped, untyped]?
        def read_frame(socket, max_bytes:, total_timeout:, idle_timeout:, first_wait:)
          deadline = total_timeout && (Process.clock_gettime(Process::CLOCK_MONOTONIC) + total_timeout)

          line = read_line_until(socket, deadline: deadline, idle_timeout: idle_timeout, first_wait: first_wait)
          return unless line

          length = parse_length(line, max_bytes)
          body = read_bytes(socket, length, deadline: deadline, idle_timeout: idle_timeout, first_wait: idle_timeout)
          raise Malformed, "the peer closed the connection inside a frame" unless body

          payload = parse_body(body)
          raise Malformed, "expected a JSON object, got #{payload.class}" unless payload.is_a?(Hash)

          payload
        end

        # Returns `nil` at a clean end of file, so a caller can separate a close from a broken frame.
        #: (IO socket, deadline: Float?, idle_timeout: Float?, first_wait: Float?) -> String?
        def read_line_until(socket, deadline:, idle_timeout:, first_wait:)
          buffer = +""
          wait = first_wait

          loop do
            byte = read_bytes(socket, 1, deadline: deadline, idle_timeout: idle_timeout, first_wait: wait)
            return if byte.nil? && buffer.empty?
            raise ReadTimeout, "the peer sent a partial line" unless byte

            buffer << byte
            wait = idle_timeout
            return buffer if byte == "\n"
            raise Malformed, "a line exceeded #{MAX_LINE_BYTES} bytes" if buffer.bytesize >= MAX_LINE_BYTES
          end
        end

        # Each chunk resets the idle budget, so a slow but steady peer succeeds.
        #
        # A close after a partial read is a broken frame, never a short payload: otherwise a peer
        # could declare 100 bytes, send `{}`, close, and the caller would accept that valid JSON.
        #: (IO socket, Integer length, deadline: Float?, idle_timeout: Float?, first_wait: Float?) -> String?
        def read_bytes(socket, length, deadline:, idle_timeout:, first_wait:)
          buffer = +""
          idle = first_wait

          while buffer.bytesize < length
            wait = wait_budget(deadline, idle)
            expired(deadline, idle) if IO.select([socket], nil, nil, wait).nil?

            want = length - buffer.bytesize
            chunk = socket.read_nonblock([want, CHUNK_BYTES].min, exception: false)

            if chunk.nil?
              return if buffer.empty?

              raise Malformed, "the peer closed the connection after #{buffer.bytesize} of #{length} bytes"
            end

            next if chunk == :wait_readable

            buffer << chunk
            idle = idle_timeout
          end

          buffer
        end

        # How long one wait lasts. `nil` blocks until the peer sends a byte.
        #: (Float? deadline, Float? idle) -> Float?
        def wait_budget(deadline, idle)
          return idle unless deadline

          remaining = deadline - Process.clock_gettime(Process::CLOCK_MONOTONIC)
          expired(deadline, idle) if remaining <= 0

          idle ? [remaining, idle].min : remaining
        end

        # A peer that sends one byte at a time never idles, so it gets the deadline message.
        #: (Float? deadline, Float? idle) -> bot
        def expired(deadline, idle)
          if deadline && (deadline - Process.clock_gettime(Process::CLOCK_MONOTONIC)) <= 0
            raise ReadTimeout, "the peer did not complete the frame within its deadline"
          end

          raise ReadTimeout, "the peer sent nothing for #{idle} seconds"
        end

        # The message keeps only the start of the parser text, because that text quotes the payload.
        #: (String body) -> untyped
        def parse_body(body)
          JSON.parse(body)
        rescue JSON::ParserError => e
          raise Malformed, "the payload is not valid JSON: #{e.message[0, 120]}"
        end

        #: (String line, Integer max_bytes) -> Integer
        def parse_length(line, max_bytes)
          raise Malformed, "expected a decimal length, got #{line.inspect}" unless LENGTH_LINE.match?(line)

          length = Integer(line.chomp, 10)
          raise Malformed, "expected a positive length, got #{length}" if length <= 0
          raise Malformed, "a payload of #{length} bytes exceeds the limit of #{max_bytes}" if length > max_bytes

          length
        end
      end
    end
  end
end
