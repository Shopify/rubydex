# frozen_string_literal: true

require "test_helper"
require "rubydex/server"
require "socket"

module Rubydex
  module Server
    class FrameTest < Minitest::Test
      def setup
        # Framing needs a socket pair and nothing else: no `State`, no fork, no `O_NOFOLLOW`.
        skip("this platform has no UNIX sockets") unless defined?(::UNIXSocket)
      end

      def test_write_then_read_round_trips_a_payload
        a, b = UNIXSocket.pair

        payload = { "command" => "query", "query" => "MATCH (n) RETURN n", "token" => "abc" }
        Frame.write(a, payload)

        assert_equal(payload, Frame.read_request(b))
      ensure
        a&.close
        b&.close
      end

      def test_read_returns_nil_on_eof
        a, b = UNIXSocket.pair
        a.close

        assert_nil(Frame.read_request(b))
      ensure
        b&.close
      end

      def test_read_handles_large_payloads
        a, b = UNIXSocket.pair

        payload = { "stdout" => "x" * 100_000 }
        Thread.new { Frame.write(a, payload) }

        assert_equal(payload, Frame.read_request(b))
      ensure
        a&.close
        b&.close
      end

      def test_read_line_raises_when_nothing_arrives
        a, b = UNIXSocket.pair

        assert_raises(Frame::ReadTimeout) do
          Frame.read_line(b, timeout: 0.05)
        end
      ensure
        a&.close
        b&.close
      end

      def test_read_line_returns_the_line
        a, b = UNIXSocket.pair
        a.puts("0.2.5:fingerprint")

        assert_equal("0.2.5:fingerprint", Frame.read_line(b, timeout: 1.0).chomp)
      ensure
        a&.close
        b&.close
      end

      def test_read_rejects_a_length_line_that_is_not_a_number
        a, b = UNIXSocket.pair
        a.puts("100garbage")
        a.write("x" * 100)
        a.flush

        # `String#to_i` would accept this line and return 100.
        error = assert_raises(Frame::Malformed) { Frame.read_request(b, timeout: 1.0) }

        assert_match(/expected a decimal length/, error.message)
      ensure
        a&.close
        b&.close
      end

      def test_read_rejects_a_payload_that_is_not_an_object
        a, b = UNIXSocket.pair
        body = JSON.dump([1, 2, 3])
        a.puts(body.bytesize)
        a.write(body)
        a.flush

        error = assert_raises(Frame::Malformed) { Frame.read_request(b, timeout: 1.0) }

        assert_match(/expected a JSON object/, error.message)
      ensure
        a&.close
        b&.close
      end

      # The JSON is valid on its own, so only the length check rejects this short body as a complete
      # frame.
      def test_read_rejects_a_truncated_frame
        a, b = UNIXSocket.pair
        a.puts(100)
        a.write("{}")
        a.flush
        a.close

        error = assert_raises(Frame::Malformed) { Frame.read_request(b, timeout: 1.0) }

        assert_match(/closed the connection after 2 of 100 bytes/, error.message)
      ensure
        a&.close unless a&.closed?
        b&.close
      end

      def test_read_request_times_out_on_a_short_body
        a, b = UNIXSocket.pair
        a.puts(1000)
        a.write("only a few bytes")
        a.flush

        assert_raises(Frame::ReadTimeout) { Frame.read_request(b, timeout: 0.2) }
      ensure
        a&.close
        b&.close
      end

      # A response may be slow but steady, so the read must survive the gap between two chunks, not a
      # stall.
      def test_read_response_accepts_a_body_that_arrives_slowly_but_steadily
        a, b = UNIXSocket.pair
        payload = { "stdout" => "abcdefgh" }
        body = JSON.dump(payload)

        writer = Thread.new do
          a.puts(body.bytesize)
          body.each_char do |char|
            a.write(char)
            a.flush
            sleep(0.05)
          end
        end

        # The transfer takes about 0.4 seconds in total, which is longer than the idle timeout.
        assert_equal(payload, Frame.read_response(b, idle_timeout: 0.25))
      ensure
        writer&.join
        a&.close
        b&.close
      end

      # A server sends nothing until the query finishes, so a client must wait for the first byte
      # however long the query takes.
      def test_read_response_waits_without_a_limit_for_the_first_byte
        a, b = UNIXSocket.pair
        payload = { "stdout" => "the query finished" }

        writer = Thread.new do
          sleep(0.3)
          Frame.write(a, payload)
        end

        # The idle timeout is far shorter than the wait for the first byte.
        assert_equal(payload, Frame.read_response(b, idle_timeout: 0.05))
      ensure
        writer&.join
        a&.close
        b&.close
      end

      def test_read_response_still_times_out_after_the_first_byte_arrives
        a, b = UNIXSocket.pair
        a.puts(1000)
        a.write("a partial body")
        a.flush

        assert_raises(Frame::ReadTimeout) do
          Frame.read_response(b, idle_timeout: 0.2)
        end
      ensure
        a&.close
        b&.close
      end

      # A query result may be larger than any request, so the two directions carry different caps.
      def test_read_response_accepts_a_payload_above_the_request_cap
        assert_operator(Frame::MAX_RESPONSE_BYTES, :>, Frame::MAX_REQUEST_BYTES)

        a, b = UNIXSocket.pair
        payload = { "stdout" => "x" * (Frame::MAX_REQUEST_BYTES + 1) }
        writer = Thread.new { Frame.write(a, payload) }

        assert_equal(payload, Frame.read_response(b))
      ensure
        writer&.join
        a&.close
        b&.close
      end

      def test_read_rejects_a_response_sized_payload_on_the_request_path
        a, b = UNIXSocket.pair
        a.puts(Frame::MAX_REQUEST_BYTES + 1)
        a.flush

        error = assert_raises(Frame::Malformed) { Frame.read_request(b, timeout: 1.0) }

        assert_match(/exceeds the limit of #{Frame::MAX_REQUEST_BYTES}/, error.message)
      ensure
        a&.close
        b&.close
      end

      # A request needs an absolute deadline, because a client that drips within the idle limit could
      # otherwise hold the single-client server forever.
      def test_read_request_stops_a_client_that_drips_bytes
        a, b = UNIXSocket.pair
        body = JSON.dump({ "command" => "query", "query" => "MATCH (n) RETURN n" })

        writer = Thread.new do
          a.puts(body.bytesize)
          body.each_char do |char|
            a.write(char)
            a.flush
            sleep(0.05)
          end
        rescue IOError, Errno::EPIPE
          nil
        end

        assert_raises(Frame::ReadTimeout) { Frame.read_request(b, timeout: 0.3) }
      ensure
        b&.close
        writer&.kill
        a&.close
      end

      # The module raises its own error types, so a caller never rescues `JSON::ParserError` for a
      # body that is not JSON.
      def test_read_reports_a_body_that_is_not_json_as_a_malformed_frame
        a, b = UNIXSocket.pair
        body = "this is not json"
        a.puts(body.bytesize)
        a.write(body)
        a.flush

        error = assert_raises(Frame::Malformed) { Frame.read_request(b, timeout: 1.0) }

        assert_match(/not valid JSON/, error.message)
      ensure
        a&.close
        b&.close
      end

      def test_read_response_reports_a_body_that_is_not_json_as_a_malformed_frame
        a, b = UNIXSocket.pair
        body = "{ \"stdout\": "
        a.puts(body.bytesize)
        a.write(body)
        a.flush

        assert_raises(Frame::Malformed) { Frame.read_response(b) }
      ensure
        a&.close
        b&.close
      end

      # `IO#read_nonblock` allocates its `maxlen` before it reads, so the reader must cap each read at
      # one chunk, not the advertised length.
      def test_read_asks_for_no_more_than_one_chunk_per_read
        a, b = UNIXSocket.pair
        requested = []
        b.define_singleton_method(:read_nonblock) do |maxlen, **kwargs|
          requested << maxlen
          super(maxlen, **kwargs)
        end

        payload = { "stdout" => "x" * (Frame::CHUNK_BYTES * 3) }
        writer = Thread.new { Frame.write(a, payload) }

        assert_equal(payload, Frame.read_response(b))
        assert_operator(requested.max, :<=, Frame::CHUNK_BYTES)
      ensure
        writer&.join
        a&.close
        b&.close
      end
    end
  end
end
