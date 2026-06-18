# frozen_string_literal: true

require "test_helper"
require "rubydex/server"
require "socket"

module Rubydex
  module Server
    class RequestTest < Minitest::Test
      def test_write_then_read_round_trips_a_payload
        a, b = UNIXSocket.pair

        payload = { "command" => "query", "query" => "MATCH (n) RETURN n", "token" => "abc" }
        Request.write(a, payload)

        assert_equal(payload, Request.read(b))
      ensure
        a&.close
        b&.close
      end

      def test_read_returns_nil_on_eof
        a, b = UNIXSocket.pair
        a.close

        assert_nil(Request.read(b))
      ensure
        b&.close
      end

      def test_read_handles_large_payloads
        a, b = UNIXSocket.pair

        payload = { "stdout" => "x" * 100_000 }
        Thread.new { Request.write(a, payload) }

        assert_equal(payload, Request.read(b))
      ensure
        a&.close
        b&.close
      end

      def test_read_line_with_timeout_raises_when_nothing_arrives
        a, b = UNIXSocket.pair

        assert_raises(ServerReadTimeout) do
          Request.read_line_with_timeout(b, 0.05)
        end
      ensure
        a&.close
        b&.close
      end

      def test_read_line_with_timeout_returns_the_line
        a, b = UNIXSocket.pair
        a.puts("0.2.5:fingerprint")

        assert_equal("0.2.5:fingerprint", Request.read_line_with_timeout(b, 1.0).chomp)
      ensure
        a&.close
        b&.close
      end
    end
  end
end
