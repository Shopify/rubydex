# frozen_string_literal: true

require "test_helper"

class SignatureTest < Minitest::Test
  def test_signature_with_parameters
    loc = Rubydex::Location.new(uri: "/test.rb", start_line: 0, end_line: 0, start_column: 0, end_column: 1)
    params = [
      Rubydex::Signature::PositionalParameter.new(:a, loc),
      Rubydex::Signature::KeywordParameter.new(:b, loc),
      Rubydex::Signature::BlockParameter.new(:block, loc),
    ]
    sig = Rubydex::Signature.new(params)

    assert_equal(3, sig.parameters.length)
    assert_instance_of(Rubydex::Signature::PositionalParameter, sig.parameters[0])
    assert_instance_of(Rubydex::Signature::KeywordParameter, sig.parameters[1])
    assert_instance_of(Rubydex::Signature::BlockParameter, sig.parameters[2])
  end

  def test_deconstruct
    loc = Rubydex::Location.new(uri: "/test.rb", start_line: 0, end_line: 0, start_column: 0, end_column: 1)
    params = [
      Rubydex::Signature::PositionalParameter.new(:a, loc),
      Rubydex::Signature::OptionalPositionalParameter.new(:b, loc),
      Rubydex::Signature::RestPositionalParameter.new(:c, loc),
      Rubydex::Signature::PostParameter.new(:d, loc),
      Rubydex::Signature::KeywordParameter.new(:e, loc),
      Rubydex::Signature::OptionalKeywordParameter.new(:f, loc),
      Rubydex::Signature::RestKeywordParameter.new(:g, loc),
      Rubydex::Signature::BlockParameter.new(:h, loc),
    ]
    sig = Rubydex::Signature.new(params)

    sig => [positionals, optional_positionals, rest_positional, posts,
      keywords, optional_keywords, rest_keyword, forward, block]

    assert_equal([:a], positionals.map(&:name))
    assert_equal([:b], optional_positionals.map(&:name))
    assert_equal(:c, rest_positional.name)
    assert_equal([:d], posts.map(&:name))
    assert_equal([:e], keywords.map(&:name))
    assert_equal([:f], optional_keywords.map(&:name))
    assert_equal(:g, rest_keyword.name)
    assert_nil(forward)
    assert_equal(:h, block.name)
  end

  def test_deconstruct_keys
    loc = Rubydex::Location.new(uri: "/test.rb", start_line: 0, end_line: 0, start_column: 0, end_column: 1)
    params = [
      Rubydex::Signature::PositionalParameter.new(:a, loc),
      Rubydex::Signature::OptionalPositionalParameter.new(:b, loc),
      Rubydex::Signature::RestPositionalParameter.new(:c, loc),
      Rubydex::Signature::PostParameter.new(:d, loc),
      Rubydex::Signature::KeywordParameter.new(:e, loc),
      Rubydex::Signature::OptionalKeywordParameter.new(:f, loc),
      Rubydex::Signature::RestKeywordParameter.new(:g, loc),
      Rubydex::Signature::BlockParameter.new(:h, loc),
    ]
    sig = Rubydex::Signature.new(params)

    sig => { positional_parameters:, block_parameter: }
    assert_equal([:a], positional_parameters.map(&:name))
    assert_equal(:h, block_parameter.name)

    sig => { keyword_parameters:, rest_keyword_parameter: }
    assert_equal([:e], keyword_parameters.map(&:name))
    assert_equal(:g, rest_keyword_parameter.name)
  end

  def test_accessors
    loc = Rubydex::Location.new(uri: "/test.rb", start_line: 0, end_line: 0, start_column: 0, end_column: 1)
    params = [
      Rubydex::Signature::PositionalParameter.new(:a, loc),
      Rubydex::Signature::OptionalPositionalParameter.new(:b, loc),
      Rubydex::Signature::RestPositionalParameter.new(:c, loc),
      Rubydex::Signature::PostParameter.new(:d, loc),
      Rubydex::Signature::KeywordParameter.new(:e, loc),
      Rubydex::Signature::OptionalKeywordParameter.new(:f, loc),
      Rubydex::Signature::RestKeywordParameter.new(:g, loc),
      Rubydex::Signature::BlockParameter.new(:h, loc),
    ]
    sig = Rubydex::Signature.new(params)

    assert_equal([:a], sig.positional_parameters.map(&:name))
    assert_equal([:b], sig.optional_positional_parameters.map(&:name))
    assert_equal(:c, sig.rest_positional_parameter.name)
    assert_equal([:d], sig.post_parameters.map(&:name))
    assert_equal([:e], sig.keyword_parameters.map(&:name))
    assert_equal([:f], sig.optional_keyword_parameters.map(&:name))
    assert_equal(:g, sig.rest_keyword_parameter.name)
    assert_equal(:h, sig.block_parameter.name)
  end

  def test_deconstruct_with_forward
    loc = Rubydex::Location.new(uri: "/test.rb", start_line: 0, end_line: 0, start_column: 0, end_column: 1)
    sig = Rubydex::Signature.new([Rubydex::Signature::ForwardParameter.new(:fwd, loc)])

    sig => [*, forward, _]
    assert_equal(:fwd, forward.name)

    assert_equal(:fwd, sig.forward_parameter.name)
  end
end
