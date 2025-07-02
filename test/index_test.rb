# frozen_string_literal: true

require "test_helper"

class IndexRustyTest < Minitest::Test
  def test_that_it_has_a_version_number
    refute_nil(::Index::VERSION)
  end

  def test_index_all
    repository = Index::Repository.new
    repository.index_all([__FILE__])
  end
end
