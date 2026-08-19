# frozen_string_literal: true

require "test_helper"

class DiagnosticTest < Minitest::Test
  class ExampleRule < Rubydex::Rule
    class << self
      def default_severity = Rubydex::Severity::Warning
    end
  end

  def test_severity_from_value
    {
      error: Rubydex::Severity::Error,
      warning: Rubydex::Severity::Warning,
      information: Rubydex::Severity::Information,
      hint: Rubydex::Severity::Hint,
    }.each do |value, severity|
      assert_equal(severity, Rubydex::Severity.from_value(value))
    end
  end

  def test_severity_from_value_rejects_invalid_values
    error = assert_raises(ArgumentError) do
      Rubydex::Severity.from_value(:critical)
    end

    assert_equal(
      "Invalid severity :critical. Expected one of: error, warning, information, hint",
      error.message,
    )
  end

  def test_diagnostic_exposes_its_rule_and_related_information
    related = Rubydex::RelatedInformation.new("Also defined here", location(uri: "file:///related.rb"))
    diagnostic = Rubydex::Diagnostic.new(
      rule: ExampleRule,
      message: "Example",
      location: location,
      related_information: [related],
    )

    assert_equal(ExampleRule, diagnostic.rule)
    assert_equal("Example", diagnostic.message)
    assert_equal(location, diagnostic.location)
    assert_equal([related], diagnostic.related_information)
  end

  private

  def location(uri: "file:///example.rb")
    Rubydex::Location.new(uri: uri, start_line: 0, end_line: 0, start_column: 0, end_column: 1)
  end
end
