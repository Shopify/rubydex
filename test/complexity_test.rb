# frozen_string_literal: true

require "test_helper"
require "helpers/context"
require "json"
require "tmpdir"

class ComplexityTest < Minitest::Test
  include Test::Helpers::WithContext

  SOURCE = <<~RUBY
    class Foo
      def bar(a)
        if a
          baz(a)
        end
      end
    end
  RUBY

  def test_analyze_text_report
    with_context do |context|
      context.write!("foo.rb", SOURCE)

      output = Rubydex::Complexity.analyze([context.absolute_path])
      assert_includes(output, "total complexity")
      assert_includes(output, "average complexity")
      assert_includes(output, "Foo#bar")
    end
  end

  def test_analyze_json_report
    with_context do |context|
      context.write!("foo.rb", SOURCE)

      json = JSON.parse(Rubydex::Complexity.analyze([context.absolute_path], format: :json, top: 0))
      assert_equal(1, json["schema_version"])
      assert_equal(1, json["methods_count"])
      entry = json["methods"].first
      assert_equal("Foo#bar", entry["name"])
      assert_in_delta(1.49, entry["score"], 0.01)
    end
  end

  def test_diff_reports_changed_method
    with_context do |context|
      context.write!("foo.rb", SOURCE)
      baseline = Rubydex::Complexity.analyze([context.absolute_path], format: :json, top: 0)

      context.write!("foo.rb", <<~RUBY)
        class Foo
          def bar(a)
            if a
              baz(a)
              if a > 1
                baz(a)
              end
            end
          end
        end
      RUBY
      current = Rubydex::Complexity.analyze([context.absolute_path], format: :json, top: 0)

      diff = Rubydex::Complexity.diff(baseline, current)
      assert_includes(diff, "Regressions:")
      assert_includes(diff, "Foo#bar")
    end
  end

  def test_analyze_rejects_non_array_paths
    assert_raises(TypeError) { Rubydex::Complexity.analyze(123) }
  end

  def test_diff_rejects_non_string_arguments
    assert_raises(TypeError) { Rubydex::Complexity.diff(nil, nil) }
  end

  def test_analyze_surfaces_unreadable_path_warnings
    missing = File.join(Dir.mktmpdir, "does_not_exist.rb")
    report = nil
    _stdout, stderr = capture_io do
      report = Rubydex::Complexity.analyze([missing], format: :json, top: 0)
    end
    # The analysis still succeeds (returns a valid JSON report)...
    json = JSON.parse(report)
    assert_equal(0, json["methods_count"])
    # ...but the unreadable path is reported on stderr, not silently dropped.
    refute_match(/does_not_exist\.rb/, report)
    assert_match(/does_not_exist\.rb/, stderr)
  end

  def test_analyze_details_includes_breakdown
    with_context do |context|
      context.write!("foo.rb", "def x; a += 1; end\n")

      output = Rubydex::Complexity.analyze([context.absolute_path], details: true, top: 0)
      assert_includes(output, "main#x")
      assert_includes(output, "assignment")
      assert_includes(output, "magic_number")
    end
  end

  def test_analyze_details_json_payload
    with_context do |context|
      context.write!("foo.rb", "def x; a += 1; end\n")

      json = JSON.parse(Rubydex::Complexity.analyze([context.absolute_path], format: :json, details: true, top: 0))
      entry = json["methods"].first
      labels = entry["details"].map { |d| d["label"] }
      assert_includes(labels, "assignment")
      assert_includes(labels, "magic_number")
    end
  end

  def test_analyze_details_omitted_when_off
    with_context do |context|
      context.write!("foo.rb", "def x; a += 1; end\n")

      json = JSON.parse(Rubydex::Complexity.analyze([context.absolute_path], format: :json, top: 0))
      # `details` is skip_serializing_if empty, so a default run omits the field entirely.
      refute(json["methods"].first.key?("details"))
    end
  end

  def test_methods_only_skips_out_of_method_code
    with_context do |context|
      context.write!("foo.rb", "class Foo; include Bar; def none; bar; end; baz; end\n")

      json = JSON.parse(
        Rubydex::Complexity.analyze([context.absolute_path], format: :json, methods_only: true, top: 0),
      )
      assert_equal(1, json["methods_count"])
      assert_equal("Foo#none", json["methods"].first["name"])
      # Only `bar` (inside `def none`) is scored; the out-of-method `include`/`baz` are dropped.
      assert_operator(json["methods"].first["calls"], :<, 2.0)
      assert_equal(true, json["methods_only"])
    end
  end

  def test_group_emits_subtotals
    with_context do |context|
      context.write!("foo.rb", "class Foo; def a; 1; end; def b; 2; end; end\nclass Bar; def c; 1; end; end\n")

      output = Rubydex::Complexity.analyze([context.absolute_path], group: true, top: 0)
      assert_includes(output, "Foo total")
      assert_includes(output, "Bar total")
      # Foo (two methods) is grouped before Bar (one method).
      assert(output.index("Foo total") < output.index("Bar total"))
    end
  end

  def test_diff_rejects_mismatched_methods_only
    with_context do |context|
      context.write!("foo.rb", "class Foo; include Bar; end\n")
      baseline = Rubydex::Complexity.analyze([context.absolute_path], format: :json, top: 0)
      current = Rubydex::Complexity.analyze([context.absolute_path], format: :json, methods_only: true, top: 0)

      error = assert_raises(ArgumentError) { Rubydex::Complexity.diff(baseline, current) }
      assert_match(/mismatched scoring modes/, error.message)
    end
  end

  def test_complexity_exclude_is_decoupled_from_indexer_exclude
    with_context do |context|
      context.write!("rubydex.toml", <<~TOML)
        exclude = ["index_only_skip/**"]

        [complexity]
        exclude = ["complexity_skip/**"]
      TOML
      context.write!("keep.rb", "def keep; a = 1; end\n")
      # Excluded from indexing only -> still scored for complexity (decoupled).
      context.write!("index_only_skip/still.rb", "def still; a = 1; end\n")
      # Excluded from complexity -> skipped by the report.
      context.write!("complexity_skip/gone.rb", "def gone; a = 1; end\n")

      json = JSON.parse(Rubydex::Complexity.analyze([context.absolute_path], format: :json, top: 0))
      files = json["methods"].map { |m| m["file"] }.uniq
      assert_includes(files, File.join(context.absolute_path, "keep.rb"))
      assert_includes(files, File.join(context.absolute_path, "index_only_skip", "still.rb"))
      refute_includes(files, File.join(context.absolute_path, "complexity_skip", "gone.rb"))
    end
  end
end
