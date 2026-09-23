# frozen_string_literal: true

require "test_helper"
require "helpers/context"
require "mocha/minitest"
require "rubydex/dead_code"

class DeadCodeTest < Minitest::Test
  include Test::Helpers::WithContext

  def test_candidates_include_sorted_workspace_definitions_and_name_locations
    with_context do |context|
      context.write!("zebra.rb", "class Zebra; end\n")
      context.write!("alpha two.rb", "\n  module Alpha; end\n")
      context.write!("alpha one.rb", "module Alpha; end\n")
      context.write!("constants.rb", "UNUSED = 1\nclass Used; end\nUsed.new\n")
      graph = indexed_graph(context.absolute_path, context.glob("*.rb"))

      assert_equal(
        [
          {
            name: "Alpha",
            kind: "Module",
            locations: [
              { path: "alpha one.rb", line: 1, column: 8 },
              { path: "alpha two.rb", line: 2, column: 10 },
            ],
          },
          { name: "UNUSED", kind: "Constant", locations: [{ path: "constants.rb", line: 1, column: 1 }] },
          { name: "Zebra", kind: "Class", locations: [{ path: "zebra.rb", line: 1, column: 7 }] },
        ],
        Rubydex::DeadCode.candidates(graph),
      )
    end
  end

  def test_dependency_references_count_but_only_workspace_definitions_are_reported
    with_context do |context|
      context.write!("app/models.rb", "class UsedByDependency; end\nclass Shared; end\n")
      context.write!("app-dependency/library.rb", "UsedByDependency\nclass Shared; end\nclass External; end\n")
      graph = indexed_graph(context.absolute_path_to("app"), [context.absolute_path])

      assert_equal(
        [{ name: "Shared", kind: "Class", locations: [{ path: "models.rb", line: 2, column: 7 }] }],
        Rubydex::DeadCode.candidates(graph),
      )
    end
  end

  def test_path_filter_reports_only_matching_definition_locations
    with_context do |context|
      context.write!("app/shared.rb", "class Shared; end\n")
      context.write!("lib/shared.rb", "class Shared; end\nclass Outside; end\n")
      graph = indexed_graph(context.absolute_path, [context.absolute_path])

      assert_equal(
        [{ name: "Shared", kind: "Class", locations: [{ path: "app/shared.rb", line: 1, column: 7 }] }],
        Rubydex::DeadCode.candidates(graph, path: "./app/shared.rb"),
      )
      assert_empty(Rubydex::DeadCode.candidates(graph, path: "missing/**/*.rb"))
    end
  end

  def test_recursive_path_filter_preserves_references_from_other_paths
    with_context do |context|
      context.write!("app/services/local.rb", "class Local; end\nclass Used; end\n")
      context.write!("app/services/nested/inner.rb", "class Inner; end\n")
      context.write!("app/services-other/outside.rb", "class Outside; end\n")
      context.write!("lib/caller.rb", "Used.new\n")
      graph = indexed_graph(context.absolute_path, [context.absolute_path])

      ["app/services/**", "app/services/**/*.rb"].each do |path|
        candidates = Rubydex::DeadCode.candidates(graph, path: path)

        assert_equal(["Inner", "Local"], candidates.map { |candidate| candidate[:name] })
      end

      candidates = Rubydex::DeadCode.candidates(graph, path: "app/services/*.rb")

      assert_equal(["Local"], candidates.map { |candidate| candidate[:name] })
    end
  end

  def test_gems_installed_under_the_workspace_are_not_reported
    with_context do |context|
      context.write!("app.rb", "class Local; end\nclass Used; end\n")
      context.write!("vendor/bundle/gems/example/lib/example.rb", "class Dependency; end\nUsed\n")
      graph = indexed_graph(context.absolute_path, [context.absolute_path])
      Gem.stubs(:path).returns([context.absolute_path_to("vendor/bundle"), context.absolute_path_to("missing")])

      assert_equal(["Local"], Rubydex::DeadCode.candidates(graph).map { |candidate| candidate[:name] })
    end
  end

  def test_config_exclusions_hide_definitions_but_preserve_their_references
    with_context do |context|
      context.write!("rubydex.toml", <<~TOML)
        [dead-code]
        exclude = ["./generated/**", "app/exempt.rb"]
      TOML
      context.write!("app/models.rb", "class Used; end\nclass Shared; end\n")
      context.write!("app/exempt.rb", "class Exempt; end\n")
      context.write!("generated/nested/models.rb", "class Shared; end\nclass Hidden; end\nUsed.new\n")
      graph = indexed_graph(context.absolute_path, [context.absolute_path])

      refute_nil(graph["Hidden"])
      refute_nil(graph["Exempt"])
      assert_equal(
        [{ name: "Shared", kind: "Class", locations: [{ path: "app/models.rb", line: 2, column: 7 }] }],
        Rubydex::DeadCode.candidates(graph),
      )
      assert_empty(Rubydex::DeadCode.candidates(graph, path: "generated/**"))
    end
  end

  def test_exclusion_globs_respect_directory_boundaries
    with_context do |context|
      context.write!("rubydex.toml", <<~TOML)
        [dead-code]
        exclude = ["app/generated/*.rb"]
      TOML
      context.write!("app/generated/ignored.rb", "class Ignored; end\n")
      context.write!("app/generated/nested/kept.rb", "class Nested; end\n")
      context.write!("app/generated-other/kept.rb", "class Sibling; end\n")
      graph = indexed_graph(context.absolute_path, [context.absolute_path])

      assert_equal(["Nested", "Sibling"], Rubydex::DeadCode.candidates(graph).map { |candidate| candidate[:name] })

      context.write!("rubydex.toml", <<~TOML)
        [dead-code]
        exclude = ["app/generated/**"]
      TOML
      graph.load_config(Rubydex::Config.load(context.absolute_path))

      assert_equal(["Sibling"], Rubydex::DeadCode.candidates(graph).map { |candidate| candidate[:name] })
    end
  end

  def test_exclusions_match_file_paths_with_spaces_and_unicode
    with_context do |context|
      excluded_path = "app/caf\u00e9 helpers/ignored.rb"
      kept_path = "app/caf\u00e9 helpers/kept.rb"
      context.write!("rubydex.toml", <<~TOML)
        [dead-code]
        exclude = ["#{excluded_path}"]
      TOML
      context.write!(excluded_path, "class Ignored; end\n")
      context.write!(kept_path, "class Kept; end\n")
      graph = indexed_graph(context.absolute_path, [context.absolute_path])

      refute_nil(graph["Ignored"])
      assert_equal(
        [{ name: "Kept", kind: "Class", locations: [{ path: kept_path, line: 1, column: 7 }] }],
        Rubydex::DeadCode.candidates(graph, path: "app/caf\u00e9 helpers/**"),
      )
    end
  end

  def test_dependency_paths_through_a_workspace_symlink_are_not_reported
    with_context do |context|
      context.write!("app/app.rb", "class Local; end\n")
      context.write!("app/vendor/bundle/gems/example/lib/example.rb", "class Dependency; end\n")
      File.symlink(context.absolute_path_to("app"), context.absolute_path_to("app-link"))
      graph = indexed_graph(context.absolute_path_to("app-link"), [context.absolute_path_to("app")])
      Gem.stubs(:path).returns([context.absolute_path_to("app-link/vendor/bundle")])

      assert_equal(["Local"], Rubydex::DeadCode.candidates(graph).map { |candidate| candidate[:name] })
    end
  end

  def test_symlinked_gem_homes_under_the_workspace_are_not_reported
    with_context do |context|
      context.write!("app/app.rb", "class Local; end\n")
      context.write!("gems/example/lib/example.rb", "class Dependency; end\n")
      File.symlink(context.absolute_path_to("gems"), context.absolute_path_to("app/vendor"))
      graph = indexed_graph(context.absolute_path_to("app"), [context.absolute_path_to("app")])
      Gem.stubs(:path).returns([context.absolute_path_to("app/vendor")])

      assert_equal(["Local"], Rubydex::DeadCode.candidates(graph).map { |candidate| candidate[:name] })
    end
  end

  def test_non_file_documents_are_not_reported
    with_context do |context|
      context.write!("app.rb", "class Used; end\n")
      graph = indexed_graph(context.absolute_path, [context.absolute_path])
      graph.index_source("untitled:Untitled-1", "class Unsaved; end\nUsed\n", "ruby")
      graph.resolve

      assert_empty(Rubydex::DeadCode.candidates(graph))
    end
  end

  private

  def indexed_graph(workspace, paths)
    graph = Rubydex::Graph.configure_for_workspace(workspace)
    assert_empty(graph.index_all(paths))
    graph.resolve
  end
end
