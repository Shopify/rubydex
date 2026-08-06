# frozen_string_literal: true

require "test_helper"
require "helpers/context"
require "mocha/minitest"
require "rubydex/skill"
require "rubydex/skill_registry"

class SkillRegistryTest < Minitest::Test
  include Test::Helpers::WithContext

  def test_load_scans_directory_and_returns_ids
    with_context do |context|
      write_skill(context, "alpha", "Alpha skill.", "# Alpha")
      write_skill(context, "beta", "Beta skill.", "# Beta")

      registry = Rubydex::SkillRegistry.load(context.absolute_path_to("skills"))

      assert_equal(["alpha", "beta"], registry.ids)
    end
  end

  def test_load_records_paths_without_reading_the_files
    with_context do |context|
      write_skill(context, "alpha", "Alpha skill.", "# Alpha")

      # A skill file is only opened when it is fetched, so scanning records paths and nothing else.
      File.expects(:read).never

      registry = Rubydex::SkillRegistry.load(context.absolute_path_to("skills"))

      assert_equal(["alpha"], registry.ids)
    end
  end

  def test_fetch_returns_a_loaded_skill
    with_context do |context|
      write_skill(context, "alpha", "Alpha skill.", "# Alpha\n\nBody.")

      registry = Rubydex::SkillRegistry.load(context.absolute_path_to("skills"))
      skill = registry.fetch("alpha")

      assert_equal("alpha", skill.name)
      assert_equal("Alpha skill.", skill.description)
      assert_equal("# Alpha\n\nBody.", skill.body)
    end
  end

  def test_fetch_caches_the_loaded_skill
    with_context do |context|
      write_skill(context, "alpha", "Alpha skill.", "# Alpha")

      registry = Rubydex::SkillRegistry.load(context.absolute_path_to("skills"))

      first = registry.fetch("alpha")
      second = registry.fetch("alpha")

      assert_same(first, second)
    end
  end

  def test_fetch_names_only_the_id_for_an_unknown_skill
    with_context do |context|
      write_skill(context, "alpha", "Alpha skill.", "# Alpha")
      write_skill(context, "beta", "Beta skill.", "# Beta")

      registry = Rubydex::SkillRegistry.load(context.absolute_path_to("skills"))

      error = assert_raises(Rubydex::UnknownSkillError) { registry.fetch("nonexistent") }

      # Which ids to offer instead is the interface's decision, so the registry does not spell them
      # into the message.
      assert_equal("Unknown skill: nonexistent", error.message)
      assert_kind_of(Rubydex::SkillError, error)
    end
  end

  def test_fetch_rejects_a_name_that_disagrees_with_the_directory
    with_context do |context|
      # The directory is the id callers use, so a frontmatter `name` that spells something else is a
      # second, unreachable id rather than a harmless mismatch.
      context.write!("skills/alpha/SKILL.md", <<~SKILL)
        ---
        name: not-alpha
        description: Alpha skill.
        ---

        # Alpha
      SKILL

      registry = Rubydex::SkillRegistry.load(context.absolute_path_to("skills"))

      error = assert_raises(Rubydex::SkillError) { registry.fetch("alpha") }

      assert_match(/declares `name: not-alpha` but lives in `alpha`/, error.message)
    end
  end

  def test_fetch_reports_a_malformed_skill_file_as_a_skill_error
    with_context do |context|
      context.write!("skills/alpha/SKILL.md", <<~SKILL)
        ---
        name: alpha
        description: "unterminated
        ---

        # Alpha
      SKILL

      registry = Rubydex::SkillRegistry.load(context.absolute_path_to("skills"))

      error = assert_raises(Rubydex::SkillError) { registry.fetch("alpha") }

      assert_match(/Malformed YAML frontmatter/, error.message)
    end
  end

  def test_load_skips_subdirectories_without_skill_file
    with_context do |context|
      write_skill(context, "alpha", "Alpha skill.", "# Alpha")
      context.write!("skills/not-a-skill/other.md", "# Other")

      registry = Rubydex::SkillRegistry.load(context.absolute_path_to("skills"))

      assert_equal(["alpha"], registry.ids)
    end
  end

  def test_load_returns_empty_registry_for_empty_directory
    with_context do |context|
      context.write!("skills/.keep", "")

      registry = Rubydex::SkillRegistry.load(context.absolute_path_to("skills"))

      assert_empty(registry.ids)
    end
  end

  def test_load_raises_when_directory_does_not_exist
    with_context do |context|
      assert_raises(Rubydex::UnknownSkillDirectoryError) do
        Rubydex::SkillRegistry.load(context.absolute_path_to("nonexistent"))
      end
    end
  end

  def test_registry_is_frozen
    with_context do |context|
      write_skill(context, "alpha", "Alpha skill.", "# Alpha")

      registry = Rubydex::SkillRegistry.load(context.absolute_path_to("skills"))

      assert_predicate(registry, :frozen?)
      # The cache lives inside the frozen registry, so fetching must still work.
      assert_equal("alpha", registry.fetch("alpha").name)
    end
  end

  private

  #: (Test::Helpers::Context, String, String, String) -> void
  def write_skill(context, id, description, body)
    context.write!("skills/#{id}/SKILL.md", <<~SKILL)
      ---
      name: #{id}
      description: #{description}
      ---

      #{body}
    SKILL
  end
end
