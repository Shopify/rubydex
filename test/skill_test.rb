# frozen_string_literal: true

require "test_helper"
require "helpers/context"
require "rubydex/skill"

class SkillTest < Minitest::Test
  include Test::Helpers::WithContext

  def test_load_parses_name_description_and_body
    with_context do |context|
      context.write!("SKILL.md", <<~SKILL)
        ---
        name: my-skill
        description: Does a thing.
        ---

        # My Skill

        Some body text.
      SKILL

      skill = Rubydex::Skill.load(context.absolute_path_to("SKILL.md"))

      assert_equal("my-skill", skill.name)
      assert_equal("Does a thing.", skill.description)
      assert_equal("# My Skill\n\nSome body text.", skill.body)
    end
  end

  def test_load_freezes_the_instance
    with_context do |context|
      context.write!("SKILL.md", <<~SKILL)
        ---
        name: my-skill
        description: Does a thing.
        ---

        Body.
      SKILL

      skill = Rubydex::Skill.load(context.absolute_path_to("SKILL.md"))

      assert_predicate(skill, :frozen?)
      assert_predicate(skill.name, :frozen?)
      assert_predicate(skill.description, :frozen?)
      assert_predicate(skill.body, :frozen?)
    end
  end

  def test_from_content_parses_without_a_file
    skill = Rubydex::Skill.from_content(<<~SKILL)
      ---
      name: inline-skill
      description: Inline description.
      ---

      # Inline
    SKILL

    assert_equal("inline-skill", skill.name)
    assert_equal("Inline description.", skill.description)
    assert_equal("# Inline", skill.body)
  end

  def test_load_raises_when_frontmatter_is_missing
    with_context do |context|
      context.write!("SKILL.md", "# No frontmatter here")

      error = assert_raises(Rubydex::SkillError) do
        Rubydex::Skill.load(context.absolute_path_to("SKILL.md"))
      end

      assert_match(/Missing YAML frontmatter/, error.message)
    end
  end

  def test_load_raises_when_frontmatter_is_unterminated
    with_context do |context|
      context.write!("SKILL.md", <<~SKILL)
        ---
        name: my-skill
        description: Does a thing.

        # No closing delimiter
      SKILL

      error = assert_raises(Rubydex::SkillError) do
        Rubydex::Skill.load(context.absolute_path_to("SKILL.md"))
      end

      assert_match(/Unterminated/, error.message)
    end
  end

  def test_load_raises_when_name_is_missing
    with_context do |context|
      context.write!("SKILL.md", <<~SKILL)
        ---
        description: Does a thing.
        ---

        Body.
      SKILL

      error = assert_raises(Rubydex::SkillError) do
        Rubydex::Skill.load(context.absolute_path_to("SKILL.md"))
      end

      assert_match(/`name`/, error.message)
    end
  end

  def test_load_raises_when_description_is_missing
    with_context do |context|
      context.write!("SKILL.md", <<~SKILL)
        ---
        name: my-skill
        ---

        Body.
      SKILL

      error = assert_raises(Rubydex::SkillError) do
        Rubydex::Skill.load(context.absolute_path_to("SKILL.md"))
      end

      assert_match(/`description`/, error.message)
    end
  end

  def test_load_includes_path_in_error_message
    with_context do |context|
      context.write!("SKILL.md", "# No frontmatter")
      path = context.absolute_path_to("SKILL.md")

      error = assert_raises(Rubydex::SkillError) do
        Rubydex::Skill.load(path)
      end

      assert_match(/SKILL\.md/, error.message)
    end
  end

  def test_from_content_omits_path_in_error_message
    error = assert_raises(Rubydex::SkillError) do
      Rubydex::Skill.from_content("# No frontmatter")
    end

    refute_match(/ in /, error.message)
  end

  def test_load_reports_a_missing_file_as_a_skill_error
    with_context do |context|
      path = context.absolute_path_to("nonexistent/SKILL.md")

      error = assert_raises(Rubydex::SkillError) { Rubydex::Skill.load(path) }

      assert_match(/Cannot read skill file #{Regexp.escape(path)}/, error.message)
    end
  end

  def test_load_reports_malformed_yaml_as_a_skill_error
    with_context do |context|
      # An unterminated quoted scalar makes Psych raise; the caller must still see a `SkillError`.
      context.write!("SKILL.md", <<~SKILL)
        ---
        name: my-skill
        description: "unterminated
        ---

        Body.
      SKILL
      path = context.absolute_path_to("SKILL.md")

      error = assert_raises(Rubydex::SkillError) { Rubydex::Skill.load(path) }

      assert_match(/Malformed YAML frontmatter in #{Regexp.escape(path)}/, error.message)
    end
  end

  def test_from_content_reports_a_disallowed_yaml_tag_as_a_skill_error
    error = assert_raises(Rubydex::SkillError) do
      Rubydex::Skill.from_content("---\nname: my-skill\ndescription: !ruby/object {}\n---\n\nBody.\n")
    end

    assert_match(/Malformed YAML frontmatter/, error.message)
  end
end
