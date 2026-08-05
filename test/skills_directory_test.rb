# frozen_string_literal: true

require "test_helper"
require "rubydex/skill"
require "rubydex/skill_registry"

# Validates that every skill shipped in the repository's `skills/` directory can be parsed and
# satisfies the contracts the registry enforces: a `SKILL.md` with YAML frontmatter declaring a
# `name` (matching its directory) and `description`, plus a non-empty Markdown body.
# This guards against a malformed skill file landing in a release unnoticed.
class SkillsDirectoryTest < Minitest::Test
  SKILLS_DIRECTORY = File.expand_path("../skills", __dir__) #: String

  def test_every_skill_in_the_repository_parses
    registry = Rubydex::SkillRegistry.load(SKILLS_DIRECTORY)

    refute_empty(registry.ids, "expected at least one skill in #{SKILLS_DIRECTORY}")

    registry.ids.each do |id|
      skill = registry.fetch(id)

      assert_equal(id, skill.name, "frontmatter `name` for #{id} must match its directory")
      refute_empty(skill.description, "description for #{id} must not be empty")
      refute_empty(skill.body, "body for #{id} must not be empty")
    end
  end
end
