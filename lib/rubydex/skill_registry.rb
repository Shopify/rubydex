# frozen_string_literal: true

require "rubydex/skill"

module Rubydex
  # The skills available on disk, keyed by id. `load` scans a directory for `<id>/SKILL.md` entries
  # and records their paths.
  # A skill file is only read when {#fetch} asks for it. Listing every description reads all skill files.
  #
  # The directory name is the id: it is what `rdx skill <id>` takes as a parameter.
  class SkillRegistry
    class << self
      #: (String directory) -> Rubydex::SkillRegistry
      def load(directory)
        unless File.directory?(directory)
          raise UnknownSkillDirectoryError, "Skill directory does not exist: #{directory}"
        end

        paths = {}
        Dir.foreach(directory) do |entry|
          next if entry == "." || entry == ".."

          skill_path = File.join(directory, entry, "SKILL.md")
          next unless File.file?(skill_path)

          paths[entry] = skill_path
        end

        new(paths)
      end
    end

    #: (Hash[String, String] paths) -> void
    def initialize(paths)
      @paths = paths.freeze
      # Deliberately mutable inside a frozen registry: `freeze` protects the set of skills, while
      # the cache only remembers what was already read from those files.
      @cache = {} #: Hash[String, Skill]
      freeze
    end

    #: -> Array[String]
    def ids
      @paths.keys.sort
    end

    #: (String id) -> Rubydex::Skill
    def fetch(id)
      @cache[id] ||= begin
        path = @paths[id]
        raise UnknownSkillError, "Unknown skill: #{id}" unless path

        skill = Skill.load(path)
        unless skill.name == id
          raise SkillError, "Skill in #{path} declares `name: #{skill.name}` but lives in `#{id}`"
        end

        skill
      end
    end
  end
end
