# frozen_string_literal: true

require "rubydex/cli/command"

module Rubydex
  module CLI
    class Command
      # `rdx skill [ID]` — lists available skill ids, or loads and prints a skill's body by id.
      class Skill < Command
        command "skill"
        arguments "[ID]"
        summary <<~TEXT
          Load a Rubydex skill from the library by id. With no id, list the available skills.
        TEXT

        #: -> void
        def run
          parse_options!

          id = argv.shift
          abort_with_usage("unexpected argument: #{argv.first}") unless argv.empty?

          id ? print_skill(id) : list_skills
        rescue Rubydex::SkillError => e
          abort(e.message)
        end

        private

        #: -> void
        def list_skills
          ids = registry.ids

          if ids.empty?
            puts(available_skills)
            return
          end

          ids.each { |id| puts(id) }
        end

        #: (String id) -> void
        def print_skill(id)
          puts(registry.fetch(id).body)
        rescue Rubydex::UnknownSkillError => e
          abort("#{e.message}. #{available_skills}")
        end

        #: -> String
        def available_skills
          ids = registry.ids

          ids.empty? ? "No skills are available." : "Available skills: #{ids.join(", ")}"
        end

        #: -> Rubydex::SkillRegistry
        def registry
          require "rubydex/skill_registry"
          @registry ||= Rubydex::SkillRegistry.load(skills_directory)
        end

        #: -> String
        def skills_directory
          File.expand_path("../../../../skills", __dir__)
        end
      end
    end
  end
end
