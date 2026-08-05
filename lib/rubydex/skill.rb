# frozen_string_literal: true

require "rubydex/errors"

module Rubydex
  # A skill file loaded from disk. Skill files are Markdown documents with YAML frontmatter
  # that declare a `name` and `description`. The body is the Markdown content after the frontmatter.
  class Skill
    #: String
    attr_reader :name

    #: String
    attr_reader :description

    #: String
    attr_reader :body

    class << self
      #: (String path) -> Rubydex::Skill
      def load(path)
        from_content(File.read(path), path:)
      rescue SystemCallError, IOError => e
        raise SkillError, "Cannot read skill file #{path}: #{e.message}"
      end

      #: (String content, ?path: String?) -> Rubydex::Skill
      def from_content(content, path: nil)
        new(**parse(content, path:))
      end

      private

      #: (String content, path: String?) -> { name: String, description: String, body: String }
      def parse(content, path:)
        lines = content.lines

        unless lines.first&.strip == "---"
          raise SkillError, "Missing YAML frontmatter delimiter#{location(path)}"
        end

        closing_index = (1...lines.length).find { |i| lines[i].strip == "---" }
        unless closing_index
          raise SkillError, "Unterminated YAML frontmatter#{location(path)}"
        end

        frontmatter = lines[1...closing_index].join
        body = lines[(closing_index + 1)..].join.strip

        yaml = parse_frontmatter(frontmatter, path)
        unless yaml.is_a?(Hash)
          raise SkillError, "Empty or malformed YAML frontmatter#{location(path)}"
        end

        name = yaml["name"]
        unless name.is_a?(String)
          raise SkillError, "Missing or non-string `name` in frontmatter#{location(path)}"
        end

        description = yaml["description"]
        unless description.is_a?(String)
          raise SkillError, "Missing or non-string `description` in frontmatter#{location(path)}"
        end

        { name:, description:, body: }
      end

      #: (String frontmatter, String? path) -> untyped
      def parse_frontmatter(frontmatter, path)
        YAML.safe_load(frontmatter)
      rescue Psych::Exception => e
        raise SkillError, "Malformed YAML frontmatter#{location(path)}: #{e.message}"
      end

      #: (String?) -> String
      def location(path)
        path ? " in #{path}" : ""
      end
    end

    #: (name: String, description: String, body: String) -> void
    def initialize(name:, description:, body:)
      @name = name.freeze
      @description = description.freeze
      @body = body.freeze
      freeze
    end
  end
end
