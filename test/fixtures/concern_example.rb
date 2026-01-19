module Taggable
  extend ActiveSupport::Concern

  included do
    has_many :taggings
  end

  class_methods do
    def find_by_tag(tag)
      # implementation
    end

    def popular_tags
      # implementation
    end
  end

  def tags
    taggings.map(&:tag)
  end
end

class Article
  include Taggable
end
