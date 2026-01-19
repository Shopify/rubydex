class Author < ApplicationRecord
end

class Comment < ApplicationRecord
  belongs_to :post
end

class Post < ApplicationRecord
  belongs_to :author
  has_many :comments
  has_one :featured_image

  # Edge cases:
  Foo.belongs_to :bar       # has explicit receiver "Foo" - invalid
  belongs_to                # no arguments - invalid (no name)
  self.has_many :things     # self receiver - valid
end
