# frozen_string_literal: true

module Rubydex
  # A reference to a constant name, which includes its nesting and parent scope
  #
  # Example
  # ```ruby
  # # NameRef("Foo", nesting: nil, parent_scope: nil)
  # module Foo
  #   # NameRef("Baz",
  #   #   nesting: NameRef("Foo", nesting: nil, parent_scope: nil),
  #   #   parent_scope: NameRef(
  #   #     "Bar",
  #   #     nesting: NameRef("Foo", nesting: nil, parent_scope: nil),
  #   #     parent_scope: nil
  #   #   )
  #   # )
  #   class Bar::Baz
  #   end
  # end
  # ```
  class NameRef
    #: NameRef?
    attr_reader :nesting, :parent_scope

    #: String
    attr_reader :str

    #: (String, ?nesting: NameRef?, ?parent_scope: NameRef?) -> void
    def initialize(str, nesting: nil, parent_scope: nil)
      @str = str
      @nesting = nesting
      @parent_scope = parent_scope
    end
  end
end
