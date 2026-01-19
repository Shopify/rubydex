# frozen_string_literal: true

require "test_helper"

class PrototypePluginsTest < Minitest::Test
  #############################################################################
  # Scenario 1: ActiveRecord Associations Plugin
  # Demonstrates: DSL capture + add_method Stage 1 API
  # Verifies: Query graph["Post"].members to see synthetic methods
  #############################################################################

  def test_activerecord_belongs_to_creates_methods
    graph = Rubydex::Graph.new
    graph.capture_dsl_methods(["belongs_to", "has_many", "has_one"])
    graph.index_all([fixture_path("activerecord_example.rb")])
    graph.resolve

    file_content = File.read(fixture_path("activerecord_example.rb"))

    # Process DSL events and ACTUALLY add methods
    graph.each_dsl_file do |file_path, events|
      clean_path = file_path.sub(%r{^file://}, "")

      events.each do |event|
        # Skip calls with explicit receivers (but allow self)
        next if event.receiver && event.receiver != "self"

        # Get owner directly from nesting_stack (innermost class/module)
        owner = event.nesting_stack.last
        next unless owner

        line = byte_offset_to_line(file_content, event.offset)

        case event.method_name
        when "belongs_to", "has_one", "has_many"
          name = event.arguments.first
          next unless name

          # Actually call add_method to create getter and setter
          graph.add_method(owner: owner, name: name, file_path: clean_path, line: line, column: 0)
          graph.add_method(owner: owner, name: "#{name}=", file_path: clean_path, line: line, column: 0)
        end
      end
    end

    # Re-resolve to process synthetic methods
    graph.resolve

    # VERIFY by querying the graph
    post = graph["Post"]
    refute_nil(post, "Post should exist in graph")

    member_names = post.members.map(&:unqualified_name)

    # belongs_to :author -> author, author=
    assert_includes(member_names, "author", "Post should have author method")
    assert_includes(member_names, "author=", "Post should have author= method")

    # has_many :comments -> comments, comments=
    assert_includes(member_names, "comments", "Post should have comments method")
    assert_includes(member_names, "comments=", "Post should have comments= method")

    # has_one :featured_image -> featured_image, featured_image=
    assert_includes(member_names, "featured_image", "Post should have featured_image method")
    assert_includes(member_names, "featured_image=", "Post should have featured_image= method")

    # Verify Comment has belongs_to :post
    comment = graph["Comment"]
    refute_nil(comment, "Comment should exist in graph")
    comment_members = comment.members.map(&:unqualified_name)
    assert_includes(comment_members, "post", "Comment should have post method")
    assert_includes(comment_members, "post=", "Comment should have post= method")

    # Verify self.has_many :things IS included (self receiver is valid)
    assert_includes(member_names, "things", "Post should have things from self.has_many")
    assert_includes(member_names, "things=", "Post should have things= from self.has_many")

    # Verify Foo.belongs_to :bar is EXCLUDED (explicit receiver is invalid)
    refute_includes(member_names, "bar", "Foo.belongs_to :bar should be excluded")
    refute_includes(member_names, "bar=", "Foo.belongs_to :bar should be excluded")
  end

  #############################################################################
  # Scenario 2: ActiveSupport::Concern class_methods Plugin
  # Demonstrates: DSL capture with blocks + add_module + add_method
  # Verifies: Query graph["Taggable::ClassMethods"].members
  #############################################################################

  def test_concern_class_methods_creates_module_with_methods
    graph = Rubydex::Graph.new
    graph.capture_dsl_methods(["class_methods"])
    graph.index_all([fixture_path("concern_example.rb")])
    graph.resolve

    file_content = File.read(fixture_path("concern_example.rb"))

    # Collect modules to create
    modules_to_create = []

    # First pass: Create modules
    graph.each_dsl_file do |file_path, events|
      clean_path = file_path.sub(%r{^file://}, "")

      events.each do |event|
        next unless event.method_name == "class_methods" && event.has_block
        # Skip calls with explicit receivers (but allow self)
        next if event.receiver && event.receiver != "self"

        # Get owner directly from nesting_stack
        owner = event.nesting_stack.last
        next unless owner

        line = byte_offset_to_line(file_content, event.offset)
        class_methods_module = "#{owner}::ClassMethods"
        graph.add_module(name: class_methods_module, file_path: clean_path, line: line, column: 0)
        modules_to_create << { module_name: class_methods_module, file_path: clean_path, line: line }
      end
    end

    # Resolve to create declarations for new modules
    graph.resolve

    # Second pass: Add methods to the newly created modules
    modules_to_create.each do |mod|
      graph.add_method(owner: mod[:module_name], name: "find_by_tag", file_path: mod[:file_path], line: mod[:line], column: 0)
      graph.add_method(owner: mod[:module_name], name: "popular_tags", file_path: mod[:file_path], line: mod[:line], column: 0)
    end

    # Final resolve
    graph.resolve

    # VERIFY by querying the graph
    class_methods = graph["Taggable::ClassMethods"]
    refute_nil(class_methods, "Taggable::ClassMethods should exist in graph")

    cm_members = class_methods.members.map(&:unqualified_name)
    assert_includes(cm_members, "find_by_tag", "ClassMethods should have find_by_tag")
    assert_includes(cm_members, "popular_tags", "ClassMethods should have popular_tags")
  end

  #############################################################################
  # Scenario 3: RSpec DSL Plugin
  # Demonstrates: Nested DSL blocks with parent_id + add_class + add_method
  # Verifies: Query graph["RSpec::ExampleGroups::Calculator"].members
  #############################################################################

  def test_rspec_creates_example_groups_with_let_methods
    graph = Rubydex::Graph.new
    graph.capture_dsl_methods(["describe", "context", "let", "subject"])
    graph.index_all([fixture_path("rspec_example.rb")])
    graph.resolve

    file_content = File.read(fixture_path("rspec_example.rb"))

    # Build mapping from event_id to class name for tracking nesting
    event_to_class = {}
    base_class = "RSpec::ExampleGroups"
    methods_to_add = []

    graph.each_dsl_file do |file_path, events|
      clean_path = file_path.sub(%r{^file://}, "")

      # First pass: Create all classes
      events.each do |event|
        line = byte_offset_to_line(file_content, event.offset)

        case event.method_name
        when "describe"
          # Allow no receiver or RSpec receiver
          next if event.receiver && event.receiver != "RSpec"

          parent_class = if event.parent_id
            event_to_class[event.parent_id] || base_class
          else
            base_class
          end

          desc = event.arguments.first || "Anonymous"
          class_suffix = sanitize_name(desc)
          class_name = "#{parent_class}::#{class_suffix}"

          graph.add_class(name: class_name, parent: nil, file_path: clean_path, line: line, column: 0)
          event_to_class[event.id] = class_name

        when "context"
          # Allow no receiver or self
          next if event.receiver && event.receiver != "self"

          parent_class = if event.parent_id
            event_to_class[event.parent_id] || base_class
          else
            base_class
          end

          desc = event.arguments.first || "Anonymous"
          class_suffix = sanitize_name(desc)
          class_name = "#{parent_class}::#{class_suffix}"

          graph.add_class(name: class_name, parent: nil, file_path: clean_path, line: line, column: 0)
          event_to_class[event.id] = class_name

        when "let"
          # Allow no receiver or self
          next if event.receiver && event.receiver != "self"

          parent_class = event_to_class[event.parent_id]
          next unless parent_class

          method_name = event.arguments.first
          next unless method_name

          methods_to_add << { owner: parent_class, name: method_name, file_path: clean_path, line: line }

        when "subject"
          # Allow no receiver or self
          next if event.receiver && event.receiver != "self"

          parent_class = event_to_class[event.parent_id]
          next unless parent_class && event.has_block

          methods_to_add << { owner: parent_class, name: "subject", file_path: clean_path, line: line }
        end
      end
    end

    # Resolve to create declarations for new classes
    graph.resolve

    # Second pass: Add methods to the newly created classes
    methods_to_add.each do |m|
      graph.add_method(owner: m[:owner], name: m[:name], file_path: m[:file_path], line: m[:line], column: 0)
    end

    # Final resolve
    graph.resolve

    # VERIFY by querying the graph
    calc_group = graph["RSpec::ExampleGroups::Calculator"]
    refute_nil(calc_group, "Calculator example group should exist")

    calc_members = calc_group.members.map(&:unqualified_name)
    assert_includes(calc_members, "subject", "Calculator should have subject method")
    assert_includes(calc_members, "initial_value", "Calculator should have initial_value from let")

    # Verify nested describe #add
    add_group = graph["RSpec::ExampleGroups::Calculator::Add"]
    refute_nil(add_group, "Calculator::Add example group should exist")

    add_members = add_group.members.map(&:unqualified_name)
    assert_includes(add_members, "amount", "Add should have amount from let")

    # Verify context creates nested class
    negative_group = graph["RSpec::ExampleGroups::Calculator::Add::WithNegativeNumbers"]
    refute_nil(negative_group, "WithNegativeNumbers context should exist")

    negative_members = negative_group.members.map(&:unqualified_name)
    assert_includes(negative_members, "amount", "Context should shadow parent's let")

    # Verify sibling describe #multiply
    multiply_group = graph["RSpec::ExampleGroups::Calculator::Multiply"]
    refute_nil(multiply_group, "Multiply example group should exist")

    multiply_members = multiply_group.members.map(&:unqualified_name)
    assert_includes(multiply_members, "factor", "Multiply should have factor from let")

    # Verify Foo.context "invalid" is EXCLUDED (explicit receiver is invalid)
    invalid_group = graph["RSpec::ExampleGroups::Calculator::Invalid"]
    assert_nil(invalid_group, "Foo.context 'invalid' should be excluded")

    # Verify Foo.let(:invalid) is EXCLUDED (explicit receiver is invalid)
    refute_includes(calc_members, "invalid", "Foo.let(:invalid) should be excluded")
  end

  #############################################################################
  # Combined: All Scenarios Together
  #############################################################################

  def test_all_plugins_combined
    graph = Rubydex::Graph.new

    # Enable all DSL capture
    graph.capture_dsl_methods([
      "belongs_to",
      "has_many",
      "has_one",
      "class_methods",
      "describe",
      "context",
      "let",
      "subject",
    ])

    # Index all fixtures
    fixtures = [
      fixture_path("activerecord_example.rb"),
      fixture_path("concern_example.rb"),
      fixture_path("rspec_example.rb"),
    ]
    graph.index_all(fixtures)
    graph.resolve

    # Read file contents for offset->line conversion
    file_contents = fixtures.map { |f| [f, File.read(f)] }.to_h

    event_to_class = {}
    base_class = "RSpec::ExampleGroups"
    modules_to_create = []
    methods_to_add = []

    # First pass: Create classes and modules, collect methods to add
    graph.each_dsl_file do |file_path, events|
      clean_path = file_path.sub(%r{^file://}, "")
      file_content = file_contents[clean_path]

      events.each do |event|
        line = file_content ? byte_offset_to_line(file_content, event.offset) : 1

        case event.method_name
        # ActiveRecord - methods can be added directly (owner already exists)
        when "belongs_to", "has_one", "has_many"
          next if event.receiver && event.receiver != "self"

          owner = event.nesting_stack.last
          next unless owner

          name = event.arguments.first
          next unless name

          methods_to_add << { owner: owner, name: name, file_path: clean_path, line: line }
          methods_to_add << { owner: owner, name: "#{name}=", file_path: clean_path, line: line }

        # Concern - create module first
        when "class_methods"
          next if event.receiver && event.receiver != "self"

          owner = event.nesting_stack.last
          next unless owner && event.has_block

          class_methods_module = "#{owner}::ClassMethods"
          graph.add_module(name: class_methods_module, file_path: clean_path, line: line, column: 0)
          modules_to_create << { module_name: class_methods_module, file_path: clean_path, line: line }

        # RSpec - create classes first
        when "describe"
          next if event.receiver && event.receiver != "RSpec"

          parent_class = event.parent_id ? (event_to_class[event.parent_id] || base_class) : base_class
          desc = event.arguments.first || "Anonymous"
          class_name = "#{parent_class}::#{sanitize_name(desc)}"
          graph.add_class(name: class_name, parent: nil, file_path: clean_path, line: line, column: 0)
          event_to_class[event.id] = class_name

        when "context"
          next if event.receiver && event.receiver != "self"

          parent_class = event.parent_id ? (event_to_class[event.parent_id] || base_class) : base_class
          desc = event.arguments.first || "Anonymous"
          class_name = "#{parent_class}::#{sanitize_name(desc)}"
          graph.add_class(name: class_name, parent: nil, file_path: clean_path, line: line, column: 0)
          event_to_class[event.id] = class_name

        when "let"
          next if event.receiver && event.receiver != "self"

          parent_class = event_to_class[event.parent_id]
          next unless parent_class

          method_name = event.arguments.first
          next unless method_name

          methods_to_add << { owner: parent_class, name: method_name, file_path: clean_path, line: line }

        when "subject"
          next if event.receiver && event.receiver != "self"

          parent_class = event_to_class[event.parent_id]
          next unless parent_class && event.has_block

          methods_to_add << { owner: parent_class, name: "subject", file_path: clean_path, line: line }
        end
      end
    end

    # Resolve to create declarations for new classes/modules
    graph.resolve

    # Second pass: Add methods (including to newly created modules)
    modules_to_create.each do |mod|
      methods_to_add << { owner: mod[:module_name], name: "find_by_tag", file_path: mod[:file_path], line: mod[:line] }
      methods_to_add << { owner: mod[:module_name], name: "popular_tags", file_path: mod[:file_path], line: mod[:line] }
    end

    methods_to_add.each do |m|
      graph.add_method(owner: m[:owner], name: m[:name], file_path: m[:file_path], line: m[:line], column: 0)
    end

    graph.resolve

    # VERIFY all scenarios by querying graph
    # ActiveRecord
    post = graph["Post"]
    refute_nil(post, "Post should exist")
    assert(post.members.map(&:unqualified_name).include?("author"), "Post should have author")

    # Concern
    class_methods = graph["Taggable::ClassMethods"]
    refute_nil(class_methods, "Taggable::ClassMethods should exist")
    assert(class_methods.members.map(&:unqualified_name).include?("find_by_tag"), "ClassMethods should have find_by_tag")

    # RSpec
    calc = graph["RSpec::ExampleGroups::Calculator"]
    refute_nil(calc, "Calculator example group should exist")
    assert(calc.members.map(&:unqualified_name).include?("subject"), "Calculator should have subject")
  end

  private

  def fixture_path(name)
    File.expand_path("fixtures/#{name}", __dir__)
  end

  def sanitize_name(description)
    description
      .to_s
      .gsub(/^#/, "")
      .gsub(/[^a-zA-Z0-9\s]/, "")
      .split(/\s+/)
      .map(&:capitalize)
      .join
  end

  # Convert byte offset to line number (1-indexed)
  def byte_offset_to_line(content, offset)
    content[0...offset].count("\n") + 1
  end
end
