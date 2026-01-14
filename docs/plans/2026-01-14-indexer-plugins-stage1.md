# Indexer Plugins - Stage 1: Ruby API Foundation

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add Ruby APIs to insert method/class definitions and query class members.

**Architecture:** Extend the existing FFI pipeline (Rust → C → Ruby) with new functions for inserting definitions and querying members. Low-level API style matching Rust internals.

**Tech Stack:** Rust (rubydex, rubydex-sys), C extension, Ruby

---

## Task 1: Expose Declaration#members in Ruby

Add a method to query the members (methods, constants, etc.) owned by a class/module declaration.

**Files:**
- Modify: `rust/rubydex-sys/src/declaration_api.rs`
- Modify: `ext/rubydex/declaration.c`
- Create: `test/declaration_members_test.rb`

**Step 1: Write the failing Ruby test**

```ruby
# test/declaration_members_test.rb
# frozen_string_literal: true

require "test_helper"

class DeclarationMembersTest < Minitest::Test
  def setup
    @graph = Rubydex::Graph.new
  end

  def test_members_returns_enumerator
    @graph.index_all([fixture_path("post_with_method.rb")])
    @graph.resolve

    post = @graph["Post"]
    assert_respond_to post, :members
    assert_kind_of Enumerator, post.members
  end

  def test_members_yields_member_declarations
    @graph.index_all([fixture_path("post_with_method.rb")])
    @graph.resolve

    post = @graph["Post"]
    member_names = post.members.map(&:unqualified_name)
    assert_includes member_names, "save"
  end

  private

  def fixture_path(name)
    File.expand_path("fixtures/#{name}", __dir__)
  end
end
```

**Step 2: Create the test fixture**

```ruby
# test/fixtures/post_with_method.rb
class Post
  def save
    true
  end
end
```

**Step 3: Run test to verify it fails**

Run: `chruby 3.4.3 && bundle exec ruby -Itest test/declaration_members_test.rb`
Expected: FAIL with "undefined method `members'"

**Step 4: Add Rust FFI function for members iterator**

```rust
// rust/rubydex-sys/src/declaration_api.rs (add after existing code)

/// Creates a new iterator over member declaration IDs for a given namespace declaration.
/// Returns an iterator of (declaration_id, kind) pairs for each member.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer`
/// - The returned pointer must be freed with `rdx_members_iter_free`
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_declaration_members_iter_new(
    pointer: GraphPointer,
    decl_id: i64,
) -> *mut MembersIter {
    with_graph(pointer, |graph| {
        let decl_id = DeclarationId::new(decl_id);
        if let Some(decl) = graph.declarations().get(&decl_id) {
            let members: Vec<i64> = match decl {
                rubydex::model::declaration::Declaration::Class(c) => {
                    c.members().values().map(|id| **id).collect()
                }
                rubydex::model::declaration::Declaration::Module(m) => {
                    m.members().values().map(|id| **id).collect()
                }
                rubydex::model::declaration::Declaration::SingletonClass(s) => {
                    s.members().values().map(|id| **id).collect()
                }
                _ => Vec::new(),
            };
            Box::into_raw(Box::new(MembersIter {
                ids: members.into_boxed_slice(),
                index: 0,
            }))
        } else {
            Box::into_raw(Box::new(MembersIter {
                ids: Vec::new().into_boxed_slice(),
                index: 0,
            }))
        }
    })
}

/// Iterator over member declaration IDs
#[derive(Debug)]
pub struct MembersIter {
    ids: Box<[i64]>,
    index: usize,
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_members_iter_next(iter: *mut MembersIter, out_id: *mut i64) -> bool {
    if iter.is_null() || out_id.is_null() {
        return false;
    }
    let it = unsafe { &mut *iter };
    if it.index >= it.ids.len() {
        return false;
    }
    unsafe { *out_id = it.ids[it.index] };
    it.index += 1;
    true
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_members_iter_len(iter: *const MembersIter) -> usize {
    if iter.is_null() { 0 } else { unsafe { (*iter).ids.len() } }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_members_iter_free(iter: *mut MembersIter) {
    if !iter.is_null() {
        unsafe { let _ = Box::from_raw(iter); }
    }
}
```

**Step 5: Add C bindings for members**

```c
// ext/rubydex/declaration.c (add to existing file)

// Body function for rb_ensure in Declaration#members
static VALUE declaration_members_yield(VALUE args) {
    VALUE self = rb_ary_entry(args, 0);
    void *iter = (void *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));

    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    int64_t id = 0;
    while (rdx_members_iter_next(iter, &id)) {
        VALUE argv[] = {data->graph_obj, LL2NUM(id)};
        VALUE handle = rb_class_new_instance(2, argv, cDeclaration);
        rb_yield(handle);
    }

    return Qnil;
}

// Ensure function for rb_ensure in Declaration#members
static VALUE declaration_members_ensure(VALUE args) {
    void *iter = (void *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));
    rdx_members_iter_free(iter);
    return Qnil;
}

// Size function for Declaration#members enumerator
static VALUE declaration_members_size(VALUE self, VALUE _args, VALUE _eobj) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);
    void *iter = rdx_declaration_members_iter_new(graph, data->id);
    size_t len = rdx_members_iter_len(iter);
    rdx_members_iter_free(iter);

    return SIZET2NUM(len);
}

// Declaration#members: () -> Enumerator[Declaration]
static VALUE sr_declaration_members(VALUE self) {
    if (!rb_block_given_p()) {
        return rb_enumeratorize_with_size(self, rb_str_new2("members"), 0, NULL, declaration_members_size);
    }

    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    void *iter = rdx_declaration_members_iter_new(graph, data->id);
    VALUE args = rb_ary_new_from_args(2, self, ULL2NUM((uintptr_t)iter));
    rb_ensure(declaration_members_yield, args, declaration_members_ensure, args);

    return self;
}
```

**Step 6: Register the members method**

```c
// ext/rubydex/declaration.c - in initialize_declaration function, add:
rb_define_method(cDeclaration, "members", sr_declaration_members, 0);
```

**Step 7: Update rustbindings.h**

```c
// ext/rubydex/rustbindings.h (add declarations)
typedef struct MembersIter MembersIter;
MembersIter *rdx_declaration_members_iter_new(void *graph, int64_t decl_id);
bool rdx_members_iter_next(MembersIter *iter, int64_t *out_id);
size_t rdx_members_iter_len(const MembersIter *iter);
void rdx_members_iter_free(MembersIter *iter);
```

**Step 8: Compile and run tests**

Run: `chruby 3.4.3 && bundle exec rake compile && bundle exec ruby -Itest test/declaration_members_test.rb`
Expected: PASS

**Step 9: Commit**

```bash
git add rust/rubydex-sys/src/declaration_api.rs ext/rubydex/declaration.c ext/rubydex/rustbindings.h test/declaration_members_test.rb test/fixtures/post_with_method.rb
git commit -m "feat: add Declaration#members to query class/module members"
```

---

## Task 2: Add Graph#add_method for inserting method definitions

Add ability to insert synthetic method definitions from Ruby.

**Files:**
- Modify: `rust/rubydex-sys/src/graph_api.rs`
- Modify: `ext/rubydex/graph.c`
- Modify: `ext/rubydex/rustbindings.h`
- Create: `test/graph_add_method_test.rb`

**Step 1: Write the failing Ruby test**

```ruby
# test/graph_add_method_test.rb
# frozen_string_literal: true

require "test_helper"

class GraphAddMethodTest < Minitest::Test
  def setup
    @graph = Rubydex::Graph.new
  end

  def test_add_method_creates_method_declaration
    @graph.index_all([fixture_path("empty_post.rb")])
    @graph.resolve

    # Add synthetic method
    @graph.add_method(
      owner: "Post",
      name: "author",
      file_path: fixture_path("empty_post.rb"),
      line: 2,
      column: 2
    )

    # Re-resolve to pick up the new method
    @graph.resolve

    # Verify the method exists
    post = @graph["Post"]
    member_names = post.members.map(&:unqualified_name)
    assert_includes member_names, "author"
  end

  def test_add_method_with_setter
    @graph.index_all([fixture_path("empty_post.rb")])
    @graph.resolve

    @graph.add_method(owner: "Post", name: "author", file_path: fixture_path("empty_post.rb"), line: 2, column: 2)
    @graph.add_method(owner: "Post", name: "author=", file_path: fixture_path("empty_post.rb"), line: 2, column: 2)
    @graph.resolve

    post = @graph["Post"]
    member_names = post.members.map(&:unqualified_name)
    assert_includes member_names, "author"
    assert_includes member_names, "author="
  end

  private

  def fixture_path(name)
    File.expand_path("fixtures/#{name}", __dir__)
  end
end
```

**Step 2: Create the test fixture**

```ruby
# test/fixtures/empty_post.rb
class Post
end
```

**Step 3: Run test to verify it fails**

Run: `chruby 3.4.3 && bundle exec ruby -Itest test/graph_add_method_test.rb`
Expected: FAIL with "undefined method `add_method'"

**Step 4: Add Rust function to insert method definition**

```rust
// rust/rubydex-sys/src/graph_api.rs (add after existing functions)

use rubydex::model::definitions::{Definition, MethodDefinition};
use rubydex::model::ids::UriId;
use rubydex::offset::Offset;

/// Adds a synthetic method definition to the graph.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer`
/// - `owner_name`, `method_name`, `file_path` must be valid UTF-8 C strings
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_add_method(
    pointer: GraphPointer,
    owner_name: *const c_char,
    method_name: *const c_char,
    file_path: *const c_char,
    line: u32,
    column: u32,
) -> bool {
    let Ok(owner) = (unsafe { utils::convert_char_ptr_to_string(owner_name) }) else {
        return false;
    };
    let Ok(name) = (unsafe { utils::convert_char_ptr_to_string(method_name) }) else {
        return false;
    };
    let Ok(path) = (unsafe { utils::convert_char_ptr_to_string(file_path) }) else {
        return false;
    };

    with_graph(pointer, |graph| {
        // Create a synthetic method definition
        let uri_id = UriId::from(path.as_str());

        // We need to find or create the owner declaration first
        let owner_decl_id = DeclarationId::from(owner.as_str());

        // Check owner exists
        if !graph.declarations().contains_key(&owner_decl_id) {
            return false;
        }

        // Create synthetic offset (line/column based)
        let start = (line - 1) * 1000 + column; // Rough byte offset approximation
        let offset = Offset::new(start, start + name.len() as u32, line, column);

        // Add the synthetic method definition
        graph.add_synthetic_method(&owner, &name, uri_id, offset);
        true
    })
}
```

**Step 5: Add Graph method in Rust for synthetic methods**

```rust
// rust/rubydex/src/model/graph.rs (add method to Graph impl)

/// Adds a synthetic method definition created by plugins
pub fn add_synthetic_method(&mut self, owner_name: &str, method_name: &str, uri_id: UriId, offset: Offset) {
    use crate::model::definitions::{Definition, MethodDefinition};
    use crate::model::ids::{DefinitionId, StringId};
    use crate::model::visibility::Visibility;

    let name_str_id = self.intern_string(method_name.to_string());
    let fqn = format!("{}#{}", owner_name, method_name);

    // Create definition ID from synthetic location
    let def_id = DefinitionId::synthetic(&fqn, *uri_id);

    // Create the method definition
    let definition = Definition::Method(MethodDefinition::new(
        name_str_id,
        offset,
        Vec::new(), // no comments
        crate::model::definitions::DefinitionFlags::SYNTHETIC,
        None, // no owner_id yet - will be set during resolution
        uri_id,
        Visibility::Public,
        Vec::new(), // no parameters
    ));

    self.definitions_mut().insert(def_id, definition);

    // Mark as needing re-resolution
    self.synthetic_definitions.push((def_id, owner_name.to_string()));
}
```

**Step 6: Add C binding for add_method**

```c
// ext/rubydex/graph.c (add after sr_graph_resolve)

// Graph#add_method: (owner:, name:, file_path:, line:, column:) -> bool
static VALUE sr_graph_add_method(int argc, VALUE *argv, VALUE self) {
    VALUE kwargs;
    rb_scan_args(argc, argv, ":", &kwargs);

    VALUE owner = rb_hash_aref(kwargs, ID2SYM(rb_intern("owner")));
    VALUE name = rb_hash_aref(kwargs, ID2SYM(rb_intern("name")));
    VALUE file_path = rb_hash_aref(kwargs, ID2SYM(rb_intern("file_path")));
    VALUE line = rb_hash_aref(kwargs, ID2SYM(rb_intern("line")));
    VALUE column = rb_hash_aref(kwargs, ID2SYM(rb_intern("column")));

    if (NIL_P(owner) || NIL_P(name) || NIL_P(file_path) || NIL_P(line) || NIL_P(column)) {
        rb_raise(rb_eArgError, "missing required keyword arguments");
    }

    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    bool success = rdx_graph_add_method(
        graph,
        StringValueCStr(owner),
        StringValueCStr(name),
        StringValueCStr(file_path),
        NUM2UINT(line),
        NUM2UINT(column)
    );

    return success ? Qtrue : Qfalse;
}
```

**Step 7: Register the add_method function**

```c
// ext/rubydex/graph.c - in initialize_graph function, add:
rb_define_method(cGraph, "add_method", sr_graph_add_method, -1);
```

**Step 8: Update rustbindings.h**

```c
// ext/rubydex/rustbindings.h (add declaration)
bool rdx_graph_add_method(void *graph, const char *owner_name, const char *method_name, const char *file_path, uint32_t line, uint32_t column);
```

**Step 9: Compile and run tests**

Run: `chruby 3.4.3 && bundle exec rake compile && bundle exec ruby -Itest test/graph_add_method_test.rb`
Expected: PASS

**Step 10: Commit**

```bash
git add rust/rubydex-sys/src/graph_api.rs rust/rubydex/src/model/graph.rs ext/rubydex/graph.c ext/rubydex/rustbindings.h test/graph_add_method_test.rb test/fixtures/empty_post.rb
git commit -m "feat: add Graph#add_method for synthetic method definitions"
```

---

## Task 3: Add Graph#add_class for inserting class definitions

Add ability to insert synthetic class definitions (needed for RSpec anonymous classes).

**Files:**
- Modify: `rust/rubydex-sys/src/graph_api.rs`
- Modify: `rust/rubydex/src/model/graph.rs`
- Modify: `ext/rubydex/graph.c`
- Modify: `ext/rubydex/rustbindings.h`
- Create: `test/graph_add_class_test.rb`

**Step 1: Write the failing Ruby test**

```ruby
# test/graph_add_class_test.rb
# frozen_string_literal: true

require "test_helper"

class GraphAddClassTest < Minitest::Test
  def setup
    @graph = Rubydex::Graph.new
  end

  def test_add_class_creates_class_declaration
    @graph.index_all([fixture_path("rspec_example.rb")])
    @graph.resolve

    # Add synthetic class for RSpec describe block
    @graph.add_class(
      name: "RSpec::ExampleGroups::Calculator",
      parent: "RSpec::Core::ExampleGroup",
      file_path: fixture_path("rspec_example.rb"),
      line: 1,
      column: 0
    )
    @graph.resolve

    # Verify the class exists
    calc_class = @graph["RSpec::ExampleGroups::Calculator"]
    refute_nil calc_class
    assert_equal "Calculator", calc_class.unqualified_name
  end

  def test_add_class_with_nested_context
    @graph.index_all([fixture_path("rspec_example.rb")])
    @graph.resolve

    @graph.add_class(
      name: "RSpec::ExampleGroups::Calculator",
      parent: "RSpec::Core::ExampleGroup",
      file_path: fixture_path("rspec_example.rb"),
      line: 1,
      column: 0
    )
    @graph.add_class(
      name: "RSpec::ExampleGroups::Calculator::WhenAdding",
      parent: "RSpec::ExampleGroups::Calculator",
      file_path: fixture_path("rspec_example.rb"),
      line: 5,
      column: 2
    )
    @graph.resolve

    nested = @graph["RSpec::ExampleGroups::Calculator::WhenAdding"]
    refute_nil nested
    assert_equal "WhenAdding", nested.unqualified_name
  end

  private

  def fixture_path(name)
    File.expand_path("fixtures/#{name}", __dir__)
  end
end
```

**Step 2: Create the test fixture**

```ruby
# test/fixtures/rspec_example.rb
RSpec.describe "Calculator" do
  subject { Object.new }
  let(:value) { 42 }

  context "when adding" do
    let(:other) { 10 }
  end
end
```

**Step 3: Run test to verify it fails**

Run: `chruby 3.4.3 && bundle exec ruby -Itest test/graph_add_class_test.rb`
Expected: FAIL with "undefined method `add_class'"

**Step 4: Add Rust function to insert class definition**

```rust
// rust/rubydex-sys/src/graph_api.rs (add after rdx_graph_add_method)

/// Adds a synthetic class definition to the graph.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer`
/// - `class_name`, `parent_name`, `file_path` must be valid UTF-8 C strings
/// - `parent_name` can be null for classes without explicit parent
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_add_class(
    pointer: GraphPointer,
    class_name: *const c_char,
    parent_name: *const c_char,
    file_path: *const c_char,
    line: u32,
    column: u32,
) -> bool {
    let Ok(name) = (unsafe { utils::convert_char_ptr_to_string(class_name) }) else {
        return false;
    };
    let parent = if parent_name.is_null() {
        None
    } else {
        unsafe { utils::convert_char_ptr_to_string(parent_name).ok() }
    };
    let Ok(path) = (unsafe { utils::convert_char_ptr_to_string(file_path) }) else {
        return false;
    };

    with_graph(pointer, |graph| {
        let uri_id = UriId::from(path.as_str());
        let start = (line - 1) * 1000 + column;
        let offset = Offset::new(start, start + name.len() as u32, line, column);

        graph.add_synthetic_class(&name, parent.as_deref(), uri_id, offset);
        true
    })
}
```

**Step 5: Add Graph method for synthetic classes**

```rust
// rust/rubydex/src/model/graph.rs (add method to Graph impl)

/// Adds a synthetic class definition created by plugins
pub fn add_synthetic_class(
    &mut self,
    class_name: &str,
    parent_name: Option<&str>,
    uri_id: UriId,
    offset: Offset,
) {
    use crate::model::definitions::{ClassDefinition, Definition, DefinitionFlags};
    use crate::model::ids::{DefinitionId, NameId, StringId};

    // Extract unqualified name
    let unqualified = class_name.rsplit("::").next().unwrap_or(class_name);
    let name_str_id = self.intern_string(unqualified.to_string());

    // Create NameId for the class
    let name_id = NameId::from(class_name);

    // Create definition ID
    let def_id = DefinitionId::synthetic(class_name, *uri_id);

    // Create parent reference if provided
    let parent_name_id = parent_name.map(|p| {
        let parent_unqualified = p.rsplit("::").next().unwrap_or(p);
        let parent_str_id = self.intern_string(parent_unqualified.to_string());
        self.add_name(crate::model::name::Name::new(parent_str_id, None, None))
    });

    let definition = Definition::Class(ClassDefinition::new(
        name_str_id,
        name_id,
        offset,
        Vec::new(), // no comments
        DefinitionFlags::SYNTHETIC,
        None, // owner set during resolution
        uri_id,
        parent_name_id,
    ));

    self.definitions_mut().insert(def_id, definition);
    self.synthetic_classes.push(def_id);
}
```

**Step 6: Add C binding for add_class**

```c
// ext/rubydex/graph.c (add after sr_graph_add_method)

// Graph#add_class: (name:, parent:, file_path:, line:, column:) -> bool
static VALUE sr_graph_add_class(int argc, VALUE *argv, VALUE self) {
    VALUE kwargs;
    rb_scan_args(argc, argv, ":", &kwargs);

    VALUE name = rb_hash_aref(kwargs, ID2SYM(rb_intern("name")));
    VALUE parent = rb_hash_aref(kwargs, ID2SYM(rb_intern("parent")));
    VALUE file_path = rb_hash_aref(kwargs, ID2SYM(rb_intern("file_path")));
    VALUE line = rb_hash_aref(kwargs, ID2SYM(rb_intern("line")));
    VALUE column = rb_hash_aref(kwargs, ID2SYM(rb_intern("column")));

    if (NIL_P(name) || NIL_P(file_path) || NIL_P(line) || NIL_P(column)) {
        rb_raise(rb_eArgError, "missing required keyword arguments");
    }

    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    bool success = rdx_graph_add_class(
        graph,
        StringValueCStr(name),
        NIL_P(parent) ? NULL : StringValueCStr(parent),
        StringValueCStr(file_path),
        NUM2UINT(line),
        NUM2UINT(column)
    );

    return success ? Qtrue : Qfalse;
}
```

**Step 7: Register the add_class function**

```c
// ext/rubydex/graph.c - in initialize_graph function, add:
rb_define_method(cGraph, "add_class", sr_graph_add_class, -1);
```

**Step 8: Update rustbindings.h**

```c
// ext/rubydex/rustbindings.h (add declaration)
bool rdx_graph_add_class(void *graph, const char *class_name, const char *parent_name, const char *file_path, uint32_t line, uint32_t column);
```

**Step 9: Compile and run tests**

Run: `chruby 3.4.3 && bundle exec rake compile && bundle exec ruby -Itest test/graph_add_class_test.rb`
Expected: PASS

**Step 10: Commit**

```bash
git add rust/rubydex-sys/src/graph_api.rs rust/rubydex/src/model/graph.rs ext/rubydex/graph.c ext/rubydex/rustbindings.h test/graph_add_class_test.rb test/fixtures/rspec_example.rb
git commit -m "feat: add Graph#add_class for synthetic class definitions"
```

---

## Task 4: Integration test with belongs_to scenario

Verify the full flow works for the `belongs_to` use case.

**Files:**
- Create: `test/integration/belongs_to_plugin_test.rb`
- Create: `test/fixtures/belongs_to_example.rb`

**Step 1: Write the integration test**

```ruby
# test/integration/belongs_to_plugin_test.rb
# frozen_string_literal: true

require "test_helper"

class BelongsToPluginTest < Minitest::Test
  def setup
    @graph = Rubydex::Graph.new
  end

  def test_simulated_belongs_to_plugin
    # Index the file
    @graph.index_all([fixture_path("belongs_to_example.rb")])
    @graph.resolve

    # Simulate what a plugin would do when seeing `belongs_to :author`
    # The plugin would detect the DSL call and insert synthetic methods
    @graph.add_method(
      owner: "Post",
      name: "author",
      file_path: fixture_path("belongs_to_example.rb"),
      line: 2,
      column: 2
    )
    @graph.add_method(
      owner: "Post",
      name: "author=",
      file_path: fixture_path("belongs_to_example.rb"),
      line: 2,
      column: 2
    )

    # Re-resolve
    @graph.resolve

    # Verify Post now has the synthetic methods
    post = @graph["Post"]
    refute_nil post, "Post class should exist"

    member_names = post.members.map(&:unqualified_name)
    assert_includes member_names, "author", "Post should have author method"
    assert_includes member_names, "author=", "Post should have author= method"
  end

  private

  def fixture_path(name)
    File.expand_path("../fixtures/#{name}", __dir__)
  end
end
```

**Step 2: Create the test fixture**

```ruby
# test/fixtures/belongs_to_example.rb
class Post
  belongs_to :author
end
```

**Step 3: Run the integration test**

Run: `chruby 3.4.3 && bundle exec ruby -Itest test/integration/belongs_to_plugin_test.rb`
Expected: PASS

**Step 4: Commit**

```bash
git add test/integration/belongs_to_plugin_test.rb test/fixtures/belongs_to_example.rb
git commit -m "test: add integration test for belongs_to plugin scenario"
```

---

## Task 5: Run full test suite and cleanup

**Step 1: Run full test suite**

Run: `chruby 3.4.3 && bundle exec rake test`
Expected: All tests pass

**Step 2: Run linter**

Run: `chruby 3.4.3 && bundle exec rake lint`
Expected: No new lint errors

**Step 3: Final commit if any fixes needed**

```bash
git add -A
git commit -m "chore: cleanup and fix any lint issues"
```

---

## Notes for Implementation

1. **Synthetic definitions need special handling in resolution** - The `add_synthetic_method` and `add_synthetic_class` functions store definitions but they need proper resolution. You may need to add fields to `Graph` to track synthetic definitions and process them during `resolve_all`.

2. **DefinitionFlags::SYNTHETIC** - This flag may not exist yet. Add it to `rust/rubydex/src/model/definitions.rs` if needed.

3. **DefinitionId::synthetic** - This constructor may not exist. Add it to create deterministic IDs for synthetic definitions.

4. **The test assumes methods are visible via `Declaration#members`** - Make sure synthetic methods get properly linked to their owner during resolution.
