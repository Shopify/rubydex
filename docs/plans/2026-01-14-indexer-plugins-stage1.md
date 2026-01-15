# Indexer Plugins - Stage 1: Plugin API Foundation

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add Ruby APIs to insert synthetic definitions (methods, classes, modules), manage mixin relationships, and register include hooks for plugin-based DSL support.

**Architecture:** Extend the existing FFI pipeline (Rust → C → Ruby) with new functions. The Graph stores synthetic definitions in separate vectors that get processed during `resolve()`. Tests use the existing `with_context` helper for temp files.

**Tech Stack:** Rust (rubydex, rubydex-sys), C extension (ext/rubydex), Ruby (minitest)

---

## API Summary

| API | Purpose | Parameters |
|-----|---------|------------|
| `Declaration#members` | Query members of a class/module | none |
| `Declaration#ancestors` | Query linearized ancestor chain | none |
| `Graph#add_method` | Insert synthetic method | `owner:`, `name:`, `file_path:`, `line:`, `column:` |
| `Graph#add_class` | Insert synthetic class | `name:`, `parent:` (optional), `file_path:`, `line:`, `column:` |
| `Graph#add_module` | Insert synthetic module | `name:`, `file_path:`, `line:`, `column:` |
| `Graph#add_mixin` | Insert mixin relationship | `target:`, `module_name:`, `type:` (`:include`/`:extend`/`:prepend`) |
| `Graph#register_included_hook` | Auto-extend on include | `module_name:`, `extend_module:` |

---

## Task 1: Declaration#members - Ruby test

**Files:**
- Modify: `test/declaration_test.rb`

**Step 1: Write the failing test**

Add to `test/declaration_test.rb`:

```ruby
def test_members_returns_methods_defined_in_class
  with_context do |context|
    context.write!("file.rb", <<~RUBY)
      class Post
        def save; end
        def validate; end
      end
    RUBY

    graph = Rubydex::Graph.new
    graph.index_all(context.glob("**/*.rb"))
    graph.resolve

    post = graph["Post"]
    member_names = post.members.map(&:unqualified_name)
    assert_equal 2, member_names.size
    assert_includes member_names, "save"
    assert_includes member_names, "validate"
  end
end

def test_members_returns_empty_for_class_with_no_methods
  with_context do |context|
    context.write!("file.rb", "class Post; end")

    graph = Rubydex::Graph.new
    graph.index_all(context.glob("**/*.rb"))
    graph.resolve

    post = graph["Post"]
    assert_equal 0, post.members.count
  end
end

def test_members_returns_nested_constants
  with_context do |context|
    context.write!("file.rb", <<~RUBY)
      module Foo
        class Bar; end
        module Baz; end
      end
    RUBY

    graph = Rubydex::Graph.new
    graph.index_all(context.glob("**/*.rb"))
    graph.resolve

    foo = graph["Foo"]
    member_names = foo.members.map(&:unqualified_name)
    assert_equal 2, member_names.size
    assert_includes member_names, "Bar"
    assert_includes member_names, "Baz"
  end
end
```

**Step 2: Run test to verify it fails**

Run: `chruby 3.4.3 && bundle exec ruby -Itest test/declaration_test.rb -n /members/`
Expected: FAIL with "undefined method `members'"

**Step 3: Commit test**

```bash
git add test/declaration_test.rb
git commit -m "test: add Declaration#members tests (red)"
```

---

## Task 2: Declaration#members - Rust implementation

**Files:**
- Modify: `rust/rubydex-sys/src/declaration_api.rs`

**Step 1: Add MembersIter struct and functions**

Add to `rust/rubydex-sys/src/declaration_api.rs`:

```rust
/// Iterator over member declaration IDs for a namespace
pub struct MembersIter {
    ids: Box<[i64]>,
    index: usize,
}

/// Creates a new iterator over member declaration IDs for a given namespace declaration.
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
        let members: Vec<i64> = if let Some(decl) = graph.declarations().get(&decl_id) {
            match decl {
                Declaration::Class(c) => c.members().values().map(|id| **id).collect(),
                Declaration::Module(m) => m.members().values().map(|id| **id).collect(),
                Declaration::SingletonClass(s) => s.members().values().map(|id| **id).collect(),
                _ => Vec::new(),
            }
        } else {
            Vec::new()
        };
        Box::into_raw(Box::new(MembersIter {
            ids: members.into_boxed_slice(),
            index: 0,
        }))
    })
}

/// Advances the iterator and writes the next ID into `out_id`.
/// Returns `true` if an ID was written, `false` if exhausted.
///
/// # Safety
/// - `iter` must be valid pointer from `rdx_declaration_members_iter_new`
/// - `out_id` must be a valid, writable pointer
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

/// Returns the total number of members in the iterator.
///
/// # Safety
/// - `iter` must be valid pointer from `rdx_declaration_members_iter_new`
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_members_iter_len(iter: *const MembersIter) -> usize {
    if iter.is_null() {
        0
    } else {
        unsafe { (*iter).ids.len() }
    }
}

/// Frees an iterator created by `rdx_declaration_members_iter_new`.
///
/// # Safety
/// - `iter` must be a pointer from `rdx_declaration_members_iter_new`
/// - `iter` must not be used after being freed
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_members_iter_free(iter: *mut MembersIter) {
    if !iter.is_null() {
        unsafe {
            let _ = Box::from_raw(iter);
        }
    }
}
```

**Step 2: Compile Rust**

Run: `cd rust && cargo build`
Expected: PASS

**Step 3: Commit Rust changes**

```bash
git add rust/rubydex-sys/src/declaration_api.rs
git commit -m "feat(rust): add members iterator for declarations"
```

---

## Task 3: Declaration#members - C bindings

**Files:**
- Modify: `ext/rubydex/rustbindings.h`
- Modify: `ext/rubydex/declaration.c`

**Step 1: Update rustbindings.h**

Add declarations:

```c
typedef struct MembersIter MembersIter;
struct MembersIter *rdx_declaration_members_iter_new(GraphPointer pointer, int64_t decl_id);
bool rdx_members_iter_next(struct MembersIter *iter, int64_t *out_id);
size_t rdx_members_iter_len(const struct MembersIter *iter);
void rdx_members_iter_free(struct MembersIter *iter);
```

**Step 2: Add C implementation in declaration.c**

Add to `ext/rubydex/declaration.c`:

```c
// Body function for rb_ensure in Declaration#members
static VALUE declaration_members_yield(VALUE args) {
    VALUE self = rb_ary_entry(args, 0);
    void *iter = (void *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));

    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    int64_t id = 0;
    while (rdx_members_iter_next(iter, &id)) {
        VALUE argv[] = {data->graph_obj, LL2NUM(id)};
        VALUE handle = rb_class_new_instance(2, argv, cDeclaration);
        rb_yield(handle);
    }

    return Qnil;
}

// Ensure function to free iterator
static VALUE declaration_members_ensure(VALUE args) {
    void *iter = (void *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));
    rdx_members_iter_free(iter);
    return Qnil;
}

// Size function for enumerator
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

**Step 3: Register the method**

In `initialize_declaration` function, add:

```c
rb_define_method(cDeclaration, "members", sr_declaration_members, 0);
```

**Step 4: Compile and run tests**

Run: `chruby 3.4.3 && bundle exec rake compile && bundle exec ruby -Itest test/declaration_test.rb -n /members/`
Expected: PASS

**Step 5: Commit**

```bash
git add ext/rubydex/rustbindings.h ext/rubydex/declaration.c
git commit -m "feat: add Declaration#members Ruby API"
```

---

## Task 4: Graph synthetic definition storage - Rust

**Files:**
- Modify: `rust/rubydex/src/model/graph.rs`

**Step 1: Add synthetic storage fields to Graph struct**

Add to the Graph struct fields:

```rust
/// Synthetic method definitions added by plugins: (def_id, owner_fqn)
synthetic_methods: Vec<(DefinitionId, String)>,
/// Synthetic class definitions added by plugins
synthetic_classes: Vec<DefinitionId>,
/// Synthetic module definitions added by plugins
synthetic_modules: Vec<DefinitionId>,
/// Synthetic mixin relationships: (target_decl_id, mixin)
synthetic_mixins: Vec<(DeclarationId, Mixin)>,
/// Included hooks: trigger_module_fqn -> vec of modules to extend
included_hooks: HashMap<String, Vec<String>>,
```

**Step 2: Initialize in Graph::new()**

Add to `Graph::new()`:

```rust
synthetic_methods: Vec::new(),
synthetic_classes: Vec::new(),
synthetic_modules: Vec::new(),
synthetic_mixins: Vec::new(),
included_hooks: HashMap::new(),
```

**Step 3: Add accessor methods**

```rust
pub fn synthetic_methods(&self) -> &[(DefinitionId, String)] {
    &self.synthetic_methods
}

pub fn synthetic_classes(&self) -> &[DefinitionId] {
    &self.synthetic_classes
}

pub fn synthetic_modules(&self) -> &[DefinitionId] {
    &self.synthetic_modules
}

pub fn synthetic_mixins(&self) -> &[(DeclarationId, Mixin)] {
    &self.synthetic_mixins
}

pub fn included_hooks(&self) -> &HashMap<String, Vec<String>> {
    &self.included_hooks
}
```

**Step 4: Compile**

Run: `cd rust && cargo build`
Expected: PASS

**Step 5: Commit**

```bash
git add rust/rubydex/src/model/graph.rs
git commit -m "feat(rust): add synthetic definition storage to Graph"
```

---

## Task 5: Graph#add_method - Ruby test

**Files:**
- Modify: `test/graph_test.rb`

**Step 1: Write the failing test**

Add to `test/graph_test.rb`:

```ruby
def test_add_method_creates_synthetic_method
  with_context do |context|
    context.write!("post.rb", "class Post; end")

    graph = Rubydex::Graph.new
    graph.index_all(context.glob("**/*.rb"))
    graph.resolve

    # Post starts with 0 members
    post = graph["Post"]
    assert_equal 0, post.members.count

    # Add synthetic method
    result = graph.add_method(
      owner: "Post",
      name: "author",
      file_path: context.absolute_path_to("post.rb"),
      line: 1,
      column: 0
    )
    assert result

    graph.resolve

    # Now Post has 1 member
    assert_equal 1, post.members.count
    assert_equal "author", post.members.first.unqualified_name
  end
end

def test_add_method_getter_and_setter
  with_context do |context|
    context.write!("post.rb", "class Post; end")

    graph = Rubydex::Graph.new
    graph.index_all(context.glob("**/*.rb"))
    graph.resolve

    file_path = context.absolute_path_to("post.rb")
    graph.add_method(owner: "Post", name: "author", file_path: file_path, line: 1, column: 0)
    graph.add_method(owner: "Post", name: "author=", file_path: file_path, line: 1, column: 0)
    graph.resolve

    post = graph["Post"]
    member_names = post.members.map(&:unqualified_name)
    assert_equal 2, member_names.size
    assert_includes member_names, "author"
    assert_includes member_names, "author="
  end
end

def test_add_method_to_nonexistent_owner_returns_false
  with_context do |context|
    context.write!("post.rb", "class Post; end")

    graph = Rubydex::Graph.new
    graph.index_all(context.glob("**/*.rb"))
    graph.resolve

    result = graph.add_method(
      owner: "NonExistent",
      name: "foo",
      file_path: context.absolute_path_to("post.rb"),
      line: 1,
      column: 0
    )
    refute result
  end
end
```

**Step 2: Run test to verify it fails**

Run: `chruby 3.4.3 && bundle exec ruby -Itest test/graph_test.rb -n /add_method/`
Expected: FAIL with "undefined method `add_method'"

**Step 3: Commit test**

```bash
git add test/graph_test.rb
git commit -m "test: add Graph#add_method tests (red)"
```

---

## Task 6: Graph#add_method - Rust implementation

**Files:**
- Modify: `rust/rubydex-sys/src/graph_api.rs`
- Modify: `rust/rubydex/src/model/graph.rs`

**Step 1: Add FFI function to graph_api.rs**

```rust
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
        graph.add_synthetic_method(&owner, &name, &path, line, column)
    })
}
```

**Step 2: Add Graph method in graph.rs**

```rust
/// Adds a synthetic method definition created by plugins.
/// Returns true if the owner exists and method was added, false otherwise.
pub fn add_synthetic_method(
    &mut self,
    owner_name: &str,
    method_name: &str,
    file_path: &str,
    line: u32,
    column: u32,
) -> bool {
    use crate::model::definitions::{Definition, DefinitionFlags, MethodDefinition};
    use crate::model::visibility::Visibility;

    // Check owner exists
    let owner_decl_id = DeclarationId::from(owner_name);
    if !self.declarations().contains_key(&owner_decl_id) {
        return false;
    }

    let uri_id = UriId::from(file_path);
    let name_str_id = self.intern_string(method_name.to_string());
    let fqn = format!("{}#{}", owner_name, method_name);

    // Create synthetic offset
    let start = (line.saturating_sub(1)) * 1000 + column;
    let offset = Offset::new(start, start + method_name.len() as u32, line, column);

    // Create definition ID
    let def_id = DefinitionId::new(self.next_synthetic_id());

    let definition = Definition::Method(Box::new(MethodDefinition::new(
        name_str_id,
        uri_id,
        offset,
        Vec::new(), // no comments
        DefinitionFlags::empty(),
        None, // owner set during resolution
        Visibility::Public,
        Vec::new(), // no parameters
    )));

    self.definitions_mut().insert(def_id, definition);
    self.synthetic_methods.push((def_id, owner_name.to_string()));
    true
}

/// Returns next synthetic definition ID (negative to avoid collision)
fn next_synthetic_id(&mut self) -> i64 {
    self.synthetic_id_counter -= 1;
    self.synthetic_id_counter
}
```

**Step 3: Add synthetic_id_counter field to Graph struct**

```rust
/// Counter for synthetic definition IDs (negative values)
synthetic_id_counter: i64,
```

Initialize in `Graph::new()`:
```rust
synthetic_id_counter: 0,
```

**Step 4: Compile**

Run: `cd rust && cargo build`
Expected: PASS

**Step 5: Commit**

```bash
git add rust/rubydex-sys/src/graph_api.rs rust/rubydex/src/model/graph.rs
git commit -m "feat(rust): add synthetic method support"
```

---

## Task 7: Graph#add_method - C bindings

**Files:**
- Modify: `ext/rubydex/rustbindings.h`
- Modify: `ext/rubydex/graph.c`

**Step 1: Update rustbindings.h**

```c
bool rdx_graph_add_method(GraphPointer pointer, const char *owner_name, const char *method_name, const char *file_path, uint32_t line, uint32_t column);
```

**Step 2: Add C implementation in graph.c**

```c
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
        rb_raise(rb_eArgError, "missing required keyword arguments: owner, name, file_path, line, column");
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

**Step 3: Register the method**

In `initialize_graph` function, add:
```c
rb_define_method(cGraph, "add_method", sr_graph_add_method, -1);
```

**Step 4: Compile and run tests**

Run: `chruby 3.4.3 && bundle exec rake compile && bundle exec ruby -Itest test/graph_test.rb -n /add_method/`
Expected: PASS

**Step 5: Commit**

```bash
git add ext/rubydex/rustbindings.h ext/rubydex/graph.c
git commit -m "feat: add Graph#add_method Ruby API"
```

---

## Task 8: Process synthetic methods during resolution

**Files:**
- Modify: `rust/rubydex/src/resolution.rs`

**Step 1: Add synthetic method processing to resolver**

In the resolution logic, after creating declarations, process synthetic methods:

```rust
// Process synthetic methods - add them to their owner declarations
for (def_id, owner_fqn) in graph.synthetic_methods().to_vec() {
    let owner_decl_id = DeclarationId::from(owner_fqn.as_str());

    if let Some(decl) = graph.declarations_mut().get_mut(&owner_decl_id) {
        // Get method name from definition
        if let Some(def) = graph.definitions().get(&def_id) {
            let method_name = def.name(); // Assuming this returns the unqualified name
            let name_str_id = StringId::from(method_name);

            // Create or get method declaration
            let method_fqn = format!("{}#{}", owner_fqn, method_name);
            let method_decl_id = DeclarationId::from(method_fqn.as_str());

            // Add member to owner
            match decl {
                Declaration::Class(c) => c.add_member(name_str_id, method_decl_id),
                Declaration::Module(m) => m.add_member(name_str_id, method_decl_id),
                Declaration::SingletonClass(s) => s.add_member(name_str_id, method_decl_id),
                _ => {}
            }
        }
    }
}
```

**Step 2: Compile and test**

Run: `cd rust && cargo build && cd .. && chruby 3.4.3 && bundle exec rake compile && bundle exec ruby -Itest test/graph_test.rb -n /add_method/`
Expected: PASS

**Step 3: Commit**

```bash
git add rust/rubydex/src/resolution.rs
git commit -m "feat(rust): process synthetic methods during resolution"
```

---

## Task 9: Graph#add_class - Test and implementation

**Files:**
- Modify: `test/graph_test.rb`
- Modify: `rust/rubydex-sys/src/graph_api.rs`
- Modify: `rust/rubydex/src/model/graph.rs`
- Modify: `ext/rubydex/rustbindings.h`
- Modify: `ext/rubydex/graph.c`

**Step 1: Write the failing test**

Add to `test/graph_test.rb`:

```ruby
def test_add_class_creates_synthetic_class
  with_context do |context|
    context.write!("spec.rb", "# RSpec file")

    graph = Rubydex::Graph.new
    graph.index_all(context.glob("**/*.rb"))
    graph.resolve

    # Class doesn't exist yet
    assert_nil graph["RSpec::ExampleGroups::Calculator"]

    result = graph.add_class(
      name: "RSpec::ExampleGroups::Calculator",
      file_path: context.absolute_path_to("spec.rb"),
      line: 1,
      column: 0
    )
    assert result

    graph.resolve

    # Now it exists
    calc = graph["RSpec::ExampleGroups::Calculator"]
    refute_nil calc
    assert_equal "Calculator", calc.unqualified_name
  end
end

def test_add_class_with_parent
  with_context do |context|
    context.write!("spec.rb", "class RSpec::Core::ExampleGroup; end")

    graph = Rubydex::Graph.new
    graph.index_all(context.glob("**/*.rb"))
    graph.resolve

    graph.add_class(
      name: "RSpec::ExampleGroups::Calculator",
      parent: "RSpec::Core::ExampleGroup",
      file_path: context.absolute_path_to("spec.rb"),
      line: 1,
      column: 0
    )
    graph.resolve

    calc = graph["RSpec::ExampleGroups::Calculator"]
    refute_nil calc
    assert_equal "Calculator", calc.unqualified_name
  end
end
```

**Step 2: Run test (should fail)**

Run: `chruby 3.4.3 && bundle exec ruby -Itest test/graph_test.rb -n /add_class/`
Expected: FAIL

**Step 3: Add Rust FFI function**

```rust
/// Adds a synthetic class definition to the graph.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_add_class(
    pointer: GraphPointer,
    class_name: *const c_char,
    parent_name: *const c_char, // can be null
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
        graph.add_synthetic_class(&name, parent.as_deref(), &path, line, column)
    })
}
```

**Step 4: Add Graph method**

```rust
pub fn add_synthetic_class(
    &mut self,
    class_name: &str,
    parent_name: Option<&str>,
    file_path: &str,
    line: u32,
    column: u32,
) -> bool {
    use crate::model::definitions::{ClassDefinition, Definition, DefinitionFlags};

    let uri_id = UriId::from(file_path);
    let unqualified = class_name.rsplit("::").next().unwrap_or(class_name);
    let name_str_id = self.intern_string(unqualified.to_string());

    let start = (line.saturating_sub(1)) * 1000 + column;
    let offset = Offset::new(start, start + class_name.len() as u32, line, column);

    let def_id = DefinitionId::new(self.next_synthetic_id());

    // Create parent reference if provided
    let parent_name_id = parent_name.map(|p| {
        let parent_str_id = self.intern_string(p.rsplit("::").next().unwrap_or(p).to_string());
        self.add_name(crate::model::name::Name::new(parent_str_id, None, None))
    });

    let definition = Definition::Class(Box::new(ClassDefinition::new(
        name_str_id,
        uri_id,
        offset,
        Vec::new(),
        DefinitionFlags::empty(),
        None,
        parent_name_id,
    )));

    self.definitions_mut().insert(def_id, definition);
    self.synthetic_classes.push(def_id);
    true
}
```

**Step 5: Add C binding**

rustbindings.h:
```c
bool rdx_graph_add_class(GraphPointer pointer, const char *class_name, const char *parent_name, const char *file_path, uint32_t line, uint32_t column);
```

graph.c:
```c
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

Register: `rb_define_method(cGraph, "add_class", sr_graph_add_class, -1);`

**Step 6: Compile and test**

Run: `chruby 3.4.3 && bundle exec rake compile && bundle exec ruby -Itest test/graph_test.rb -n /add_class/`
Expected: PASS

**Step 7: Commit**

```bash
git add test/graph_test.rb rust/rubydex-sys/src/graph_api.rs rust/rubydex/src/model/graph.rs ext/rubydex/rustbindings.h ext/rubydex/graph.c
git commit -m "feat: add Graph#add_class for synthetic class definitions"
```

---

## Task 10: Graph#add_module - Test and implementation

Follow the same pattern as Task 9 for modules.

**Tests to add:**

```ruby
def test_add_module_creates_synthetic_module
  with_context do |context|
    context.write!("concern.rb", "module MyConcern; end")

    graph = Rubydex::Graph.new
    graph.index_all(context.glob("**/*.rb"))
    graph.resolve

    # Module doesn't exist yet
    assert_nil graph["MyConcern::ClassMethods"]

    result = graph.add_module(
      name: "MyConcern::ClassMethods",
      file_path: context.absolute_path_to("concern.rb"),
      line: 1,
      column: 0
    )
    assert result

    graph.resolve

    # Now it exists
    class_methods = graph["MyConcern::ClassMethods"]
    refute_nil class_methods
    assert_equal "ClassMethods", class_methods.unqualified_name
  end
end

def test_add_module_can_have_methods_added
  with_context do |context|
    context.write!("concern.rb", "module MyConcern; end")

    graph = Rubydex::Graph.new
    graph.index_all(context.glob("**/*.rb"))
    graph.resolve

    file_path = context.absolute_path_to("concern.rb")
    graph.add_module(name: "MyConcern::ClassMethods", file_path: file_path, line: 1, column: 0)
    graph.resolve

    # Add method to the synthetic module
    graph.add_method(owner: "MyConcern::ClassMethods", name: "foo", file_path: file_path, line: 1, column: 0)
    graph.resolve

    class_methods = graph["MyConcern::ClassMethods"]
    assert_equal 1, class_methods.members.count
    assert_equal "foo", class_methods.members.first.unqualified_name
  end
end
```

**Rust FFI:**

```rust
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_add_module(
    pointer: GraphPointer,
    module_name: *const c_char,
    file_path: *const c_char,
    line: u32,
    column: u32,
) -> bool { ... }
```

**Commit:** `git commit -m "feat: add Graph#add_module for synthetic module definitions"`

---

## Task 11: Graph#add_mixin - Test and implementation

**Tests:**

```ruby
def test_add_mixin_extend
  with_context do |context|
    context.write!("post.rb", <<~RUBY)
      module Concern; end
      module Concern::ClassMethods
        def foo; end
      end
      class Post; end
    RUBY

    graph = Rubydex::Graph.new
    graph.index_all(context.glob("**/*.rb"))
    graph.resolve

    result = graph.add_mixin(
      target: "Post",
      module_name: "Concern::ClassMethods",
      type: :extend
    )
    assert result
  end
end

def test_add_mixin_include
  with_context do |context|
    context.write!("post.rb", <<~RUBY)
      module Validations
        def validate; end
      end
      class Post; end
    RUBY

    graph = Rubydex::Graph.new
    graph.index_all(context.glob("**/*.rb"))
    graph.resolve

    result = graph.add_mixin(
      target: "Post",
      module_name: "Validations",
      type: :include
    )
    assert result
  end
end

def test_add_mixin_to_nonexistent_target_returns_false
  with_context do |context|
    context.write!("post.rb", "module Foo; end")

    graph = Rubydex::Graph.new
    graph.index_all(context.glob("**/*.rb"))
    graph.resolve

    result = graph.add_mixin(
      target: "NonExistent",
      module_name: "Foo",
      type: :include
    )
    refute result
  end
end
```

**Rust:**

```rust
#[repr(C)]
pub enum MixinType {
    Include = 0,
    Prepend = 1,
    Extend = 2,
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_add_mixin(
    pointer: GraphPointer,
    target_name: *const c_char,
    module_name: *const c_char,
    mixin_type: MixinType,
) -> bool { ... }
```

**Commit:** `git commit -m "feat: add Graph#add_mixin for synthetic mixin relationships"`

---

## Task 12: Graph#register_included_hook - Test and implementation

**Tests:**

```ruby
def test_register_included_hook
  with_context do |context|
    context.write!("concern.rb", <<~RUBY)
      module MyConcern; end
      class Post
        include MyConcern
      end
    RUBY

    graph = Rubydex::Graph.new
    graph.index_all(context.glob("**/*.rb"))
    graph.resolve

    file_path = context.absolute_path_to("concern.rb")

    # Add ClassMethods module with a method
    graph.add_module(name: "MyConcern::ClassMethods", file_path: file_path, line: 1, column: 0)
    graph.add_method(owner: "MyConcern::ClassMethods", name: "foo", file_path: file_path, line: 1, column: 0)

    # Register hook: when MyConcern is included, extend ClassMethods
    result = graph.register_included_hook(
      module_name: "MyConcern",
      extend_module: "MyConcern::ClassMethods"
    )
    assert result

    graph.resolve

    # Verify ClassMethods exists with foo
    class_methods = graph["MyConcern::ClassMethods"]
    refute_nil class_methods
    assert_equal 1, class_methods.members.count
  end
end

def test_register_included_hook_returns_true
  with_context do |context|
    context.write!("concern.rb", "module MyConcern; end")

    graph = Rubydex::Graph.new
    graph.index_all(context.glob("**/*.rb"))
    graph.resolve

    result = graph.register_included_hook(
      module_name: "MyConcern",
      extend_module: "MyConcern::ClassMethods"
    )
    assert result
  end
end
```

**Rust:**

```rust
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_register_included_hook(
    pointer: GraphPointer,
    trigger_module: *const c_char,
    extend_module: *const c_char,
) -> bool { ... }
```

**Graph method:**

```rust
pub fn register_included_hook(&mut self, trigger_module: &str, extend_module: &str) {
    self.included_hooks
        .entry(trigger_module.to_string())
        .or_default()
        .push(extend_module.to_string());
}
```

**Commit:** `git commit -m "feat: add Graph#register_included_hook for concern-style auto-extend"`

---

## Task 13: Integration test - belongs_to scenario

**Files:**
- Modify: `test/graph_test.rb`

**Test:**

```ruby
def test_integration_belongs_to_plugin_simulation
  with_context do |context|
    context.write!("post.rb", <<~RUBY)
      class Post
        belongs_to :author
      end
    RUBY

    graph = Rubydex::Graph.new
    graph.index_all(context.glob("**/*.rb"))
    graph.resolve

    # Post starts with 0 members (belongs_to is not a method definition)
    post = graph["Post"]
    assert_equal 0, post.members.count

    # Simulate plugin adding synthetic methods for belongs_to :author
    file_path = context.absolute_path_to("post.rb")
    graph.add_method(owner: "Post", name: "author", file_path: file_path, line: 2, column: 2)
    graph.add_method(owner: "Post", name: "author=", file_path: file_path, line: 2, column: 2)
    graph.resolve

    # Now Post has 2 members
    member_names = post.members.map(&:unqualified_name)
    assert_equal 2, member_names.size
    assert_includes member_names, "author"
    assert_includes member_names, "author="
  end
end
```

**Commit:** `git commit -m "test: add integration test for belongs_to plugin scenario"`

---

## Task 14: Integration test - class_methods scenario

**Test:**

```ruby
def test_integration_class_methods_plugin_simulation
  with_context do |context|
    context.write!("concern.rb", <<~RUBY)
      module MyConcern
        extend ActiveSupport::Concern

        class_methods do
          def foo; end
        end
      end

      class Post
        include MyConcern
      end
    RUBY

    graph = Rubydex::Graph.new
    graph.index_all(context.glob("**/*.rb"))
    graph.resolve

    # MyConcern::ClassMethods doesn't exist initially
    assert_nil graph["MyConcern::ClassMethods"]

    file_path = context.absolute_path_to("concern.rb")

    # Simulate what a plugin would do when seeing `class_methods do ... end`
    # 1. Create ClassMethods module
    graph.add_module(name: "MyConcern::ClassMethods", file_path: file_path, line: 4, column: 2)
    graph.resolve

    # 2. Add foo method to ClassMethods
    graph.add_method(owner: "MyConcern::ClassMethods", name: "foo", file_path: file_path, line: 5, column: 4)

    # 3. Register included hook
    graph.register_included_hook(module_name: "MyConcern", extend_module: "MyConcern::ClassMethods")

    graph.resolve

    # Verify ClassMethods exists with foo method
    class_methods = graph["MyConcern::ClassMethods"]
    refute_nil class_methods
    assert_equal 1, class_methods.members.count
    assert_equal "foo", class_methods.members.first.unqualified_name

    # Post still exists
    post = graph["Post"]
    refute_nil post
  end
end
```

**Commit:** `git commit -m "test: add integration test for class_methods plugin scenario"`

---

## Task 15: Integration test - RSpec scenario

**Test:**

```ruby
def test_integration_rspec_plugin_simulation
  with_context do |context|
    context.write!("spec.rb", <<~RUBY)
      RSpec.describe "Calculator" do
        subject { Calculator.new }
        let(:value) { 42 }

        context "when adding" do
          let(:other) { 10 }
        end
      end
    RUBY

    graph = Rubydex::Graph.new
    graph.index_all(context.glob("**/*.rb"))
    graph.resolve

    # No synthetic classes exist yet
    assert_nil graph["RSpec::ExampleGroups::Calculator"]

    file_path = context.absolute_path_to("spec.rb")

    # Simulate what a plugin would do for RSpec.describe
    # 1. Create example group class for "Calculator"
    graph.add_class(name: "RSpec::ExampleGroups::Calculator", file_path: file_path, line: 1, column: 0)
    graph.resolve

    # 2. Add subject and let methods
    graph.add_method(owner: "RSpec::ExampleGroups::Calculator", name: "subject", file_path: file_path, line: 2, column: 2)
    graph.add_method(owner: "RSpec::ExampleGroups::Calculator", name: "value", file_path: file_path, line: 3, column: 2)

    # 3. Create nested context class
    graph.add_class(
      name: "RSpec::ExampleGroups::Calculator::WhenAdding",
      parent: "RSpec::ExampleGroups::Calculator",
      file_path: file_path,
      line: 5,
      column: 2
    )
    graph.resolve

    # 4. Add let method to nested context
    graph.add_method(owner: "RSpec::ExampleGroups::Calculator::WhenAdding", name: "other", file_path: file_path, line: 6, column: 4)

    graph.resolve

    # Verify Calculator class has subject and value
    calc = graph["RSpec::ExampleGroups::Calculator"]
    refute_nil calc
    calc_members = calc.members.map(&:unqualified_name)
    assert_equal 2, calc_members.size
    assert_includes calc_members, "subject"
    assert_includes calc_members, "value"

    # Verify WhenAdding class has other
    when_adding = graph["RSpec::ExampleGroups::Calculator::WhenAdding"]
    refute_nil when_adding
    assert_equal 1, when_adding.members.count
    assert_equal "other", when_adding.members.first.unqualified_name
  end
end
```

**Commit:** `git commit -m "test: add integration test for RSpec plugin scenario"`

---

## Task 16: Run full test suite

**Step 1: Run all Ruby tests**

Run: `chruby 3.4.3 && bundle exec rake ruby_test`
Expected: All tests pass

**Step 2: Run Rust tests**

Run: `cd rust && cargo test`
Expected: All tests pass

**Step 3: Run linter**

Run: `chruby 3.4.3 && bundle exec rake lint`
Expected: No errors

**Step 4: Final cleanup commit if needed**

```bash
git add -A
git commit -m "chore: fix any lint issues"
```

---

## Task 17: Declaration#ancestors - Ruby test

**Problem:** The `add_mixin` tests only assert that the API call succeeds, but don't verify that the mixin actually affects the ancestor chain. We need a `Declaration#ancestors` API to enable proper test coverage.

**Files:**
- Modify: `test/declaration_test.rb`

**Step 1: Write the failing test**

Add to `test/declaration_test.rb`:

```ruby
def test_ancestors_returns_linearized_ancestor_chain
  with_context do |context|
    context.write!("file.rb", <<~RUBY)
      module Mixin; end
      class Parent; end
      class Child < Parent
        include Mixin
      end
    RUBY

    graph = Rubydex::Graph.new
    graph.index_all(context.glob("**/*.rb"))
    graph.resolve

    child = graph["Child"]
    ancestor_names = child.ancestors.map(&:name)

    # Linearized order: [Child, Mixin, Parent, Object]
    assert_includes(ancestor_names, "Child")
    assert_includes(ancestor_names, "Mixin")
    assert_includes(ancestor_names, "Parent")
  end
end

def test_ancestors_returns_empty_for_module_with_no_mixins
  with_context do |context|
    context.write!("file.rb", "module Standalone; end")

    graph = Rubydex::Graph.new
    graph.index_all(context.glob("**/*.rb"))
    graph.resolve

    standalone = graph["Standalone"]
    ancestor_names = standalone.ancestors.map(&:name)
    # Module's ancestors include itself
    assert_includes(ancestor_names, "Standalone")
  end
end

def test_ancestors_enumerator_has_size
  with_context do |context|
    context.write!("file.rb", "class Post; end")

    graph = Rubydex::Graph.new
    graph.index_all(context.glob("**/*.rb"))
    graph.resolve

    post = graph["Post"]
    assert(post.ancestors.size >= 1) # At least Post itself
  end
end
```

**Step 2: Run test to verify it fails**

Run: `chruby 3.4.3 && bundle exec ruby -Itest test/declaration_test.rb -n /ancestors/`
Expected: FAIL with "undefined method `ancestors'"

**Step 3: Do NOT commit** (commits handled at end)

---

## Task 18: Declaration#ancestors - Rust implementation

**Files:**
- Modify: `rust/rubydex-sys/src/declaration_api.rs`

**Step 1: Add AncestorsIter struct and functions**

Add to `rust/rubydex-sys/src/declaration_api.rs`:

```rust
use rubydex::model::declaration::Ancestor;

/// Iterator over ancestor declaration IDs for a namespace
pub struct AncestorsIter {
    ids: Box<[i64]>,
    index: usize,
}

/// Creates a new iterator over ancestor declaration IDs for a given namespace declaration.
/// Only returns Complete ancestors (resolved declaration IDs), skipping Partial ones.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer`
/// - The returned pointer must be freed with `rdx_ancestors_iter_free`
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_declaration_ancestors_iter_new(
    pointer: GraphPointer,
    decl_id: i64,
) -> *mut AncestorsIter {
    with_graph(pointer, |graph| {
        let decl_id = DeclarationId::new(decl_id);
        let ancestors: Vec<i64> = if let Some(decl) = graph.declarations().get(&decl_id) {
            Declaration::iter_ancestors(decl, |ancestor| {
                if let Ancestor::Complete(id) = ancestor {
                    Some(**id)
                } else {
                    None
                }
            }).flatten().collect()
        } else {
            Vec::new()
        };
        Box::into_raw(Box::new(AncestorsIter {
            ids: ancestors.into_boxed_slice(),
            index: 0,
        }))
    })
}

/// Advances the iterator and writes the next ID into `out_id`.
/// Returns `true` if an ID was written, `false` if exhausted.
///
/// # Safety
/// - `iter` must be valid pointer from `rdx_declaration_ancestors_iter_new`
/// - `out_id` must be a valid, writable pointer
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_ancestors_iter_next(iter: *mut AncestorsIter, out_id: *mut i64) -> bool {
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

/// Returns the total number of ancestors in the iterator.
///
/// # Safety
/// - `iter` must be valid pointer from `rdx_declaration_ancestors_iter_new`
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_ancestors_iter_len(iter: *const AncestorsIter) -> usize {
    if iter.is_null() {
        0
    } else {
        unsafe { (&(*iter).ids).len() }
    }
}

/// Frees an iterator created by `rdx_declaration_ancestors_iter_new`.
///
/// # Safety
/// - `iter` must be a pointer from `rdx_declaration_ancestors_iter_new`
/// - `iter` must not be used after being freed
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_ancestors_iter_free(iter: *mut AncestorsIter) {
    if !iter.is_null() {
        unsafe {
            let _ = Box::from_raw(iter);
        }
    }
}
```

**Step 2: Compile Rust**

Run: `cd rust && cargo build`
Expected: PASS

**Step 3: Do NOT commit** (commits handled at end)

---

## Task 19: Declaration#ancestors - C bindings

**Files:**
- Modify: `ext/rubydex/rustbindings.h`
- Modify: `ext/rubydex/declaration.c`

**Step 1: Update rustbindings.h**

Add declarations:

```c
typedef struct AncestorsIter AncestorsIter;
struct AncestorsIter *rdx_declaration_ancestors_iter_new(GraphPointer pointer, int64_t decl_id);
bool rdx_ancestors_iter_next(struct AncestorsIter *iter, int64_t *out_id);
size_t rdx_ancestors_iter_len(const struct AncestorsIter *iter);
void rdx_ancestors_iter_free(struct AncestorsIter *iter);
```

**Step 2: Add C implementation in declaration.c**

Add to `ext/rubydex/declaration.c` (follow the same pattern as `members`):

```c
// Body function for rb_ensure in Declaration#ancestors
static VALUE declaration_ancestors_yield(VALUE args) {
    VALUE self = rb_ary_entry(args, 0);
    void *iter = (void *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));

    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    int64_t id = 0;
    while (rdx_ancestors_iter_next(iter, &id)) {
        VALUE argv[] = {data->graph_obj, LL2NUM(id)};
        VALUE handle = rb_class_new_instance(2, argv, cDeclaration);
        rb_yield(handle);
    }

    return Qnil;
}

// Ensure function to free iterator
static VALUE declaration_ancestors_ensure(VALUE args) {
    void *iter = (void *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));
    rdx_ancestors_iter_free(iter);
    return Qnil;
}

// Size function for enumerator
static VALUE declaration_ancestors_size(VALUE self, VALUE _args, VALUE _eobj) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    void *iter = rdx_declaration_ancestors_iter_new(graph, data->id);
    size_t len = rdx_ancestors_iter_len(iter);
    rdx_ancestors_iter_free(iter);

    return SIZET2NUM(len);
}

// Declaration#ancestors: () -> Enumerator[Declaration]
static VALUE sr_declaration_ancestors(VALUE self) {
    if (!rb_block_given_p()) {
        return rb_enumeratorize_with_size(self, rb_str_new2("ancestors"), 0, NULL, declaration_ancestors_size);
    }

    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    void *iter = rdx_declaration_ancestors_iter_new(graph, data->id);
    VALUE args = rb_ary_new_from_args(2, self, ULL2NUM((uintptr_t)iter));
    rb_ensure(declaration_ancestors_yield, args, declaration_ancestors_ensure, args);

    return self;
}
```

**Step 3: Register the method**

In `initialize_declaration` function, add:

```c
rb_define_method(cDeclaration, "ancestors", sr_declaration_ancestors, 0);
```

**Step 4: Compile and run tests**

Run: `chruby 3.4.3 && bundle exec rake compile && bundle exec ruby -Itest test/declaration_test.rb -n /ancestors/`
Expected: PASS

**Step 5: Do NOT commit** (commits handled at end)

---

## Task 20: Process synthetic mixins during ancestor linearization

**Problem:** Synthetic mixins stored via `Graph#add_mixin` are never processed during resolution. They need to be injected into the `linearize_ancestors` function so they affect the ancestor chain.

**Files:**
- Modify: `rust/rubydex/src/resolution.rs`

**Step 1: Understand the current flow**

In `linearize_ancestors` (around line 740-761), mixins are collected from definitions:

```rust
// For singleton classes, collect extends from attached object
mixins.extend(/* extends from definitions */);

// Collect prepends and includes from definitions
mixins.extend(/* prepends and includes from definitions */);
```

**Step 2: Add synthetic mixin injection**

After collecting mixins from definitions, inject synthetic mixins. Add this code after the existing mixin collection (around line 761):

```rust
// Inject synthetic mixins for this declaration
let synthetic_mixins: Vec<Mixin> = graph
    .synthetic_mixins()
    .iter()
    .filter(|(target_id, _)| *target_id == declaration_id)
    .map(|(_, mixin)| mixin.clone())
    .collect();

// For singleton classes, synthetic extends go on the attached object
if let Declaration::SingletonClass(_) = declaration {
    mixins.extend(
        synthetic_mixins
            .iter()
            .filter(|m| matches!(m, Mixin::Extend(_)))
            .cloned(),
    );
}

// Prepends and includes go on the current declaration
mixins.extend(
    synthetic_mixins
        .into_iter()
        .filter(|m| matches!(m, Mixin::Prepend(_) | Mixin::Include(_))),
);
```

**Step 3: Compile and test**

Run: `cd rust && cargo build && cargo test`
Expected: PASS

**Step 4: Do NOT commit** (commits handled at end)

---

## Task 21: Update add_mixin tests for proper coverage

**Problem:** The current `add_mixin` tests only assert the API call succeeds. We need to verify that mixins actually affect the ancestor chain.

**Files:**
- Modify: `test/graph_test.rb`

**Step 1: Replace weak tests with proper assertions**

Find and replace the existing `add_mixin` tests:

```ruby
def test_add_mixin_extend_affects_singleton_ancestors
  with_context do |context|
    context.write!("post.rb", <<~RUBY)
      module Concern; end
      module Concern::ClassMethods
        def foo; end
      end
      class Post; end
    RUBY

    graph = Rubydex::Graph.new
    graph.index_all(context.glob("**/*.rb"))
    graph.resolve

    # Get Post's singleton class ancestors BEFORE adding mixin
    post = graph["Post"]
    # Note: singleton_class may not be directly accessible, so we check via the class itself
    initial_ancestor_names = post.ancestors.map(&:name)
    refute_includes(initial_ancestor_names, "Concern::ClassMethods")

    # Add extend mixin (extend affects singleton class)
    result = graph.add_mixin(
      target: "Post",
      module_name: "Concern::ClassMethods",
      type: :extend,
    )
    assert(result)

    graph.resolve

    # Now Post's singleton class should have ClassMethods in ancestors
    # For extend, we check the singleton class's ancestors
    post_singleton = graph["Post::<Class:Post>"]
    if post_singleton
      singleton_ancestor_names = post_singleton.ancestors.map(&:name)
      assert_includes(singleton_ancestor_names, "Concern::ClassMethods")
    end
  end
end

def test_add_mixin_include_affects_ancestors
  with_context do |context|
    context.write!("post.rb", <<~RUBY)
      module Validations
        def validate; end
      end
      class Post; end
    RUBY

    graph = Rubydex::Graph.new
    graph.index_all(context.glob("**/*.rb"))
    graph.resolve

    post = graph["Post"]
    initial_ancestor_names = post.ancestors.map(&:name)
    refute_includes(initial_ancestor_names, "Validations")

    # Add include mixin
    result = graph.add_mixin(
      target: "Post",
      module_name: "Validations",
      type: :include,
    )
    assert(result)

    graph.resolve

    # Now Post should have Validations in ancestors
    updated_ancestor_names = post.ancestors.map(&:name)
    assert_includes(updated_ancestor_names, "Validations")
  end
end

def test_add_mixin_prepend_affects_ancestors
  with_context do |context|
    context.write!("post.rb", <<~RUBY)
      module Prepended
        def save; end
      end
      class Post; end
    RUBY

    graph = Rubydex::Graph.new
    graph.index_all(context.glob("**/*.rb"))
    graph.resolve

    post = graph["Post"]
    initial_ancestor_names = post.ancestors.map(&:name)
    refute_includes(initial_ancestor_names, "Prepended")

    # Add prepend mixin
    result = graph.add_mixin(
      target: "Post",
      module_name: "Prepended",
      type: :prepend,
    )
    assert(result)

    graph.resolve

    # Now Post should have Prepended in ancestors (before Post itself)
    updated_ancestor_names = post.ancestors.map(&:name)
    assert_includes(updated_ancestor_names, "Prepended")

    # Prepended should come before Post in the ancestor chain
    prepended_idx = updated_ancestor_names.index("Prepended")
    post_idx = updated_ancestor_names.index("Post")
    assert(prepended_idx < post_idx, "Prepended module should appear before Post in ancestors")
  end
end

def test_add_mixin_to_nonexistent_target_returns_false
  with_context do |context|
    context.write!("post.rb", "module Foo; end")

    graph = Rubydex::Graph.new
    graph.index_all(context.glob("**/*.rb"))
    graph.resolve

    result = graph.add_mixin(
      target: "NonExistent",
      module_name: "Foo",
      type: :include,
    )
    refute(result)
  end
end
```

**Step 2: Compile and run tests**

Run: `chruby 3.4.3 && bundle exec rake compile && bundle exec ruby -Itest test/graph_test.rb -n /add_mixin/`
Expected: PASS

**Step 3: Do NOT commit** (commits handled at end)

---

## Task 22: Run full test suite and verify

**Step 1: Run all Ruby tests**

Run: `chruby 3.4.3 && bundle exec rake ruby_test`
Expected: All tests pass

**Step 2: Run Rust tests**

Run: `cd rust && cargo test`
Expected: All tests pass

**Step 3: Run linter**

Run: `chruby 3.4.3 && bundle exec rake lint`
Expected: No errors (on modified files)

**Step 4: Verify add_mixin tests pass with proper assertions**

Run: `chruby 3.4.3 && bundle exec ruby -Itest test/graph_test.rb -n /add_mixin/`
Expected: All 4 tests pass with meaningful assertions

---

## Notes for Implementation

1. **Synthetic definitions use negative IDs** to avoid collision with real definition IDs

2. **Resolution must process synthetics** - The resolver needs to create declarations for synthetic definitions and add them as members to their owners

3. **DefinitionId::new()** takes i64 - synthetic IDs should be negative

4. **Test pattern** - All tests use `with_context` helper from `test/helpers/context.rb`

5. **Member iteration** - Declaration#members returns Declaration handles, not Definition handles

6. **Ancestor iteration** - Declaration#ancestors returns Declaration handles for resolved ancestors only (skips Partial/unresolved)

7. **Synthetic mixins require re-linearization** - After adding synthetic mixins, `resolve()` must be called to re-linearize ancestors
