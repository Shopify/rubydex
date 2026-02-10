# Sam's Learning Notes -- Saturn/Rubydex Codebase Review

> Explored the Ruby gem, C extension, FFI layer, build system, CI pipeline, and test infrastructure for the Rubydex code indexer. This is a Rust-powered static analysis tool for Ruby with a C FFI bridge to expose a Ruby-level API.

---

## Architecture Overview

The system has four layers:
1. **Rust core** (`rust/rubydex/`) -- all the indexing, resolution, and analysis logic (pure Rust, no Ruby dependency)
2. **Rust FFI exports** (`rust/rubydex-sys/`) -- `#[no_mangle] extern "C"` functions that expose Rust logic as C-callable functions, plus cbindgen auto-generates `rustbindings.h`
3. **C extension** (`ext/rubydex/`) -- connects the Ruby VM to the Rust FFI using Ruby's C API (`rb_define_method`, `TypedData_Wrap_Struct`, etc.)
4. **Ruby gem** (`lib/`) -- high-level Ruby classes (Graph, Location, Comment, Diagnostic) that users interact with

**Why three layers instead of Rust-to-Ruby directly?** (per Jordan) The core Rust crate is deliberately kept independent of Ruby. This means the same library can be used from other languages, tested without Ruby, and used directly by the CLI without FFI overhead.

The naming convention for C functions is documented in AGENTS.md:
- `rdx_` = Rust FFI exports (called from C into Rust)
- `rdxr_` = Ruby callback functions (registered with `rb_define_method`)
- `rdxi_` = Internal C helpers shared across files (declared in headers)
- No prefix = file-local static helpers

---

## C Extension & FFI Layer

### Q: Why does `with_graph` in `graph_api.rs` use `Box::from_raw` + `mem::forget` instead of just casting the pointer to a reference?

**Jordan:** The simpler approach (just casting the raw pointer to `&Graph` via `&*(pointer as *const Graph)`) would actually work for read-only access. The real reason for the `Box::from_raw` + `mem::forget` pattern is the *mutable* variant `with_mut_graph`. Creating `&mut Graph` from a raw pointer has stricter requirements in Rust -- you need to guarantee unique access. `Box::from_raw` gives you an owned `Box<Graph>`, from which `&mut Graph` comes naturally through `DerefMut`. The immutable `with_graph` then follows the same pattern for consistency. Both approaches are equally `unsafe` under the hood. Jordan also pointed out an important edge case: if a Ruby exception were raised inside the closure (which shouldn't happen since it's pure Rust code), the `mem::forget` would never run and the `Box` destructor would free the graph -- a use-after-free. In practice this can't happen because the Rust closures don't call back into Ruby.

### Q: What does `rb_ext_ractor_safe(true)` mean in `Init_rubydex`, and what constraints does it place on the C extension?

**Jordan:** Ractors are Ruby 3.0+'s actor-based concurrency model designed for true parallelism (unlike threads which share memory with a GVL). `rb_ext_ractor_safe(true)` tells the VM this extension can be used from any Ractor, not just the main one. The constraint is: no mutable global/static C variables, since multiple Ractors could call in concurrently. The extension meets this because the `VALUE` globals (`cGraph`, `cDeclaration`, etc.) are set once during `Init_rubydex` and never mutated, and actual data lives behind per-object pointers (each Graph owns its own Rust data via `TypedData_Wrap_Struct`). Jordan raised a subtle point: if two Ractors somehow shared the SAME Graph object, the `with_graph`/`with_mut_graph` functions would create aliased references -- undefined behavior. But Ruby prevents this because `TypedData`-wrapped objects are not shareable between Ractors unless explicitly marked with `RUBY_TYPED_FROZEN_SHAREABLE`, which this extension does not do.

### Q: The `extconf.rb` build process modifies the generated Makefile with string replacements to inject Cargo build steps. Isn't this fragile?

**Jordan:** Yes, it is somewhat fragile. The `gsub` calls rely on specific strings being present in the mkmf-generated Makefile (e.g., `$(OBJS): $(HDRS) $(ruby_headers)` and the `$(LDSHARED)` line). If mkmf changes its output, the gsub would fail silently and the build would fail with a confusing linker error. However, this is the standard approach for Ruby gems needing non-Ruby build systems -- mkmf was designed for C-only extensions. Jordan explained that alternatives exist: `rb_sys` lets you write Ruby bindings in Rust directly (eliminating the C layer), but Rubydex deliberately uses the three-layer approach (Rust -> C -> Ruby) to keep the core Rust crate completely independent of Ruby. This means the same Rust library can be used from other languages (Python, Node) by writing different FFI wrappers, the Rust code can be tested without Ruby installed, and the CLI uses the Rust library directly without FFI overhead. The Makefile patching is the price paid for that separation of concerns.

### Q: Why aren't memory leak checks (memleak.yml) run on macOS and Windows too? Is Valgrind Linux-only?

Asked **Jordan**. From the CI configuration: Valgrind is indeed primarily a Linux tool. While there is a macOS version, it has limited support for recent macOS versions and Apple Silicon. The Rust sanitizers (`-Zsanitizer=memory/leak/thread`) used in `extconf.rb:58-63` also require nightly Rust with `build-std`, which adds complexity. Running just on Linux gives good coverage of the C/Rust memory safety boundary since the core logic is platform-independent.

### Q: Getting a constant reference name requires 4 levels of indirection through names and strings. Why not store strings directly?

Asked **Jordan**. From `rust/rubydex-sys/src/reference_api.rs:104-117`: `graph.constant_references().get(id)` -> `reference.name_id()` -> `graph.names().get()` -> `name.str()` -> `graph.strings().get()`. This indirection is for memory efficiency in a "hyper-scale" indexer. String interning (storing each unique string once and referencing it by ID) means that if 10,000 references all point to a name like "initialize", there is only one copy of that string in memory. The Name layer adds semantic structure (parent scope, nesting) on top of raw strings. For a codebase with millions of definitions/references, this deduplication massively reduces memory usage.

---

## Code Quality & Patterns

### Q: Why are `handle.h` functions defined as `static` in the header instead of in a separate `handle.c` file?

Asked **Morgan**. Looking at `ext/rubydex/handle.h:11-44`, all four functions (`handle_mark`, `handle_free`, `rdxr_handle_alloc`, `rdxr_handle_initialize`) are `static`. The `static` keyword means each `.c` file that includes this header gets its own copy. But this is intentional -- these are small, frequently-called functions that benefit from inlining. Putting them in a header as `static` is essentially a C way to suggest inlining (like `inline` in C99). Since multiple C files need these exact same functions (declaration.c, definition.c, document.c, reference.c all include handle.h), this avoids having to export them from a separate translation unit while keeping the code DRY. The `rb_data_type_t handle_type` struct is also `static const` in the header for the same reason.

### Q: Why is `Declaration.new` made private using `rb_funcall` to call `private` instead of `rb_undef_alloc_func`?

Asked **Morgan**. In `ext/rubydex/declaration.c:284`: `rb_funcall(rb_singleton_class(cDeclaration), rb_intern("private"), 1, ID2SYM(rb_intern("new")))`. Using `rb_undef_alloc_func` would completely remove the allocation function, meaning even the C code couldn't create instances. By making `new` private instead, the C extension itself (and internal code) can still call `rb_class_new_instance` to create objects -- it's only the Ruby user code that's prevented from calling `Declaration.new` directly. The same pattern is used for `Reference`, `Definition`, and `Document`. This enforces that these objects can only be obtained through the Graph (e.g., `graph["Foo"]`, `graph.declarations`, `declaration.definitions`), which ensures they always have a valid graph reference and ID.

### Q: Is the iterator pattern (smuggling pointers through `ULL2NUM`/`NUM2ULL` via Ruby args) safe?

Asked **Morgan**. In `ext/rubydex/graph.c:84`, the raw C pointer to a Rust iterator is stored as a Ruby integer via `ULL2NUM((uintptr_t)iter)`. This is then passed through `rb_ensure` which guarantees the cleanup function runs. The key safety aspect is: the pointer is only stored temporarily in the `args` array during the `rb_ensure` call, never exposed to Ruby code, and always freed in the ensure block. Ruby integers (Fixnum/Bignum) are immutable values, not GC-managed heap objects, so there's no risk of the GC moving or freeing the number. The `rb_ensure` pattern (yield body + cleanup body) is Ruby's equivalent of try/finally, ensuring the Rust iterator is freed even if the block raises an exception.

### Q: Is it wasteful that `create_location_for_uri_and_offset` allocates a new CString for the URI every time?

Asked **Morgan**. Looking at `rust/rubydex-sys/src/location_api.rs:29-56`, yes, it does allocate a new `CString` for the URI on every call. With 10,000 definitions in one file, that is 10,000 copies. However, these are short-lived -- the C code immediately creates a Ruby string from it (`rb_utf8_str_new_cstr`) and then Rust frees the CString. The alternative (referencing the document's URI directly) would require the `Location` struct to borrow from the Graph, which would complicate the FFI boundary significantly since the Location needs to outlive the `with_graph` call. The current design prioritizes simplicity and correctness over allocation micro-optimization, which is a reasonable tradeoff for an FFI boundary that is not the hot path.

### Q: Why does `rdxi_str_array_to_char` copy Ruby strings via malloc+strcpy instead of passing Ruby string pointers directly?

Asked **Morgan**. In `ext/rubydex/utils.c:7-19`, each Ruby string is copied into a freshly `malloc`'d buffer. This is necessary because: (1) Ruby strings can be moved by the GC compactor, so the pointer from `StringValueCStr` could become invalid if a GC runs. (2) The Rust `index_all` function processes files in parallel using threads. Ruby's GC is not thread-safe in the sense that Rust worker threads cannot safely hold pointers into Ruby's heap. By copying the strings into plain C memory, the Rust side can safely use them from any thread without worrying about Ruby GC interactions.

---

## Ruby Semantics & Analysis

### Q: Why does the graph automatically include Object, Class, and Module as declarations? What if someone redefines them?

**Dr. Chen:** These are root declarations forming a circular dependency at the foundation of Ruby's object model. Object is the default superclass of all classes (`class Foo; end` implicitly means `class Foo < Object; end`), Class is the metaclass (every class object is an instance of Class), and Module is the superclass of Class. They're created in `resolution.rs:100-122` before processing anything else. They're needed because: (1) top-level constants are members of Object -- the resolution code uses `*OBJECT_ID` as the owner for constants without nesting at `resolution.rs:988`, (2) ancestor chains terminate at Object -- at `resolution.rs:734`, if `declaration_id == *OBJECT_ID`, return `None` for parent ancestors, and (3) singleton class parent computation needs Class and Module in `singleton_parent_id()` at `resolution.rs:1427-1458`. If someone reopens Object (`class Object; def my_method; end; end`), the indexer just creates a new Definition and resolution appends it to the existing Object Declaration. If someone wrote `module Object; end` (changing from class to module), it would be a runtime error in Ruby and the tool silently accepts it since the declaration already exists.

### Q: Why the two-level hierarchy of Declaration (unique identity) vs Definition (individual occurrences)?

**Dr. Chen:** This is the core design insight, driven by **when** information is available in the pipeline. Definitions are created during **Discovery** (parallel, per-file indexing) -- at this stage, each file is indexed independently on its own thread, we don't know what other files contain, and we can't resolve cross-file references or fully qualified names. Declarations are created during **Resolution** (single-threaded, whole-graph) -- here we can merge multiple definitions of the same name and resolve cross-file references. In Ruby, you can reopen a class in multiple files, so there is ONE `Foo` declaration but potentially many definitions (one per `class Foo` opening). The two levels are needed for IDE features: "Go to Definition" shows ALL source locations, per-definition comments/documentation are preserved, and declarations answer "what exists?" while definitions answer "where is it written?" Visible in `test/declaration_test.rb:53-69`: when class A is defined in two files, `declaration.definitions.size` is 2.

### Q: Why is nesting passed as an array of strings to `resolve_constant` rather than determined automatically from the code?

**Dr. Chen:** This API is NOT used by the indexer during normal processing (the indexer already knows nesting from the AST). It's an **external query interface** for tools like language servers. The use case: a language server receives "the user's cursor is on `CONST` at line 10 of file.rb, inside `class Bar::Baz` inside `module Foo`." The tool knows the nesting is `["Foo", "Bar::Baz"]` from the cursor position and passes it to the resolution API. The resolver then searches: (1) `Foo::Bar::Baz::CONST` (innermost scope), (2) `Foo::CONST` (enclosing module), (3) ancestor chain of `Foo::Bar::Baz` for `CONST`, (4) `::CONST` (top-level). The `nesting_stack_to_name_id` function in `name_api.rs:14` constructs the linked list of `Name` objects from these strings. After resolution, the function **untracks** the temporarily created names (`graph_api.rs:110-112`), cleaning up to avoid permanent side effects. Think of it as: indexing = the graph learns about the codebase (persistent), querying = someone asks a question (temporary, side-effect-free).

### Q: How does `rustbindings.h` (autogenerated by cbindgen) get maintained -- manual generation or automatic?

**Dr. Chen:** Fully automatic. In `rust/rubydex-sys/build.rs:4-6`, cbindgen runs during every `cargo build` and generates `rustbindings.h` into the `rubydex-sys` directory. cbindgen scans all `#[unsafe(no_mangle)]` functions and generates C function declarations, struct definitions, and enum types. The Ruby gem's Makefile (via `extconf.rb:87`) copies it from the Rust output to `ext/rubydex/` so the C extension can `#include "rustbindings.h"`. Dr. Chen also noted that both copies exist in the repo (the source-of-truth in `rust/rubydex-sys/` and the copy in `ext/rubydex/`). The checked-in copy serves purposes: CI can verify the header hasn't drifted, developers can see the C API without building, and editors can provide autocompletion. The `cbindgen.toml` configures C output, include guard, and `prefix_with_name = true` for enums (so `DefinitionKind::Class` becomes `DefinitionKind_Class` in C, avoiding name collisions).

### Q: Why do instance variables and class variables in singleton methods have different ownership rules?

Asked **Dr. Chen**. From `test/graph_test.rb:287-328`: inside `def Bar.something`, `@singleton_var` belongs to Bar's singleton class (`Bar::<Bar>#@singleton_var`), but `@@class_var_2` belongs to `Bar::Baz#@@class_var_2`. This reflects actual Ruby semantics: in Ruby, `@` instance variables belong to `self`, and in `def Bar.something`, `self` is the Bar object itself (its singleton class). But `@@` class variables are scoped to the enclosing class/module at definition time, not based on `self`. So `@@class_var_2` inside `def Bar.something` is scoped to the enclosing `class Bar::Baz` where the method was syntactically defined, not to `Bar`. This is a genuine (and confusing) Ruby behavior, not an approximation.

---

## Build System & CI

The build pipeline involves:
- `extconf.rb` generates a Makefile via `mkmf`, then patches it with `gsub` to add Cargo build steps
- `build.rs` invokes cbindgen to auto-generate `rustbindings.h` from Rust FFI signatures
- Cargo builds the Rust workspace as both `cdylib` (shared lib) and `staticlib` (for Windows)
- The Makefile copies `rustbindings.h` and the shared library to the correct locations
- The C extension compiles against Ruby headers + `rustbindings.h` and links to the Rust shared library
- `rake compile` orchestrates both Rust and C compilation
- `rake test` runs both `cargo test` (Rust) and `minitest` (Ruby)

CI has three pipelines:
- **CI** (`ci.yml`): lint + build matrix (Ruby 3.3/3.4/4.0 on Ubuntu/macOS/Windows)
- **Memleak** (`memleak.yml`): Valgrind + Rust sanitizers (memory, leak, thread) on Linux only
- **Index top 100 gems** (`indexing_top_gems.yml`): real-world smoke test against popular Ruby gems

Release builds use `lto = true`, `opt-level = 3`, `codegen-units = 1` for maximum performance (at the cost of longer compile times).

---

## Key Concepts I Learned

### Handle Pattern
The C extension uses a `HandleData` struct (in `handle.h`) that stores a Ruby `VALUE` reference to the Graph object and a `uint64_t` ID. This is used by Declaration, Definition, Document, and Reference objects. The Graph reference is marked with `rb_gc_mark` to keep the Graph alive as long as any handle exists. The ID is used to look up data through FFI calls -- the handle itself holds no Rust data, just a key into the graph.

### Iterator Snapshot Pattern
All iterators (declarations, definitions, documents, references) snapshot their IDs into a boxed slice at creation time. This means modifications to the graph after creating an iterator will not affect iteration. On the C side, `rb_ensure` wraps iteration to guarantee cleanup. The Rust iterator structs are simple: a boxed slice + an index counter.

### Memory Ownership
Memory ownership is carefully tracked across the FFI boundary:
- Strings returned from Rust (`CString`) must be freed with `free_c_string`
- Iterators must be freed with their corresponding `_iter_free` function
- Locations must be freed with `rdx_location_free` (which also frees the inner URI CString)
- Complex arrays (comments, diagnostics) have dedicated free functions that recursively free inner allocations
- The Graph itself is freed with `rdx_graph_free` via Ruby GC's free function (`graph_free` in `graph.c`)

### Class Hierarchy in the Ruby API
```
Declaration
  Namespace (adds: member, singleton_class, ancestors, descendants)
    Class
    Module
    SingletonClass
  Constant
  ConstantAlias
  Method
  GlobalVariable
  InstanceVariable
  ClassVariable

Definition (base, adds: location, comments, name, deprecated?, name_location)
  ClassDefinition
  ModuleDefinition
  SingletonClassDefinition
  ConstantDefinition
  ConstantAliasDefinition
  MethodDefinition
  AttrAccessorDefinition / AttrReaderDefinition / AttrWriterDefinition
  GlobalVariableDefinition
  InstanceVariableDefinition
  ClassVariableDefinition
  MethodAliasDefinition
  GlobalVariableAliasDefinition

Reference
  ConstantReference (adds: name, location)
  MethodReference (adds: name, location)
```

---

## Summary Observations

### Things I found most clever:
1. The Handle pattern -- using IDs instead of pointers means Ruby objects are lightweight and GC-friendly, while all real data stays in Rust-owned memory
2. The iterator snapshot approach ensures safe iteration across FFI without holding locks or borrows
3. Auto-generating `rustbindings.h` via cbindgen so the C header always matches the Rust FFI signatures
4. The `rb_ensure` usage mirrors Ruby's `ensure` keyword to guarantee Rust iterator cleanup
5. String interning through the names/strings tables for massive memory savings on large codebases
6. Making `new` private on Declaration/Definition/Document/Reference to enforce that objects only come from the Graph

### Things I found most confusing:
1. The `Box::from_raw` + `mem::forget` pattern in `with_graph` -- took me a while to understand why it is needed
2. Ruby's `@@` class variable scoping (belonging to the enclosing class at definition site, not `self`) is genuinely surprising
3. The Makefile patching in `extconf.rb` -- understanding what gets replaced requires knowing what `mkmf` generates
4. The 4 levels of indirection for string lookup (reference -> name -> string_id -> string) was initially bewildering

### My understanding of the system (in simple terms):
Rubydex is a Ruby gem that indexes Ruby source code to build a "graph" of all declarations (classes, modules, methods, constants, variables) and their relationships. The heavy lifting is done in Rust for maximum performance on huge codebases (like Shopify's). The Rust code parses Ruby files, extracts definitions, resolves fully-qualified names, and builds class hierarchies. All of this is exposed to Ruby through a C extension that acts as a thin bridge layer. Ruby programs interact with lightweight handle objects (just a graph reference + an integer ID) that look up real data from the Rust side on demand. The graph supports features like looking up a class by name, finding all definitions of a class across files, resolving constants through Ruby's lexical scoping rules, walking ancestor chains for method lookup, and searching declarations by prefix.
