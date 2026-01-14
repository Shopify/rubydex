# Indexer Plugins - Stage 2 Option 1: Batched Callbacks

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Prototype batched callbacks approach where Rust collects DSL events during indexing, then Ruby processes them per-file after indexing completes.

**Architecture:** Rust indexer captures "potential DSL calls" with nesting context during parallel file processing. After indexing, Ruby receives batched events per file and can insert definitions using Stage 1 APIs.

**Tech Stack:** Rust (rubydex, rubydex-sys), C extension, Ruby

**Prerequisites:** Stage 1 complete (Declaration#members, Graph#add_method, Graph#add_class)

**Worktree:** `saturn-plugin-batched`

---

## Overview

```
┌─────────────────────────────────────────────────────────────┐
│  Rust: Parallel file processing                             │
│                                                             │
│  For each file:                                             │
│  - Walk AST normally                                        │
│  - When encountering CallNode, check if potential DSL       │
│  - Capture: method_name, arguments, nesting_stack, location │
│  - Store in per-file event list                             │
└─────────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│  Ruby: Process batched events                               │
│                                                             │
│  graph.process_dsl_events do |file_path, events|            │
│    events.each do |event|                                   │
│      case event.method_name                                 │
│      when "belongs_to"                                      │
│        # Insert author, author= methods                     │
│      when "let", "subject"                                  │
│        # Insert method in current RSpec context             │
│      end                                                    │
│    end                                                      │
│  end                                                        │
└─────────────────────────────────────────────────────────────┘
```

---

## Task 1: Setup worktree

**Step 1: Create worktree from explore-plugin-api branch**

Run: `git worktree add ../saturn-plugin-batched -b plugin-batched-prototype explore-plugin-api`

**Step 2: Navigate to worktree**

Run: `cd ../saturn-plugin-batched`

**Step 3: Verify setup**

Run: `git log -1 --oneline && pwd`
Expected: Shows latest commit from explore-plugin-api, path ends with saturn-plugin-batched

---

## Task 2: Define DSL event data structure in Rust

**Files:**
- Create: `rust/rubydex/src/model/dsl_event.rs`
- Modify: `rust/rubydex/src/model/mod.rs`

**Step 1: Create the DSL event struct**

```rust
// rust/rubydex/src/model/dsl_event.rs

use crate::model::ids::{StringId, UriId};
use crate::offset::Offset;

/// Represents a potential DSL call captured during indexing
#[derive(Debug, Clone)]
pub struct DslEvent {
    /// The name of the method being called (e.g., "belongs_to", "let", "describe")
    pub method_name: StringId,
    /// The arguments as string representations
    pub arguments: Vec<StringId>,
    /// The nesting stack at the time of the call (fully qualified owner names)
    pub nesting_stack: Vec<String>,
    /// Source location
    pub offset: Offset,
    /// Whether this call has a block
    pub has_block: bool,
}

impl DslEvent {
    pub fn new(
        method_name: StringId,
        arguments: Vec<StringId>,
        nesting_stack: Vec<String>,
        offset: Offset,
        has_block: bool,
    ) -> Self {
        Self {
            method_name,
            arguments,
            nesting_stack,
            offset,
            has_block,
        }
    }
}

/// Collection of DSL events for a single file
#[derive(Debug, Default)]
pub struct FileDslEvents {
    pub uri_id: UriId,
    pub events: Vec<DslEvent>,
}

impl FileDslEvents {
    pub fn new(uri_id: UriId) -> Self {
        Self {
            uri_id,
            events: Vec::new(),
        }
    }

    pub fn push(&mut self, event: DslEvent) {
        self.events.push(event);
    }

    pub fn is_empty(&self) -> bool {
        self.events.is_empty()
    }
}
```

**Step 2: Add to model module**

```rust
// rust/rubydex/src/model/mod.rs (add line)
pub mod dsl_event;
```

**Step 3: Compile to verify**

Run: `cargo build`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add rust/rubydex/src/model/dsl_event.rs rust/rubydex/src/model/mod.rs
git commit -m "feat: add DslEvent struct for capturing potential DSL calls"
```

---

## Task 3: Capture DSL events during indexing

**Files:**
- Modify: `rust/rubydex/src/indexing/ruby_indexer.rs`
- Modify: `rust/rubydex/src/indexing/local_graph.rs`

**Step 1: Add DSL event storage to LocalGraph**

```rust
// rust/rubydex/src/indexing/local_graph.rs (add field and methods)

use crate::model::dsl_event::{DslEvent, FileDslEvents};

// Add to LocalGraph struct:
dsl_events: FileDslEvents,

// Add to LocalGraph::new():
dsl_events: FileDslEvents::new(uri_id),

// Add methods:
pub fn add_dsl_event(&mut self, event: DslEvent) {
    self.dsl_events.push(event);
}

pub fn take_dsl_events(&mut self) -> FileDslEvents {
    std::mem::take(&mut self.dsl_events)
}
```

**Step 2: Capture potential DSL calls in RubyIndexer**

In `ruby_indexer.rs`, modify `visit_call_node` to capture potential DSL calls:

```rust
// rust/rubydex/src/indexing/ruby_indexer.rs
// In the Visit impl, find or add visit_call_node method

fn visit_call_node(&mut self, node: &ruby_prism::CallNode) {
    // Existing logic for handling known method calls...

    // NEW: Capture potential DSL calls (method calls without receiver in class/module body)
    if node.receiver().is_none() {
        let method_name = node.name();
        let method_str = Self::location_to_string(&node.message_loc().unwrap_or(node.location()));

        // Only capture if we're inside a class/module body (not inside a method)
        if self.is_in_namespace_body() {
            let method_name_id = self.local_graph.intern_string(method_str);

            // Capture arguments
            let arguments = self.capture_call_arguments(node);

            // Capture current nesting stack
            let nesting_stack = self.capture_nesting_stack();

            let event = DslEvent::new(
                method_name_id,
                arguments,
                nesting_stack,
                Offset::from_prism_location(&node.location()),
                node.block().is_some(),
            );

            self.local_graph.add_dsl_event(event);
        }
    }

    // Continue visiting children
    ruby_prism::visit_call_node(self, node);
}

// Helper methods to add:
fn is_in_namespace_body(&self) -> bool {
    // Check if we're directly inside a class/module, not inside a method
    match self.nesting_stack.last() {
        Some(Nesting::LexicalScope(_)) | Some(Nesting::Owner(_)) => true,
        _ => false,
    }
}

fn capture_call_arguments(&mut self, node: &ruby_prism::CallNode) -> Vec<StringId> {
    let mut args = Vec::new();
    if let Some(arguments) = node.arguments() {
        for arg in &arguments.arguments() {
            match arg {
                ruby_prism::Node::SymbolNode { .. } => {
                    if let Some(symbol) = arg.as_symbol_node() {
                        if let Some(value_loc) = symbol.value_loc() {
                            let value = Self::location_to_string(&value_loc);
                            args.push(self.local_graph.intern_string(value));
                        }
                    }
                }
                ruby_prism::Node::StringNode { .. } => {
                    if let Some(string) = arg.as_string_node() {
                        let value = String::from_utf8_lossy(string.unescaped()).to_string();
                        args.push(self.local_graph.intern_string(value));
                    }
                }
                _ => {
                    // For complex arguments, store a placeholder
                    args.push(self.local_graph.intern_string("<complex>".to_string()));
                }
            }
        }
    }
    args
}

fn capture_nesting_stack(&self) -> Vec<String> {
    self.nesting_stack
        .iter()
        .filter_map(|nesting| {
            match nesting {
                Nesting::LexicalScope(id) | Nesting::Owner(id) => {
                    self.local_graph.definitions().get(id).map(|def| {
                        // Get the name from the definition
                        // This needs to construct the FQN based on current nesting
                        format!("{:?}", def) // Placeholder - needs proper implementation
                    })
                }
                Nesting::Method(_) => None,
            }
        })
        .collect()
}
```

**Step 3: Compile and test**

Run: `cargo build && cargo test`
Expected: Compiles and tests pass

**Step 4: Commit**

```bash
git add rust/rubydex/src/indexing/ruby_indexer.rs rust/rubydex/src/indexing/local_graph.rs
git commit -m "feat: capture potential DSL calls during indexing"
```

---

## Task 4: Store DSL events in Graph

**Files:**
- Modify: `rust/rubydex/src/model/graph.rs`
- Modify: `rust/rubydex/src/indexing.rs`

**Step 1: Add DSL events storage to Graph**

```rust
// rust/rubydex/src/model/graph.rs (add field)

use crate::model::dsl_event::FileDslEvents;

// Add to Graph struct:
dsl_events: HashMap<UriId, FileDslEvents>,

// Add to Graph::new():
dsl_events: HashMap::new(),

// Add methods:
pub fn add_file_dsl_events(&mut self, events: FileDslEvents) {
    if !events.is_empty() {
        self.dsl_events.insert(events.uri_id, events);
    }
}

pub fn dsl_events(&self) -> &HashMap<UriId, FileDslEvents> {
    &self.dsl_events
}

pub fn take_dsl_events(&mut self) -> HashMap<UriId, FileDslEvents> {
    std::mem::take(&mut self.dsl_events)
}
```

**Step 2: Merge DSL events when merging local graphs**

```rust
// rust/rubydex/src/indexing.rs (in merge_local_graph or similar)

// After merging definitions, also merge DSL events:
let dsl_events = local_graph.take_dsl_events();
graph.add_file_dsl_events(dsl_events);
```

**Step 3: Compile**

Run: `cargo build`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add rust/rubydex/src/model/graph.rs rust/rubydex/src/indexing.rs
git commit -m "feat: store DSL events in Graph after indexing"
```

---

## Task 5: Expose DSL events to Ruby via FFI

**Files:**
- Modify: `rust/rubydex-sys/src/graph_api.rs`
- Modify: `ext/rubydex/graph.c`
- Modify: `ext/rubydex/rustbindings.h`

**Step 1: Create FFI functions for DSL event iteration**

```rust
// rust/rubydex-sys/src/graph_api.rs

/// Iterator over files with DSL events
pub struct DslFilesIter {
    uri_ids: Box<[i64]>,
    index: usize,
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_dsl_files_iter_new(pointer: GraphPointer) -> *mut DslFilesIter {
    with_graph(pointer, |graph| {
        let uri_ids: Vec<i64> = graph
            .dsl_events()
            .keys()
            .map(|id| **id)
            .collect();
        Box::into_raw(Box::new(DslFilesIter {
            uri_ids: uri_ids.into_boxed_slice(),
            index: 0,
        }))
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_dsl_files_iter_next(iter: *mut DslFilesIter, out_uri_id: *mut i64) -> bool {
    if iter.is_null() || out_uri_id.is_null() {
        return false;
    }
    let it = unsafe { &mut *iter };
    if it.index >= it.uri_ids.len() {
        return false;
    }
    unsafe { *out_uri_id = it.uri_ids[it.index] };
    it.index += 1;
    true
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_dsl_files_iter_free(iter: *mut DslFilesIter) {
    if !iter.is_null() {
        unsafe { let _ = Box::from_raw(iter); }
    }
}

/// Struct to return DSL event data to C
#[repr(C)]
pub struct DslEventData {
    pub method_name: *const c_char,
    pub arguments: *const *const c_char,
    pub arguments_len: usize,
    pub nesting_stack: *const *const c_char,
    pub nesting_len: usize,
    pub line: u32,
    pub column: u32,
    pub has_block: bool,
}

/// Iterator over DSL events for a specific file
pub struct DslEventsIter {
    // Implementation details
}

// Add similar FFI functions for iterating over events within a file
```

**Step 2: Add C bindings**

```c
// ext/rubydex/graph.c

// Graph#each_dsl_file: () { |uri_id, events| } -> self
static VALUE sr_graph_each_dsl_file(VALUE self) {
    // Implementation that yields file URI and array of events
}
```

**Step 3: Compile and test**

Run: `chruby 3.4.3 && bundle exec rake compile`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add rust/rubydex-sys/src/graph_api.rs ext/rubydex/graph.c ext/rubydex/rustbindings.h
git commit -m "feat: expose DSL events to Ruby via FFI"
```

---

## Task 6: Write integration test for batched callbacks

**Files:**
- Create: `test/integration/batched_callbacks_test.rb`

**Step 1: Write the test**

```ruby
# test/integration/batched_callbacks_test.rb
# frozen_string_literal: true

require "test_helper"

class BatchedCallbacksTest < Minitest::Test
  def setup
    @graph = Rubydex::Graph.new
  end

  def test_belongs_to_via_batched_events
    @graph.index_all([fixture_path("belongs_to_example.rb")])

    # Process DSL events
    @graph.each_dsl_file do |file_path, events|
      events.each do |event|
        if event.method_name == "belongs_to" && event.arguments.first
          entity = event.arguments.first
          owner = event.nesting_stack.last

          @graph.add_method(
            owner: owner,
            name: entity,
            file_path: file_path,
            line: event.line,
            column: event.column
          )
          @graph.add_method(
            owner: owner,
            name: "#{entity}=",
            file_path: file_path,
            line: event.line,
            column: event.column
          )
        end
      end
    end

    @graph.resolve

    post = @graph["Post"]
    member_names = post.members.map(&:unqualified_name)
    assert_includes member_names, "author"
    assert_includes member_names, "author="
  end

  def test_rspec_let_via_batched_events
    @graph.index_all([fixture_path("rspec_let_example.rb")])

    # Track RSpec context classes
    context_stack = []

    @graph.each_dsl_file do |file_path, events|
      events.each do |event|
        case event.method_name
        when "describe", "context"
          # Create anonymous class for this describe/context block
          class_name = "RSpec::ExampleGroups::#{sanitize_name(event.arguments.first)}"
          parent = context_stack.last || "RSpec::Core::ExampleGroup"

          @graph.add_class(
            name: class_name,
            parent: parent,
            file_path: file_path,
            line: event.line,
            column: event.column
          )

          context_stack.push(class_name) if event.has_block
        when "let", "let!", "subject"
          # Add method to current context
          method_name = event.arguments.first || "subject"
          owner = context_stack.last

          @graph.add_method(
            owner: owner,
            name: method_name,
            file_path: file_path,
            line: event.line,
            column: event.column
          ) if owner
        end
      end
    end

    @graph.resolve

    # Verify the methods were created in the right context
    calc_class = @graph["RSpec::ExampleGroups::Calculator"]
    refute_nil calc_class

    member_names = calc_class.members.map(&:unqualified_name)
    assert_includes member_names, "value"
  end

  private

  def fixture_path(name)
    File.expand_path("../fixtures/#{name}", __dir__)
  end

  def sanitize_name(name)
    name.to_s.gsub(/[^a-zA-Z0-9]/, "_").gsub(/_+/, "_").capitalize
  end
end
```

**Step 2: Create test fixture**

```ruby
# test/fixtures/rspec_let_example.rb
RSpec.describe "Calculator" do
  let(:value) { 42 }

  context "when adding" do
    let(:other) { 10 }
  end
end
```

**Step 3: Run test**

Run: `chruby 3.4.3 && bundle exec ruby -Itest test/integration/batched_callbacks_test.rb`
Expected: Tests pass (or reveal issues to fix)

**Step 4: Commit**

```bash
git add test/integration/batched_callbacks_test.rb test/fixtures/rspec_let_example.rb
git commit -m "test: add integration tests for batched callbacks approach"
```

---

## Task 7: Measure and document performance

**Step 1: Create benchmark script**

```ruby
# benchmark/batched_callbacks_benchmark.rb
require "benchmark"
require "rubydex"

# Benchmark indexing with and without DSL event capture
```

**Step 2: Run benchmarks and document results**

**Step 3: Commit benchmark and results**

```bash
git add benchmark/
git commit -m "perf: add benchmarks for batched callbacks approach"
```

---

## Notes

1. **Nesting context accuracy** - The `capture_nesting_stack` implementation is a placeholder. It needs to properly reconstruct fully qualified names from the definition stack.

2. **Event ordering** - Events are captured in tree traversal order, which preserves the nesting relationship. When Ruby processes them, it can maintain a stack to track context.

3. **Block enter/leave** - For RSpec-style DSLs, we need to know when we enter and leave blocks. Consider adding `DslBlockEnter` and `DslBlockLeave` events, or use the `has_block` flag with careful ordering.

4. **Performance measurement** - Compare:
   - Baseline: indexing without DSL capture
   - With DSL capture: additional overhead
   - Ruby processing time: sequential GVL-bound phase
