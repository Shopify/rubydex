# Indexer Plugins - Stage 2 Option 3: AST Subset Serialization

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Prototype AST subset approach where Rust serializes relevant AST fragments for Ruby plugins to walk with full tree structure.

**Architecture:** Rust indexer identifies DSL-relevant subtrees (class/module bodies with method calls), serializes them to a walkable format, and Ruby plugins receive mini-ASTs they can traverse with visitor pattern.

**Tech Stack:** Rust (rubydex, rubydex-sys), C extension, Ruby, JSON for serialization

**Prerequisites:** Stage 1 complete (Declaration#members, Graph#add_method, Graph#add_class)

**Worktree:** `saturn-plugin-ast`

---

## Overview

```
┌─────────────────────────────────────────────────────────────┐
│  Rust: Parallel file processing                             │
│                                                             │
│  For each file:                                             │
│  - Walk AST normally for indexing                           │
│  - Identify "interesting" subtrees (class bodies with calls)│
│  - Serialize those subtrees to JSON with structure intact   │
│  - Store serialized fragments                               │
└─────────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│  Ruby: Process AST fragments                                │
│                                                             │
│  graph.each_ast_fragment do |file_path, fragment|           │
│    fragment.walk do |node|                                  │
│      case node.type                                         │
│      when :call                                             │
│        handle_potential_dsl(node)                           │
│      when :class, :module                                   │
│        # Track nesting                                      │
│      end                                                    │
│    end                                                      │
│  end                                                        │
└─────────────────────────────────────────────────────────────┘
```

**Key difference from Option 1:** Plugins get full tree structure and can walk it themselves, rather than receiving a flat list of events.

---

## Task 1: Setup worktree

**Step 1: Create worktree from explore-plugin-api branch**

Run: `git worktree add ../saturn-plugin-ast -b plugin-ast-prototype explore-plugin-api`

**Step 2: Navigate to worktree**

Run: `cd ../saturn-plugin-ast`

**Step 3: Verify setup**

Run: `git log -1 --oneline && pwd`
Expected: Shows latest commit from explore-plugin-api, path ends with saturn-plugin-ast

---

## Task 2: Define AST node serialization format

**Files:**
- Create: `rust/rubydex/src/model/ast_fragment.rs`
- Modify: `rust/rubydex/src/model/mod.rs`

**Step 1: Create the AST fragment structs**

```rust
// rust/rubydex/src/model/ast_fragment.rs

use crate::model::ids::UriId;
use serde::{Deserialize, Serialize};

/// A serialized AST node for plugin consumption
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum AstNode {
    #[serde(rename = "class")]
    Class {
        name: String,
        superclass: Option<String>,
        body: Vec<AstNode>,
        location: NodeLocation,
    },
    #[serde(rename = "module")]
    Module {
        name: String,
        body: Vec<AstNode>,
        location: NodeLocation,
    },
    #[serde(rename = "call")]
    Call {
        name: String,
        receiver: Option<Box<AstNode>>,
        arguments: Vec<AstNode>,
        block: Option<Box<AstNode>>,
        location: NodeLocation,
    },
    #[serde(rename = "block")]
    Block {
        body: Vec<AstNode>,
        location: NodeLocation,
    },
    #[serde(rename = "symbol")]
    Symbol {
        value: String,
        location: NodeLocation,
    },
    #[serde(rename = "string")]
    String {
        value: String,
        location: NodeLocation,
    },
    #[serde(rename = "other")]
    Other {
        kind: String,
        location: NodeLocation,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NodeLocation {
    pub line: u32,
    pub column: u32,
    pub start_offset: u32,
    pub end_offset: u32,
}

impl NodeLocation {
    pub fn new(line: u32, column: u32, start_offset: u32, end_offset: u32) -> Self {
        Self { line, column, start_offset, end_offset }
    }
}

/// A fragment of AST from a file that may contain DSL calls
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AstFragment {
    pub file_path: String,
    pub root: AstNode,
}

/// Collection of AST fragments for a file
#[derive(Debug, Default)]
pub struct FileAstFragments {
    pub uri_id: UriId,
    pub file_path: String,
    pub fragments: Vec<AstFragment>,
}

impl FileAstFragments {
    pub fn new(uri_id: UriId, file_path: String) -> Self {
        Self {
            uri_id,
            file_path,
            fragments: Vec::new(),
        }
    }

    pub fn push(&mut self, fragment: AstFragment) {
        self.fragments.push(fragment);
    }

    pub fn is_empty(&self) -> bool {
        self.fragments.is_empty()
    }

    /// Serialize all fragments to JSON
    pub fn to_json(&self) -> String {
        serde_json::to_string(&self.fragments).unwrap_or_else(|_| "[]".to_string())
    }
}
```

**Step 2: Add serde dependency to Cargo.toml**

```toml
# rust/rubydex/Cargo.toml (add to dependencies)
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

**Step 3: Add to model module**

```rust
// rust/rubydex/src/model/mod.rs (add line)
pub mod ast_fragment;
```

**Step 4: Compile to verify**

Run: `cargo build`
Expected: Compiles without errors

**Step 5: Commit**

```bash
git add rust/rubydex/src/model/ast_fragment.rs rust/rubydex/src/model/mod.rs rust/rubydex/Cargo.toml
git commit -m "feat: add AstFragment structs for serialized AST subsets"
```

---

## Task 3: Build AST serializer

**Files:**
- Create: `rust/rubydex/src/indexing/ast_serializer.rs`
- Modify: `rust/rubydex/src/indexing/mod.rs`

**Step 1: Create AST serializer**

```rust
// rust/rubydex/src/indexing/ast_serializer.rs

use crate::model::ast_fragment::{AstFragment, AstNode, NodeLocation};
use ruby_prism::{Node, Visit};

/// Serializes Prism AST nodes to our JSON-friendly format
pub struct AstSerializer<'a> {
    source: &'a str,
    file_path: String,
}

impl<'a> AstSerializer<'a> {
    pub fn new(source: &'a str, file_path: String) -> Self {
        Self { source, file_path }
    }

    /// Serialize a class or module body that may contain DSL calls
    pub fn serialize_namespace_body(&self, node: &Node) -> Option<AstFragment> {
        let ast_node = self.serialize_node(node)?;
        Some(AstFragment {
            file_path: self.file_path.clone(),
            root: ast_node,
        })
    }

    fn serialize_node(&self, node: &Node) -> Option<AstNode> {
        match node {
            Node::ClassNode { .. } => self.serialize_class(node.as_class_node().unwrap()),
            Node::ModuleNode { .. } => self.serialize_module(node.as_module_node().unwrap()),
            Node::CallNode { .. } => self.serialize_call(node.as_call_node().unwrap()),
            Node::BlockNode { .. } => self.serialize_block(node.as_block_node().unwrap()),
            Node::SymbolNode { .. } => self.serialize_symbol(node.as_symbol_node().unwrap()),
            Node::StringNode { .. } => self.serialize_string(node.as_string_node().unwrap()),
            _ => Some(AstNode::Other {
                kind: format!("{:?}", node).split('{').next().unwrap_or("Unknown").to_string(),
                location: self.node_location(&node.location()),
            }),
        }
    }

    fn serialize_class(&self, node: &ruby_prism::ClassNode) -> Option<AstNode> {
        let name = self.location_to_string(&node.constant_path().location());
        let superclass = node.superclass().map(|s| self.location_to_string(&s.location()));

        let body = if let Some(body) = node.body() {
            self.serialize_statements(&body)
        } else {
            Vec::new()
        };

        Some(AstNode::Class {
            name,
            superclass,
            body,
            location: self.node_location(&node.location()),
        })
    }

    fn serialize_module(&self, node: &ruby_prism::ModuleNode) -> Option<AstNode> {
        let name = self.location_to_string(&node.constant_path().location());

        let body = if let Some(body) = node.body() {
            self.serialize_statements(&body)
        } else {
            Vec::new()
        };

        Some(AstNode::Module {
            name,
            body,
            location: self.node_location(&node.location()),
        })
    }

    fn serialize_call(&self, node: &ruby_prism::CallNode) -> Option<AstNode> {
        let name = node.name().to_string();

        let receiver = node.receiver().and_then(|r| self.serialize_node(&r)).map(Box::new);

        let arguments = if let Some(args) = node.arguments() {
            args.arguments()
                .iter()
                .filter_map(|arg| self.serialize_node(&arg))
                .collect()
        } else {
            Vec::new()
        };

        let block = node.block().and_then(|b| self.serialize_node(&b)).map(Box::new);

        Some(AstNode::Call {
            name,
            receiver,
            arguments,
            block,
            location: self.node_location(&node.location()),
        })
    }

    fn serialize_block(&self, node: &ruby_prism::BlockNode) -> Option<AstNode> {
        let body = if let Some(body) = node.body() {
            self.serialize_statements(&body)
        } else {
            Vec::new()
        };

        Some(AstNode::Block {
            body,
            location: self.node_location(&node.location()),
        })
    }

    fn serialize_symbol(&self, node: &ruby_prism::SymbolNode) -> Option<AstNode> {
        let value = node.value_loc()
            .map(|loc| self.location_to_string(&loc))
            .unwrap_or_default();

        Some(AstNode::Symbol {
            value,
            location: self.node_location(&node.location()),
        })
    }

    fn serialize_string(&self, node: &ruby_prism::StringNode) -> Option<AstNode> {
        let value = String::from_utf8_lossy(node.unescaped()).to_string();

        Some(AstNode::String {
            value,
            location: self.node_location(&node.location()),
        })
    }

    fn serialize_statements(&self, node: &Node) -> Vec<AstNode> {
        match node {
            Node::StatementsNode { .. } => {
                let statements = node.as_statements_node().unwrap();
                statements.body()
                    .iter()
                    .filter_map(|stmt| self.serialize_node(&stmt))
                    .collect()
            }
            _ => vec![],
        }
    }

    fn node_location(&self, loc: &ruby_prism::Location) -> NodeLocation {
        NodeLocation::new(
            loc.start_line() as u32,
            loc.start_column() as u32,
            loc.start_offset() as u32,
            loc.end_offset() as u32,
        )
    }

    fn location_to_string(&self, loc: &ruby_prism::Location) -> String {
        String::from_utf8_lossy(loc.as_slice()).to_string()
    }
}
```

**Step 2: Add to indexing module**

```rust
// rust/rubydex/src/indexing/mod.rs (add line)
pub mod ast_serializer;
```

**Step 3: Compile**

Run: `cargo build`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add rust/rubydex/src/indexing/ast_serializer.rs rust/rubydex/src/indexing/mod.rs
git commit -m "feat: add AstSerializer to convert Prism AST to JSON format"
```

---

## Task 4: Capture AST fragments during indexing

**Files:**
- Modify: `rust/rubydex/src/indexing/ruby_indexer.rs`
- Modify: `rust/rubydex/src/indexing/local_graph.rs`
- Modify: `rust/rubydex/src/model/graph.rs`

**Step 1: Add AST fragment storage to LocalGraph**

```rust
// rust/rubydex/src/indexing/local_graph.rs

use crate::model::ast_fragment::{AstFragment, FileAstFragments};

// Add field to LocalGraph:
ast_fragments: FileAstFragments,

// Add to LocalGraph::new(uri, document):
ast_fragments: FileAstFragments::new(uri_id, uri.clone()),

// Add methods:
pub fn add_ast_fragment(&mut self, fragment: AstFragment) {
    self.ast_fragments.push(fragment);
}

pub fn take_ast_fragments(&mut self) -> FileAstFragments {
    std::mem::take(&mut self.ast_fragments)
}
```

**Step 2: Serialize class/module bodies in RubyIndexer**

```rust
// rust/rubydex/src/indexing/ruby_indexer.rs

use crate::indexing::ast_serializer::AstSerializer;

// In visit_class_node, after processing the class:
fn visit_class_node(&mut self, node: &ruby_prism::ClassNode) {
    // ... existing indexing logic ...

    // Serialize this class body for potential DSL processing
    if self.should_serialize_for_plugins(node) {
        let serializer = AstSerializer::new(self.source, self.uri.clone());
        if let Some(fragment) = serializer.serialize_namespace_body(&Node::ClassNode { /* ... */ }) {
            self.local_graph.add_ast_fragment(fragment);
        }
    }

    // ... continue with children ...
}

// Add helper to determine if we should serialize
fn should_serialize_for_plugins(&self, node: &impl HasBody) -> bool {
    // Only serialize if the body contains method calls (potential DSLs)
    // This is a heuristic to avoid serializing everything
    if let Some(body) = node.body() {
        self.body_contains_calls(&body)
    } else {
        false
    }
}

fn body_contains_calls(&self, body: &Node) -> bool {
    // Check if any direct children are CallNodes
    match body {
        Node::StatementsNode { .. } => {
            body.as_statements_node().unwrap().body().iter().any(|stmt| {
                matches!(stmt, Node::CallNode { .. })
            })
        }
        _ => false,
    }
}
```

**Step 3: Store in Graph**

```rust
// rust/rubydex/src/model/graph.rs

use crate::model::ast_fragment::FileAstFragments;

// Add field:
ast_fragments: HashMap<UriId, FileAstFragments>,

// Add methods:
pub fn add_file_ast_fragments(&mut self, fragments: FileAstFragments) {
    if !fragments.is_empty() {
        self.ast_fragments.insert(fragments.uri_id, fragments);
    }
}

pub fn ast_fragments(&self) -> &HashMap<UriId, FileAstFragments> {
    &self.ast_fragments
}
```

**Step 4: Compile**

Run: `cargo build`
Expected: Compiles without errors

**Step 5: Commit**

```bash
git add rust/rubydex/src/indexing/ruby_indexer.rs rust/rubydex/src/indexing/local_graph.rs rust/rubydex/src/model/graph.rs
git commit -m "feat: capture and store AST fragments for DSL-heavy class bodies"
```

---

## Task 5: Expose AST fragments to Ruby via FFI

**Files:**
- Modify: `rust/rubydex-sys/src/graph_api.rs`
- Modify: `ext/rubydex/graph.c`
- Modify: `ext/rubydex/rustbindings.h`
- Create: `lib/rubydex/ast_fragment.rb`

**Step 1: Add FFI function to get JSON fragments**

```rust
// rust/rubydex-sys/src/graph_api.rs

/// Returns JSON-serialized AST fragments for a file
/// Caller must free the returned string with free_c_string
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_get_ast_fragments_json(
    pointer: GraphPointer,
    uri_id: i64,
) -> *const c_char {
    with_graph(pointer, |graph| {
        let uri_id = UriId::new(uri_id);
        if let Some(fragments) = graph.ast_fragments().get(&uri_id) {
            let json = fragments.to_json();
            CString::new(json).unwrap().into_raw().cast_const()
        } else {
            CString::new("[]").unwrap().into_raw().cast_const()
        }
    })
}

/// Iterator over files with AST fragments
pub struct AstFragmentFilesIter {
    uri_ids: Box<[i64]>,
    index: usize,
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_ast_fragment_files_iter_new(pointer: GraphPointer) -> *mut AstFragmentFilesIter {
    with_graph(pointer, |graph| {
        let uri_ids: Vec<i64> = graph
            .ast_fragments()
            .keys()
            .map(|id| **id)
            .collect();
        Box::into_raw(Box::new(AstFragmentFilesIter {
            uri_ids: uri_ids.into_boxed_slice(),
            index: 0,
        }))
    })
}

// ... iterator next/free functions similar to other iterators ...
```

**Step 2: Add C bindings**

```c
// ext/rubydex/graph.c

// Graph#each_ast_fragment: () { |file_path, json| } -> self
static VALUE sr_graph_each_ast_fragment(VALUE self) {
    if (!rb_block_given_p()) {
        return rb_enumeratorize(self, rb_str_new2("each_ast_fragment"), 0, NULL);
    }

    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    void *iter = rdx_graph_ast_fragment_files_iter_new(graph);
    int64_t uri_id;

    while (rdx_ast_fragment_files_iter_next(iter, &uri_id)) {
        // Get file path from document
        const char *path = rdx_document_path(graph, uri_id);
        VALUE rb_path = path ? rb_utf8_str_new_cstr(path) : Qnil;
        if (path) free_c_string(path);

        // Get JSON fragments
        const char *json = rdx_graph_get_ast_fragments_json(graph, uri_id);
        VALUE rb_json = json ? rb_utf8_str_new_cstr(json) : rb_str_new2("[]");
        if (json) free_c_string(json);

        rb_yield_values(2, rb_path, rb_json);
    }

    rdx_ast_fragment_files_iter_free(iter);
    return self;
}
```

**Step 3: Create Ruby wrapper for AST walking**

```ruby
# lib/rubydex/ast_fragment.rb
# frozen_string_literal: true

require "json"

module Rubydex
  class AstFragment
    attr_reader :file_path, :root

    def initialize(file_path, json_data)
      @file_path = file_path
      @root = AstNode.from_hash(JSON.parse(json_data))
    end

    def walk(&block)
      @root.walk(&block)
    end
  end

  class AstNode
    attr_reader :type, :location, :data

    def self.from_hash(hash)
      return nil unless hash.is_a?(Hash)

      type = hash["type"]&.to_sym
      location = hash["location"]
      data = hash.except("type", "location")

      new(type, location, data)
    end

    def initialize(type, location, data)
      @type = type
      @location = location
      @data = data
    end

    def name
      @data["name"]
    end

    def value
      @data["value"]
    end

    def arguments
      @data["arguments"]&.map { |a| AstNode.from_hash(a) } || []
    end

    def body
      @data["body"]&.map { |n| AstNode.from_hash(n) } || []
    end

    def block
      @data["block"] && AstNode.from_hash(@data["block"])
    end

    def has_block?
      !@data["block"].nil?
    end

    def walk(&block)
      yield self

      body.each { |node| node&.walk(&block) }
      arguments.each { |node| node&.walk(&block) }
      self.block&.walk(&block)
    end

    def line
      @location&.dig("line")
    end

    def column
      @location&.dig("column")
    end
  end
end
```

**Step 4: Compile and test**

Run: `chruby 3.4.3 && bundle exec rake compile`
Expected: Compiles without errors

**Step 5: Commit**

```bash
git add rust/rubydex-sys/src/graph_api.rs ext/rubydex/graph.c ext/rubydex/rustbindings.h lib/rubydex/ast_fragment.rb
git commit -m "feat: expose AST fragments to Ruby with JSON serialization"
```

---

## Task 6: Write integration test for AST subset approach

**Files:**
- Create: `test/integration/ast_subset_test.rb`

**Step 1: Write the test**

```ruby
# test/integration/ast_subset_test.rb
# frozen_string_literal: true

require "test_helper"
require "rubydex/ast_fragment"

class AstSubsetTest < Minitest::Test
  def setup
    @graph = Rubydex::Graph.new
  end

  def test_belongs_to_via_ast_walking
    @graph.index_all([fixture_path("belongs_to_example.rb")])

    @graph.each_ast_fragment do |file_path, json|
      fragments = JSON.parse(json)
      fragments.each do |fragment_data|
        fragment = Rubydex::AstFragment.new(file_path, fragment_data.to_json)

        current_class = nil

        fragment.walk do |node|
          case node.type
          when :class
            current_class = node.name
          when :call
            if node.name == "belongs_to" && current_class
              entity = node.arguments.first&.value
              if entity
                @graph.add_method(
                  owner: current_class,
                  name: entity,
                  file_path: file_path,
                  line: node.line,
                  column: node.column
                )
                @graph.add_method(
                  owner: current_class,
                  name: "#{entity}=",
                  file_path: file_path,
                  line: node.line,
                  column: node.column
                )
              end
            end
          end
        end
      end
    end

    @graph.resolve

    post = @graph["Post"]
    member_names = post.members.map(&:unqualified_name)
    assert_includes member_names, "author"
    assert_includes member_names, "author="
  end

  def test_rspec_let_via_ast_walking
    @graph.index_all([fixture_path("rspec_let_example.rb")])

    @graph.each_ast_fragment do |file_path, json|
      fragments = JSON.parse(json)
      fragments.each do |fragment_data|
        process_rspec_fragment(file_path, fragment_data)
      end
    end

    @graph.resolve

    calc_class = @graph["RSpec::ExampleGroups::Calculator"]
    refute_nil calc_class

    member_names = calc_class.members.map(&:unqualified_name)
    assert_includes member_names, "value"
  end

  private

  def process_rspec_fragment(file_path, fragment_data, context_stack = [])
    node = Rubydex::AstNode.from_hash(fragment_data)
    return unless node

    case node.type
    when :call
      case node.name
      when "describe", "context"
        class_name = "RSpec::ExampleGroups::#{sanitize_name(node.arguments.first&.value)}"
        parent = context_stack.last || "RSpec::Core::ExampleGroup"

        @graph.add_class(
          name: class_name,
          parent: parent,
          file_path: file_path,
          line: node.line || 1,
          column: node.column || 0
        )

        if node.has_block?
          node.block.body.each do |child|
            process_rspec_fragment(file_path, child.data.merge("type" => child.type.to_s), context_stack + [class_name])
          end
        end

      when "let", "let!", "subject"
        method_name = node.arguments.first&.value || "subject"
        owner = context_stack.last

        if owner
          @graph.add_method(
            owner: owner,
            name: method_name,
            file_path: file_path,
            line: node.line || 1,
            column: node.column || 0
          )
        end
      end
    end
  end

  def fixture_path(name)
    File.expand_path("../fixtures/#{name}", __dir__)
  end

  def sanitize_name(name)
    name.to_s.gsub(/[^a-zA-Z0-9]/, "_").gsub(/_+/, "_").capitalize
  end
end
```

**Step 2: Run test**

Run: `chruby 3.4.3 && bundle exec ruby -Itest test/integration/ast_subset_test.rb`
Expected: Tests pass (or reveal issues to fix)

**Step 3: Commit**

```bash
git add test/integration/ast_subset_test.rb
git commit -m "test: add integration tests for AST subset approach"
```

---

## Task 7: Measure and document performance

**Step 1: Create benchmark script**

```ruby
# benchmark/ast_subset_benchmark.rb
require "benchmark"
require "rubydex"

# Benchmark:
# 1. Indexing time with AST serialization
# 2. JSON serialization overhead
# 3. Ruby AST walking time
```

**Step 2: Compare with batched callbacks approach**

**Step 3: Document results**

**Step 4: Commit**

```bash
git add benchmark/
git commit -m "perf: add benchmarks for AST subset approach"
```

---

## Notes

1. **Serialization overhead** - JSON serialization adds CPU cost during indexing. Consider MessagePack or bincode for faster serialization if JSON proves too slow.

2. **Memory usage** - Storing full AST fragments uses more memory than the batched events approach. May need to serialize lazily or clear after processing.

3. **Plugin ergonomics** - The `AstNode#walk` API is simple but may need refinement. Consider adding:
   - `walk_calls_only` for DSL-focused plugins
   - `parent` references for context
   - Pattern matching helpers

4. **Partial serialization** - Currently serializes entire class bodies. Could optimize by only serializing subtrees that contain potential DSL calls.

5. **Comparison with Option 1:**
   - Option 1 (batched): Smaller data, but loses tree structure
   - Option 3 (AST subset): Full structure, but larger data and more complex processing
