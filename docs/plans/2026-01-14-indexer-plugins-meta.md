# Indexer Plugins - Meta Plan

> **For Claude:** This is a coordination document. See individual stage plans for implementation details.

**Goal:** Prototype two different approaches to indexer plugins and compare their trade-offs.

**Context:** [GitHub Issue #418](https://github.com/Shopify/rubydex/issues/418) - Explore indexer plugins for handling meta-programmed Ruby code (DSLs like Rails `belongs_to`, RSpec `let`/`subject`, ActiveSupport `class_methods`).

**Branch:** `explore-plugin-api` (Stage 1), worktrees for Stage 2 options.

---

## Problem Statement

Ruby meta-programming creates definitions that static analysis cannot see:

```ruby
class Post
  belongs_to :author  # Creates author, author= methods
end

module MyConcern
  class_methods do
    def foo; end  # Belongs to ClassMethods, not MyConcern
  end
end

RSpec.describe "thing" do
  let(:foo) { 1 }  # Creates foo method in describe's anonymous class
end
```

We need a plugin API that:
1. Allows tools to provide context about meta-programmed code
2. Has minimal performance impact on indexing
3. Preserves nesting context (critical for RSpec-style DSLs)

---

## Approaches to Prototype

### Option 1: Batched Callbacks

- Rust indexes files in parallel, collects DSL call events with nesting context
- After Rust phase completes, Ruby processes events per-file in tree order
- **Pro:** Rust parallelism preserved during heavy lifting
- **Con:** Two-phase adds complexity; Ruby phase is sequential

### Option 3: AST Subset Serialization

- Rust indexes normally, serializes relevant AST subtrees for DSL-heavy blocks
- Ruby plugins receive mini-ASTs they can walk with visitor pattern
- **Pro:** Full tree structure available to plugins
- **Con:** Serialization overhead; more complex plugin authoring

---

## Stage Dependencies

```
Stage 1: Ruby API Foundation (this branch)
    │
    │   Adds: Declaration#members, Graph#add_method, Graph#add_class
    │
    ├───────────────────────────┐
    ▼                           ▼
Stage 2 Option 1            Stage 2 Option 3
(saturn-plugin-batched)     (saturn-plugin-ast)
    │                           │
    └───────────────────────────┘
                │
                ▼
        Compare & Decide
```

---

## Test Scenarios

All prototypes must demonstrate these cases:

### Scenario 1: `belongs_to :author`
```ruby
class Post
  belongs_to :author
end
# Expected: Post#author, Post#author= methods indexed
```

### Scenario 2: `class_methods` block
```ruby
module MyConcern
  class_methods do
    def foo; end
  end
end
# Expected: MyConcern::ClassMethods#foo indexed
```

### Scenario 3: RSpec `let`/`subject`
```ruby
RSpec.describe "Calculator" do
  subject { Calculator.new }
  let(:value) { 42 }

  context "when adding" do
    let(:other) { 10 }
  end
end
# Expected:
# - Anonymous class for "Calculator" with subject, value methods
# - Nested anonymous class for "when adding" with other method (inherits subject, value)
```

---

## Plan Documents

| Plan | Purpose |
|------|---------|
| [Stage 1](2026-01-14-indexer-plugins-stage1.md) | Ruby API foundation (insert + query) |
| [Stage 2 Option 1](2026-01-14-indexer-plugins-stage2-opt1.md) | Batched callbacks prototype |
| [Stage 2 Option 3](2026-01-14-indexer-plugins-stage2-opt3.md) | AST subset prototype |

---

## Success Criteria

After prototyping:
1. Both approaches can handle all 3 test scenarios
2. We have performance measurements for comparison
3. We understand the ergonomics/complexity trade-offs
4. We can make an informed recommendation for the production implementation
