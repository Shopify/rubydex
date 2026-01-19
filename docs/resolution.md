# Resolution

Resolution combines the outputs of the extraction phase to create a global semantic representation of the codebase.
This is the step that tries to understand the resulting structure of everything defined in a project, including all
relationships and connections.

**What Resolution does:**

- Computes fully qualified names for all definitions
- Creates `Declaration` objects that group definitions by fully qualified name
- Resolves constant references to their target declarations
- Linearizes ancestor chains (including resolving mixins)
- Keeps track of descendants
- Assigns semantic membership (which methods/constants belong to which class)
- Creates implicit singleton classes (e.g.: `def self.method` patterns or class instance variables)

## The resolution loop

### The problem of interdependencies

To create global declarations, we need to fully qualify all names extracted from a codebase. However, determining the
fully qualified name depends on resolving constants. Consider the same example as the one in
[extraction](extraction.md).

```ruby
# bar.rb
module Bar; end

# baz.rb
class Foo
  class Bar::Baz
    def qux; end
  end
end
```

The fully qualified name of the class defined in `baz.rb` is `Bar::Baz`. Therefore, the fully qualified name of the
method is `Bar::Baz#qux`. We can only determine that correctly if we already resolved the `Bar` constant reference
involved in the class' name.

To further increase the complexity, constants have interdependencies. To resolve a constant reference, we need to:

- Search the surrounding lexical scopes
- Search the ancestor chain of the lexical scope where the reference was found
- Fall back to the top level

Considering that other constants are involved in the lexical scopes and ancestor chains, you get even more cross
dependencies. We can even have dependencies within the same ancestor chain. Consider this other example:

```ruby
module Foo
  module Bar
  end
end

class Baz
  include Foo
  include Bar
end
```

When we include `Foo`, it makes `Bar` available through inheritance, which then allows us to also include it. This
means that in order to fully resolve this example we need to:

- Create global declarations for `Foo`, `Bar` and `Baz`
- Correctly assign membership that `Bar` is owned by `Foo`
- Partially linearized the ancestor chain of `Baz`, leaving a todo for `Bar` since we can't yet resolve it
- Finally, fully linearize the ancestors now that we processed `Foo` and know `Bar` is available through inheritance

### The loop

Trying to create a tree of dependencies ahead of time to resolve constants is difficult because some dependencies are
only identified when we are in the middle of performing resolution. Instead of taking that approach, the resolution
loop is an optimistically sorted worklist algorithm (inspired by [Sorbet's](https://github.com/sorbet/sorbet) approach).

The basic idea is to sort the worklist in an order that's likely to succeed most of the time, minimizing the amount of
times we need to retry. If we fail to resolve something, we re-enqueue to try again. The loop has passes (or epochs)
where we go through the list of work. If we exhaust the worklist or fail to make any progress in a pass, then we
finalize the loop.

```rust
// See the actual implementation in resolution.rs

pub fn resolve_all(graph: &mut Graph) {
    // Partition and sort all of the work ahead of time
    let (mut unit_queue, other_ids) = sorted_units(graph);

    // Outer loop that controls the passes
    loop {
        // Keep track if we made progress this pass
        let mut made_progress = false;

        // Resolution pass. We go through the full length of the queue at this time, which automatically excludes
        // retries that we find during this pass
        for _ in 0..unit_queue.len() {
            let Some(unit_id) = unit_queue.pop_front() else {
                break;
            };

            // Perform different work dependending on what item we found
            match unit_id {
                Unit::Definition(id) => { /* handle constant definitions */ }
                Unit::Reference(id) => { /* handle constant references */ }
                Unit::Ancestors(id) => { /* handle ancestor linearization retries */ }
            }

            // If we're no longer able to advance the analysis or if we finished all of the work, break out of the loop
            if !made_progress || unit_queue.is_empty() {
                break;
            }
        }
    }
}
```
