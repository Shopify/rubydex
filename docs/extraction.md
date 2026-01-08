# Extraction

During extraction, source code is parsed into ASTs and key information is recorded to be transformed and used in
subsequent phases. The captured information is remembered as is, with no assumptions about runtime behavior or
semantics.

The intention is to be able to work backwards to the original code since the goal is to support many different tools.
As an example, considering an `unless` as an `if !` is generally correct for static analysis. However, it would not be
possible to write a linting rule prohibiting the use of `unless` if we transformed all of them into `if` statements
during this phase.

As a general rule, this phase tries to represent the code we found in documents with high fidelity. It's the
[resolution phase](resolution.md) that performs more meaningful transformations on the data.

**What Extraction does:**

- Creates `Definition` objects for classes, modules, methods, constants, variables
- Records source locations, comments, and lexical ownership (`owner_id`)
- Captures unresolved constant references (e.g., `Foo::Bar` as a `NameId`)
- Records mixins (`include`, `prepend`, `extend`) on their containing class/module

**What Extraction does NOT do:**

- Compute fully qualified names
- Resolve constant references to declarations
- Determine inheritance hierarchies
- Assign semantic membership

#### Why No Assumptions During Discovery?

Consider this example:

```ruby
# bar.rb
module Bar; end

# baz.rb
class Foo
  class Bar::Baz; end
end
```

When extracting information from `baz.rb`, it may seem that the class being created is `Foo::Bar::Baz`. However,
extraction only sees one document at a time and constants in Ruby are resolved globally, taking all of the information
from the entire codebase into account.

We can only discover that the class' true fully qualified name is actually `Bar::Baz` once we extracted information from
all of the files involved. This also affects constant ownership. At first glance, it seems that `Bar` is a member of the
`Foo` class, when in reality it is defined at the top level (and therefore a member of `Object`).
