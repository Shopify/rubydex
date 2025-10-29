# Ruby Language Behaviors

This document describes various Ruby language behaviors, compiled from observations in code analysis and testing.

## Table of Contents

1. [Namespace Qualification](#namespace-qualification)
2. [Lexical Scoping and Constant Resolution](#lexical-scoping-and-constant-resolution)
3. [Definitions vs Declarations](#definitions-vs-declarations)
4. [Method Parameters](#method-parameters)
5. [Attribute Methods](#attribute-methods)
6. [Variable Scoping](#variable-scoping)
7. [Constant References](#constant-references)

## Namespace Qualification

Ruby uses `::` as the namespace separator and supports various ways of referencing namespaced entities.

### Name Qualification Context

The same unqualified name refers to different entities depending on context:

```ruby
class Bar  # Top-level Bar
end

module Foo
  class Bar  # Foo::Bar
  end

  puts Bar # Inside Foo, "Bar" means "Foo::Bar"
end

puts Bar # Outside Foo, "Bar" means "Bar" (top-level)
```

### Unqualified vs Qualified Names

**Unqualified names** are simple identifiers:

- `Foo`
- `Bar`
- `String`

**Qualified names** include namespace separators:

- `Foo::Bar`
- `ActiveRecord::Base`
- `Rails::Application::Configuration`

**Top-level references** start with `::`:

- `::Foo` (explicitly top-level)
- `::Array` (explicitly top-level)

**Rooted qualified names** combine both (starting with `::` and including namespace separators):

- `::ActiveRecord::Base` (explicitly top-level ActiveRecord, then Base within it)
- `::Rails::Application::Configuration` (navigates from top level through the namespace chain)

### Singleton Method Naming

Singleton methods (class methods) can use the same name as instance methods:

```ruby
class Foo
  def bar; end       # Instance method Foo#bar
  def self.bar; end  # Singleton method Foo.bar (or Foo::bar)
end
```

Both methods can coexist because they operate on different receivers:

- `Foo.new.bar` calls the instance method
- `Foo.bar` calls the singleton method

## Lexical Scoping and Constant Resolution

### Fully Qualified Name Resolution Rules

Ruby resolves names according to the current scope:

```ruby
class Bar; end
class Qux; end

module Foo
  class Bar; end     # Defines Foo::Bar
  # "Bar" => "Foo::Bar"
  # "::Bar" => "Bar" (top-level)

  class Baz
    class Qux; end   # Defines Foo::Baz::Qux
    # "Bar" => "Foo::Bar"
    # "Qux" => "Foo::Baz::Qux"
    # "::Qux" => "Qux" (top-level)
  end
end
```

**Rules:**

1. If the name starts with `::`, it refers to a top-level constant
2. Otherwise, the name is qualified relative to the current namespace
3. At the top level, unqualified names remain unqualified

### Top-Level References with `::`

Ruby's top-level constant resolution operator `::` resets the namespace to the root, but **lexical nesting is still preserved** for constant lookup. This is a critical and subtle behavior.

**Example:**

```ruby
module Foo
  CONST = 42

  class ::Bar
    CONST # resolves to Foo::CONST because we're inside the `Foo` namespace
  end
end

class Bar
  CONST # NameError because this `Bar` is not lexically inside `Foo`
end
```

**Key Points:**

- The class `::Bar` defined inside `Foo` has the fully qualified name `Bar` (not `Foo::Bar`)
- However, it is **still lexically connected** to `Foo` for constant resolution
- The second `Bar` definition (outside `Foo`) has no lexical connection to `Foo`, therefore needs to access `CONST` via `Foo::CONST`

### Compact Namespace Notation

Ruby supports compact namespace notation like `class Foo::Bar::Baz`, which creates multiple nesting levels in one declaration.

**Example:**

```ruby
class Foo::Bar
  class Baz::Qux; end
  class ::Quuux; end
end
```

This creates:

- `Foo::Bar`
- `Foo::Bar::Baz::Qux` (nested under `Foo::Bar`)
- `Quuux` (top-level, but lexically inside `Foo::Bar`)

**Important:** `Foo::Bar::Baz::Qux::Quuux` does **not** exist because `::Quuux` resets to top level.

## Definitions vs Declarations

A fundamental concept in Ruby is that classes and modules can be reopened and defined multiple times.

**Example:**

```ruby
module Foo
  class Bar; end
end

class Foo::Bar; end
```

This code creates:

**3 separate definitions:**

1. Module definition for `Foo`
2. Class definition for `Foo::Bar` (inside `Foo`)
3. Class definition for `Foo::Bar` (at top level)

**2 distinct declarations:**

1. The module `Foo`
2. The class `Foo::Bar`

**Key Points:**

- A **definition** is a single definition occurrence of a class, module, method, etc. in a specific file at a specific location
- A **declaration** represents the named entity that may have multiple definitions

## Method Parameters

Ruby has a rich and expressive parameter system with multiple parameter types.

### Parameter Types

```ruby
def method_name(
  a,           # Required positional parameter
  b = 42,      # Optional positional parameter (with default)
  *c,          # Rest positional parameter (splat)
  d,           # Post parameter (required positional after rest)
  e:,          # Required keyword parameter
  f: 42,       # Optional keyword parameter (with default)
  **g,         # Rest keyword parameter (double splat)
  &h           # Block parameter
)
end
```

**Complete Example:**

```ruby
def foo(a, b = 42, *c, d, e:, g: 42, **i, &j); end
```

This demonstrates:

- `a`: required positional parameter
- `b = 42`: optional positional parameter with default value
- `*c`: rest positional (captures remaining positional args as an array)
- `d`: post parameter (required positional after rest)
- `e:`: required keyword parameter
- `g: 42`: optional keyword parameter with default value
- `**i`: rest keyword (captures remaining keyword args as a hash)
- `&j`: block parameter

### Forward Parameters

```ruby
def foo(...) # Forwards all arguments to bar
  bar(...)
end
```

This captures and forwards all positional, keyword, and block arguments.

### Singleton Methods

Methods can be defined on individual objects or as class methods:

```ruby
class Foo
  def bar; end       # Instance method
  def self.baz; end  # Singleton method (class method)
end
```

Results in:

- `Foo#bar` - instance method
- `Foo.baz` or `Foo::baz` - singleton method

## Attribute Methods

Ruby provides special methods for creating getter and setter methods automatically.

**Important:** Attribute methods (`attr`, `attr_accessor`, `attr_reader`, `attr_writer`) can only be used inside class or module definitions. They are not allowed at the top level.

### Basic Attribute Methods

```ruby
class Foo
  attr_accessor :foo   # Creates both `foo` and `foo=` methods
  attr_reader :bar     # Creates only `bar` method (getter)
  attr_writer :baz     # Creates only `baz=` method (setter)
end

# NOT allowed:
# attr_accessor :top_level  # Error: undefined method `attr_accessor'
```

### Multiple Attributes

All attribute methods accept multiple symbols:

```ruby
class Foo
  attr_accessor :bar, :baz
end
```

Creates: `bar`, `bar=`, `baz`, `baz=`

### Receiver Context

Attribute methods only work when called on `self` (implicit or explicit):

```ruby
class Foo
  attr_accessor :foo           # Works (implicit self)
  self.attr_accessor :qux      # Works (explicit self)
  foo.attr_accessor :ignored   # Does NOT work (different receiver)
end
```

### The `attr` Method with Parameters

The `attr` method has special behavior based on its arguments.

**Rules:**

1. **No second parameter:** Creates only reader

   ```ruby
   class Foo
     attr :foo   # Creates only foo (getter)
   end
   ```

2. **Second parameter is `false`:** Creates only reader

   ```ruby
   class Foo
     attr :foo, false  # Creates only foo (getter)
   end
   ```

3. **Second parameter is `true`:** Creates both reader and writer

   ```ruby
   class Foo
     attr :foo, true   # Creates both foo and foo= (getter and setter)
   end
   ```

4. **Multiple symbols/strings without second parameter:** Creates only readers for all arguments

   ```ruby
   class Foo
     attr :foo, :bar, :baz
     # Creates only: foo, bar, baz (readers only)

     attr :a, "b", "c"
     # Creates only: a, b, c (readers only)
   end
   ```

5. **Invalid second parameter:** Raises runtime error

   ```ruby
   class Foo
     attr :foo, 123  # Raises `123 is not a symbol nor a string (TypeError)`
   end
   ```

## Variable Scoping

Ruby has three types of variables distinguished by sigils: global (`$`), instance (`@`), and class (`@@`).

### Global Variables

Global variables are **not** namespaced and exist globally regardless of where they're defined:

```ruby
$foo = 1

class Foo
  $bar = 2
end
```

Both `$foo` and `$bar` are accessible globally. The class definition doesn't affect the scope of `$bar`.

### Instance Variables

Instance variables are scoped to instances of classes. When defined at the class level, they belong to the class itself (as the class is an object):

```ruby
@foo = 1           # Top-level instance variable (belongs to <main>)

class Foo
  @bar = 2         # Class instance variable (belongs to Foo object)

  def initialize
    @baz = 3       # Instance variable (belongs to instances of Foo)
  end
end
```

### Class Variables

Class variables are shared across a class and all its instances:

```ruby
class Foo
  @@bar = 2         # Class variable for Foo

  def self.bar
    @@bar
  end

  def bar
    @@bar           # Same @@bar accessible in instance methods
  end
end

# NOT allowed:
# @@foo = 1  # Error: class variable access from toplevel
```

### Multi-Assignment (Destructuring)

All variable types support multi-assignment:

```ruby
# Constants
FOO, BAR::BAZ = 1, 2

# Global variables
$foo, $bar, $baz = 1, 2, 3

# Instance variables
@foo, @bar = 1, 2

# Class variables
@@foo, @@bar = 1, 2

# Mixed with top-level references
FOO, BAR::BAZ, ::BAZ = 3, 4, 5
```

## Constant References

Constants in Ruby can be referenced before they're defined, and resolution depends on lexical scope.

### Unresolved References

A constant reference may not immediately resolve to a definition:

```ruby
module Foo
  BAR  # Could be Foo::BAR or top-level BAR - depends on what exists
end
```

Resolution depends on:

1. What constants are defined
2. The lexical scope chain
3. Whether it's a top-level reference (`::BAR`)

### Top-Level Constant References

Constants prefixed with `::` always refer to top-level constants:

```ruby
module Foo
  ::Bar  # Always top-level Bar, never Foo::Bar
  Baz    # Could be Foo::Baz or top-level Baz (searches lexical chain)
end
```

### Resolution Examples

```ruby
module Foo
  class Bar
    ::Bar      # Resolves to top-level Bar (if it exists)
    ::Baz      # Resolves to top-level Baz (if it exists)
    String     # Searches: Foo::Bar::String, Foo::String, ::String
    ::Object   # Resolves to top-level Object
  end
end

class Bar
  ::Foo::Bar   # Explicit path to Foo::Bar
end
```

**Resolution order for unqualified constants (like `String` above):**

1. Current namespace (Foo::Bar::String)
2. Each enclosing lexical scope (Foo::String)
3. Top-level (::String)

### Constant Resolution Through Inheritance

Ruby also searches the inheritance chain when resolving constants:

```ruby
class Parent
  CONST = "from parent"
end

class Child < Parent
  def show
    CONST  # Resolves to Parent::CONST through inheritance
  end
end

Child.new.show  # => "from parent"
```

**Resolution order with inheritance:**

1. Current class
2. Prepended modules (in reverse order of prepending)
3. Included modules (in reverse order of inclusion)
4. Parent class
5. Parent's prepended and included modules
6. Continue up the inheritance chain
7. Top-level

**Example with modules:**

```ruby
module M
  CONST = "from module"
end

class Parent
  CONST = "from parent"
end

class Child < Parent
  include M

  def show
    CONST  # Resolves to M::CONST (included module takes precedence)
  end
end
```

### Constant Paths

Ruby supports qualified constant paths:

```ruby
module Foo
  class Bar
    Object::String  # Explicit path: Object must exist, then String within it
  end
end
```

Constant paths are resolved left-to-right: `Object` is resolved first, then `String` is looked up within `Object`.
