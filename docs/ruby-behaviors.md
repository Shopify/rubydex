# Ruby Language Behaviors

This document describes various Ruby language behaviors, compiled from observations in code analysis and testing.

## Table of Contents

1. [Namespace Qualification](#namespace-qualification)
2. [Lexical Scoping and Constant Resolution](#lexical-scoping-and-constant-resolution)
3. [Definitions vs Declarations](#definitions-vs-declarations)
4. [Method Parameters](#method-parameters)
5. [Method Visibility](#method-visibility)
6. [Method Aliasing](#method-aliasing)
7. [Attribute Methods](#attribute-methods)
8. [Variable Scoping](#variable-scoping)
9. [Mixins](#mixins)
10. [Constant References](#constant-references)
11. [Constant Aliases](#constant-aliases)
12. [Singleton Classes](#singleton-classes)
13. [Anonymous Classes And Modules](#anonymous-classes-and-modules)

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

## Method Visibility

Ruby provides three visibility levels for methods: `public`, `private`, and `protected`.

### Default Visibility

Methods defined at the **top level** default to `private`:

```ruby
def foo; end  # private by default at top level

public

def bar; end  # now public (after public modifier)
```

Methods defined **inside a class or module** default to `public`:

```ruby
class Foo
  def bar; end  # public by default
end
```

### Visibility Modifiers

Visibility can be changed using `public`, `private`, or `protected`:

```ruby
class Foo
  def m1; end      # public (default)

  private

  def m2; end      # private (after modifier)

  protected def m3; end  # protected (inline modifier)
end
```

### Visibility Resets in Nested Scopes

**Important:** Visibility modifiers do NOT propagate into nested class or module definitions:

```ruby
private

class Foo
  def m1; end      # public (resets in new scope)

  private

  module Bar
    def m2; end    # public (resets again)
  end

  def m3; end      # private (still in Foo's private scope)
end
```

### Visibility Does NOT Apply to `def self.method`

Visibility modifiers have **no effect** on singleton methods defined with `def self.method`:

```ruby
class Foo
  private

  def self.bar; end  # STILL PUBLIC (visibility ignored)
  # private_class_method :bar can turn `bar` private
end

Foo.bar  # Works fine
```

**Important:** This does NOT apply inside `class << self` blocks - visibility works normally there:

```ruby
class Foo
  class << self
    private

    def bar; end  # Actually private
  end
end

Foo.bar  # private method 'bar' called for class Foo (NoMethodError)
```

## Method Aliasing

Ruby provides two ways to create method aliases: the `alias` keyword and the `alias_method` method.

### The `alias` Keyword

The `alias` keyword creates a new name for an existing method:

```ruby
class Foo
  def bar
    "bar"
  end

  alias baz bar  # baz is now an alias for bar
end

Foo.new.baz  # => "bar"
```

Aliases can use symbols or bare method names:

```ruby
class Foo
  def original; end

  alias :new_name :original    # symbol syntax
  alias another_name original  # bare name syntax
end
```

### `alias_method`

`alias_method` is a method from `Module` that creates aliases at runtime:

```ruby
class Foo
  def bar; end

  alias_method :baz, :bar           # symbol syntax
  alias_method "qux", "bar"         # string syntax
end
```

Unlike `alias`, `alias_method` requires symbols or strings, not bare method names.

### Top-Level Aliases

Aliases can be defined at the top level but it **only** works with the `alias` keyword:

```ruby
def foo; end

alias bar foo  # Creates top-level method alias
alias_method :bar, :foo  # NoMethodError
```

### Global Variable Aliases

The `alias` keyword can also create aliases for global variables:

```ruby
$foo = 123

alias $bar $foo  # $bar is now an alias for $foo

$bar  # => 123
$bar = 456
$foo  # => 456 (they share the same value)
```

This is different from simply assigning `$bar = $foo`, which would copy the value rather than create an alias.

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

### Instance Variables vs Class Variables: Receiver vs Lexical Scoping

A critical difference between instance variables (`@`) and class variables (`@@`) is how they determine ownership when defined inside a method with an explicit receiver:

- **Instance variables** follow the **receiver** (`self` at runtime)
- **Class variables** follow **lexical scope** (where the code is written)

```ruby
class Foo; end

class Bar
  # This defines a singleton method on Foo, but lexically inside Bar
  def Foo.demo
    @ivar = "instance var"   # Belongs to <Foo> (the receiver)
    @@cvar = "class var"     # Belongs to Bar (lexical scope)
  end
end

Foo.demo

Foo.instance_variables  # => [:@ivar]
Bar.instance_variables  # => []

Foo.class_variables     # => []
Bar.class_variables     # => [:@@cvar]
```

This behavior has important implications for static analysis:

| Variable Type | Scoping Rule | In `def Foo.demo` inside `class Bar` |
|---------------|--------------|--------------------------------------|
| `@ivar`       | Receiver     | Belongs to `Foo`                     |
| `@@cvar`      | Lexical      | Belongs to `Bar`                     |

The same applies to `class << Foo` blocks defined inside another class:

```ruby
class Foo; end

class Bar
  class << Foo
    def another_demo
      @ivar2 = 1   # Belongs to Foo (receiver is Foo's singleton)
      @@cvar2 = 2  # Belongs to Bar (lexical scope)
    end
  end
end

Foo.another_demo

Foo.instance_variables  # => [:@ivar2]
Bar.class_variables     # => [:@@cvar2]
```

## Mixins

Ruby provides three ways to mix modules into classes or other modules: `include`, `prepend`, and `extend`.

### Include

`include` inserts the module into the ancestor chain **after** the class, making the module's constants, methods, and module variables available:

```ruby
module Foo; end

class Bar
  include Foo
end

Bar.ancestors  # => [Bar, Foo, Object, Kernel, BasicObject]
```

### Prepend

`prepend` inserts the module into the ancestor chain **before** the class:

```ruby
module Foo; end

class Bar
  prepend Foo
end

Bar.ancestors  # => [Foo, Bar, Object, Kernel, BasicObject]
```

### Extend

`extend` adds module methods as **singleton methods** (class methods when used in a class):

```ruby
module Foo
  def bar; end
end

class Baz
  extend Foo
end

Baz.bar  # Works (class method)
Baz.new.bar  # NoMethodError (not an instance method)
```

#### Extend Self Pattern

A common pattern is `extend self` to make module methods callable on the module itself:

```ruby
module Foo
  extend self

  def bar
    "bar"
  end
end

Foo.bar  # => "bar"
```

#### Extend vs Include in Singleton Class

`extend Foo` is functionally equivalent to `class << self; include Foo; end` - both add methods to the singleton class. However, they trigger **different hooks**:

```ruby
module Foo
  def self.included(base)
    puts "included hook called on #{base}"
  end

  def self.extended(base)
    puts "extended hook called on #{base}"
  end
end

class UsingExtend
  extend Foo  # Triggers: "extended hook called on UsingExtend"
end

class UsingSingletonInclude
  class << self
    include Foo  # Triggers: "included hook called on #<Class:UsingSingletonInclude>"
  end
end
```

Both classes get the same methods, but:

- `extend` calls the `extended` hook
- `include` in singleton class calls the `included` hook

### Multiple Mixins

Multiple modules can be mixed in a single call:

```ruby
class Foo
  include Bar, Baz   # Baz is included first, then Bar
  prepend Qux, Quux  # Quux is prepended first, then Qux
end
```

### Mixin Deduplication

Ruby **removes duplicate modules** from the ancestor chain:

```ruby
module A; end

module B
  include A
end

class Foo
  include A
  include B
end

Foo.ancestors  # => [Foo, B, A, Object, ...]
# A appears only once, not twice
```

**Indirect duplicates** are also removed:

```ruby
module A; end

module B
  include A
end

module C
  include A
end

module Foo
  include B
  include C
end

Foo.ancestors  # => [Foo, C, B, A]
# A appears only once despite being in both B and C
```

When both parent and child include the same module, Ruby deduplicates it - the module appears **once** in the ancestor chain:

```ruby
module A; end

module B
  include A
end

class Parent
  include B
end

class Child < Parent
  include B
end

Child.ancestors  # => [Child, Parent, B, A, Object, ...]
# B and A appear once (child's include is deduplicated against parent)
```

### Top-Level Mixins

Mixins at the top level affect `Object`:

```ruby
include Foo  # Makes Foo's methods available everywhere
```

**Note:** `include self`, `prepend self`, or `extend self` at the top level is invalid and will produce an error:

```ruby
include self # => TypeError: wrong argument type Object (expected Module)
prepend self # => NoMethodError: undefined method 'prepend' for main
extend self  # => TypeError: wrong argument type Object (expected Module)
```

### Cyclic Mixins

`include self` and `prepend self` inside a module are rejected as cyclic:

```ruby
module Foo
  include self # => ArgumentError: cyclic include detected
end

module Bar
  prepend self # => ArgumentError: cyclic prepend detected
end
```

`extend self` inside a module is valid (see [Extend Self Pattern](#extend-self-pattern) above).

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

### Inheritance Chain Resolution

Ruby builds ancestors through the following steps recursively:

1. Prepended modules (in reverse order of prepending)
2. Current class
3. Included modules (in reverse order of inclusion)
4. Parent class

And then it continues up the inheritance chain.

```rb
module PrependedInPrepended
end

module Prepended
  prepend PrependedInPrepended
end

module PrependedInIncluded
  prepend PrependedInPrepended
end

module IncludedInIncluded
end

module Included
  include IncludedInIncluded
end

class Foo
end

class Bar < Foo
  prepend Prepended
  include Included
end

puts Bar.ancestors.to_s
#=> [PrependedInPrepended, Prepended, Bar, Included, IncludedInIncluded, Foo, Object, Kernel, BasicObject]
```

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

**Resolution order:**

1. Lexical scope
2. [Inheritance chain](#inheritance-chain-resolution)
3. Top-level

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

## Constant Aliases

Constants can be assigned to reference other constants, creating **aliases**:

```ruby
module Foo
  CONST = 123
end

ALIAS = Foo
ALIAS::CONST  # Resolves to Foo::CONST (123)
```

### Chained Aliases

Aliases can reference other aliases, forming chains:

```ruby
module Foo
  CONST = 123
end

ALIAS1 = Foo
ALIAS2 = ALIAS1
ALIAS2::CONST  # Resolves through ALIAS2 -> ALIAS1 -> Foo -> Foo::CONST
```

### Scoped Aliases

Aliases can be defined within namespaces:

```ruby
module Foo
  CONST = 1
end

module Bar
  MyFoo = Foo  # Bar::MyFoo is an alias for Foo
end

Bar::MyFoo::CONST  # Resolves to Foo::CONST
```

Aliases can also be assigned using qualified paths:

```ruby
module Foo; end
module Bar; end

Bar::ALIAS = Foo  # Creates Bar::ALIAS pointing to Foo
```

### Conditional Aliases

The `||=` operator can create conditional aliases:

```ruby
ALIAS ||= Foo  # Only assigns if ALIAS is not already defined
```

### Defining Classes Or Modules Under Aliased Path

Ruby allows defining classes or modules under aliases. The defined class/module will have its fully qualified name based on the alias target, not the alias itself. For example:

```ruby
class Foo; end
ALIAS = Foo

class ALIAS::Bar; end
ALIAS::Bar.name # Foo::Bar, not ALIAS::Bar
```

## Singleton Classes

Ruby can create a *singleton class* for most objects, but it usually materializes that class lazily the first time
singleton-specific syntax/methods are used.

- Defining singleton behavior (`def self.*`, `class << Foo`, `extend`, etc.) forces Ruby to create the singleton class if it was not created already.
- Calling `Foo.singleton_class` also forces creation and returns the singleton class object.
- Plain objects behave the same way: `obj.singleton_class` or `def obj.special` allocates one lazily for that object.
- Classes and modules that never reference their singleton class may run indefinitely without allocating it.
- Certain objects can never have singleton classes: integers, floats, symbols, and frozen strings. Calling `singleton_class` on them raises `TypeError`.
- Ruby boots with singleton classes for `true`, `false`, and `nil` already created.

Calling `Foo.new` does **not** force Foo's singleton class to exist. The `new` method is defined on `Class`
itself, so every class object inherits it automatically. Unless you override `Foo.new` via
`def self.new` (which would then create the singleton class) Ruby can instantiate `Foo` instances without ever
materializing `Foo`'s singleton class.

```ruby
Class.instance_method(:new).owner
# => Class
```

The singleton class is the receiver for `def self.*`, `class << self`, and it is where modules mixed in via `extend`
actually insert their methods.

```ruby
class Foo
  class << self # reopens (or creates) Foo's singleton class
    A = 1
    def bar; A; end
  end

  def self.baz; end # adds the method to Foo's singleton class

  @ivar = 1 # assigns a class instance variable on Foo's singleton class (NOT a class variable)
end
```

### Lexical Scope vs Singleton Receiver

- Inside the class body the current *receiver* (`self`) is `Foo`, so `@ivar` writes to the singleton class and `def self.baz`
  defines a singleton method.
- Lexical constant lookup still follows the surrounding constant scope, so constants created inside `class << self` do not
  become `Foo::CONST`.

```ruby
class Foo
  @counter = 0

  class << self
    A = 1

    def bar
      @counter += 1      # valid: singleton ivar
      puts A             # valid: constant defined in this singleton scope
    end
  end

  def self.baz
    puts @counter        # valid: reads the singleton's @counter
    puts A               # NameError: Foo::A is not defined
  end
end
```

If a constant must be visible as `Foo::CONST`, define it in the regular class body instead of inside a `class << self`
block.

### Reopening Singleton Class Explicitly

The singleton class can be reopened anywhere with `class << Foo` as long as the constant already exists:

```ruby
# file: foo.rb
class Foo; end

# file: foo_singleton.rb
class << Foo
  def bar!
    puts "bar"
  end
end

# file: my_class.rb
class MyClass
  # This also reopens Foo's singleton class
  class << Foo
    def baz!
      puts "baz"
    end
  end
end

Foo.bar!
Foo.baz!
```

### Class Variables and Singleton Classes

Class variables (`@@var`) **bypass singleton classes entirely**.
They always belong to the base class or module, regardless of where they are defined:

```ruby
class Foo
  @@in_class_body = 1

  class << self
    @@in_singleton_scope = 2

    def set_var
      @@in_method = 3
    end
  end
end

Foo.set_var

# All class variables belong to Foo, not its singleton class
Foo.class_variables
# => [:@@in_class_body, :@@in_singleton_scope, :@@in_method]

Foo.singleton_class.class_variables
# => [:@@in_class_body, :@@in_singleton_scope, :@@in_method]  # inherited view

# The singleton class itself owns no class variables
Foo.singleton_class.class_variables(false)
# => []
```

This is fundamentally different from instance variables (`@var`), which **do** belong to the singleton class when
defined in class scope:

```ruby
class Foo
  @class_ivar = 1  # belongs to Foo's singleton class
  @@class_var = 2  # belongs to Foo itself
end
```

Top-level class variable access (`@@foo = 1` outside any class) raises `RuntimeError: class variable access from toplevel`.

### Nested Singleton Classes

`class << self` can be nested. Each nesting opens the singleton class of the current `self`:

```ruby
class Foo
  class << self
    # self is now Foo.singleton_class
    def on_foo_singleton; end

    class << self
      # self is now Foo.singleton_class.singleton_class
      def on_singleton_singleton; end
    end
  end
end

# on_foo_singleton is callable on Foo
Foo.on_foo_singleton

# on_singleton_singleton is NOT callable on Foo
Foo.on_singleton_singleton
# => NoMethodError

# It's callable on Foo's singleton class
Foo.singleton_class.on_singleton_singleton
```

The ownership chain is: `Foo` → `Foo.singleton_class` → `Foo.singleton_class.singleton_class`.

## Anonymous Classes And Modules

`Class.new` and `Module.new` create anonymous class/module objects. When assigned to a constant, that constant names the new namespace.

### Method receivers and `self`

Inside the block, `self` is the newly created class/module.

```ruby
Foo = Class.new do
  def self.foo; end   # class method on Foo
  def bar; end        # instance method on Foo instances
  alias_method :baz, :bar
end
```

You can still access those methods even when the anonymous class isn't assigned to a constant:

```ruby
c = Class.new do
  def self.foo; end
  def bar; end
end

c.foo
c.new.bar
```

### Instance variables

Instance variables behave the same as in regular class definitions:

```ruby
Foo = Class.new do
  @class_ivar = 1     # instance variable of the Foo class
  def initialize
    @ivar = 2         # instance variable of Foo instances
  end
end
```

### Class variables and constants

Class variables and constants follow lexical scope (`Module.nesting`). `Class.new`/`Module.new` blocks do **not** change lexical scope, so unqualified constants and class variables are defined in the outer scope, not on the new class/module.

```ruby
class Outer
  Foo = Class.new do
    @@cvar = 1 # this belongs to Outer, not Foo
  end
end
```

Defining class variables in a top-level anonymous class/module results in an error:

```ruby
Foo = Class.new do
  @@cvar = 1   # RuntimeError: class variable access from toplevel
end
```

Similar to class variables, constants are attached to the closest lexical scope, not the anonymous class/module:

```ruby
Foo = Class.new do
  CONST = 2
end

defined?(Foo::CONST)  # => nil
defined?(::CONST)     # => "constant"
```

If you want constants under the new class/module, use `self::CONST` or define them after the assignment.

```ruby
Foo = Class.new do
  self::CONST = 2
end

Foo::CONST
```

### Namespaces and lexical nesting

The block does **not** introduce lexical nesting for `class`/`module` definitions:

```ruby
class Foo
  Bar = Class.new do
    class Baz; end
  end
end

# Baz is Foo::Baz, not Foo::Bar::Baz
```

You cannot reliably reference the assigned constant inside the block because the assignment happens after the block runs. `Bar::Baz` inside the block refers to an outer `Bar` (if any) or raises `NameError`. Using `self::Bar` as a superclass also fails because `self` is the anonymous class, not the outer scope:

```ruby
class Foo
  Bar = Class.new do
    class Baz < Bar; end       # NameError: uninitialized constant Foo::Bar
    class Baz < self::Bar; end # NameError: uninitialized constant #<Class:...>::Bar
  end
end
```

To define classes or constants under the new class, use `class self::Baz` or `self::Baz = Class.new` (or `const_set`) inside the block, or define them after the assignment:

```ruby
class Foo
  Bar = Class.new do
    class self::Baz; end      # defines Foo::Bar::Baz
    self::Qux = Class.new     # defines Foo::Bar::Qux
  end
end

# or
class Foo
  Bar = Class.new
  Bar::Baz = Class.new
end
```

### Nested anonymous Class.new/Module.new

The rules above apply to nested anonymous class/module definitions as well.

For example, constants defined in nested anonymous classes are still attached to the closest lexical scope:

```ruby
Foo = Class.new do
  Class.new do
    CONST = 1
  end
end

# CONST is defined on top-level, the closest lexical scope, instead of Foo
defined?(CONST) #=> "constant"
defined?(Foo::CONST) #=> nil
```

### Singleton class blocks inside anonymous classes

Although `Class.new`/`Module.new` blocks do not create lexical scopes, `class << self` blocks inside them still do. Constants and class variables defined inside such a singleton class block are attached to the singleton class:

```ruby
Foo = Class.new do
  class << self
    CONST = 1
  end
end

Foo.singleton_class.constants  # => [:CONST]
Foo.constants                  # => []
```

This also applies to nested anonymous classes within the singleton class block:

```ruby
Foo = Class.new do
  class << self
    Class.new do
      CONST = 1
    end
  end
end

Foo.singleton_class.constants  # => [:CONST]
```
