---
name: send-private-method
description: Remediate a `send`/`__send__` call that reaches a private method, restoring a real public seam instead of bypassing visibility. Use when send is being used to invoke private functionality in a class.
---

# Send Private Method

A test calls `obj.send(:some_private_method)` (or `__send__`) to invoke a method Ruby's visibility system would otherwise forbid. `send` deliberately bypasses encapsulation. `__send__` behaves identically and is the conventional spelling when a class defines its own `send`; Ruby warns when you redefine `__send__`, but it is not un-overridable. Remediation is the same for both. The smell is not "you used `send`" — it is "the test reaches past the public API into an implementation detail," which breaks the moment the private method is renamed, reorganized, or removed even though the public contract is unchanged.

## Confirm the receiver and the method

You have a call site: a file path and a line number. Before changing anything, answer two questions:

1. **What is the receiver's real type?** Trace the receiver expression at the flagged line back to its class. If it is a local/ivar, find where it is assigned (a `let`, `setup`, factory, or constructor).
2. **Where is the private method defined?** Search the workspace for `def <method_name>` (and `define_method :<method_name>`). Confirm the definition is under `private` (or `private :<method_name>`). Note its enclosing class/module and read the method body — you cannot decide the fix without knowing what the method does.

A name-based search matches by method *name* across the whole workspace, so a public method and a private method sharing the same unqualified name on different classes can produce a false positive. If the receiver's type has a *public* method of that name, this is not the smell — stop here.

## Decide which situation you are in

Read the method body and the test. Exactly one of these applies:

- **The caller needs behavior that should be public.** The private method does something the application legitimately depends on, and there is no public entry point that exposes it. The encapsulation boundary was drawn too tightly.
- **The caller is a test reaching past an existing public API.** A public method already wraps or drives this private one; the test bypassed it to assert on an intermediate result instead of the observable outcome. If the public outcome is already tested elsewhere, the private-method test is redundant — delete it rather than rewriting it.
- **The private method is the wrong home for the logic.** The behavior is genuinely useful to more than one caller, but it lives tucked away inside one class's private section. Both production and tests want it.

## Fix per situation

### 1. Promote to public (or `protected`)

If the behavior is part of the object's real contract, make it public and document it as API. Use `protected` only when every legitimate caller is an instance inside the same class hierarchy as the class that defines the method — never to paper over a single test's reach-in.

Before:
```ruby
class Foo
  def bar = qux.sum

  private

  def qux
    @items.reject(&:voided?).map { |i| i.baz - i.discount }
  end
end
# test
foo.send(:qux)
```

After:
```ruby
class Foo
  def bar = qux.sum

  # Public: items with discounts applied, voided items excluded.
  # Used by reporting and by #bar's total.
  def qux
    @items.reject(&:voided?).map { |i| i.baz - i.discount }
  end
end
# test
foo.qux
```

### 2. Drive the public entry point, assert on the observable outcome

If a public method already exercises the private one, call the public method and assert on what it produces — not on the private intermediate.

Before:
```ruby
class Foo
  def bar(baz) = Receipt.new(tax: baz.amount * qux(baz.quux))

  private

  def qux(quux) = RATES.fetch(quux)
end
# test
foo.send(:qux, "CA") # => 0.05
```

After:
```ruby
# test — assert on the observable result of the public API
result = foo.bar(baz_with(quux: "CA", amount: 100))
assert_equal 5.0, result.tax
```

### 3. Extract to its own object/module

When the logic serves multiple callers but is trapped in one class's private section, move it to a dedicated object or module with a public interface both callers can use.

Before:
```ruby
class Foo
  private

  def bar(raw) = raw.transform_keys(&:to_s).compact
end
# test
foo.send(:bar, { baz: "Q" })
```

After:
```ruby
class Bar
  def call(raw) = raw.transform_keys(&:to_s).compact
end

class Foo
  def initialize(bar: Bar.new) = @bar = bar
  def baz(raw) = @bar.call(raw)
end
# test
assert_equal({ "baz" => "Q" }, Bar.new.call({ baz: "Q" }))
```

## Private class methods and `module_function`

The three situations above cover instance methods. Two related visibility forms need special handling:

- **Private class methods** (`private_class_method :bar`, or a `def bar` under `private` inside `class << self`). A bare `private` does **not** apply to `def self.bar`: that singleton method stays public wherever it sits relative to the `private` line, so a public `def self.bar` under `private` is not this smell. A `send` like `Foo.send(:bar)` reaches a genuinely private singleton method. The remediation mirrors instance methods — promote with `public_class_method :bar` (or drop the `private_class_method :bar` call), drive an existing public class method, or extract. Do not use the instance-method patterns verbatim; the fix targets the singleton class.
- **`module_function`**. `module_function :foo` creates a public singleton method and a private instance method. If the test calls `instance.send(:foo)` on the private instance side, the public entry point already exists: call `MyModule.foo` directly. This is situation 2 — drive the public singleton method, do not promote the instance method.

## Legitimate exceptions — and what to do about them

Not every `send` to a private name is a smell to remove. Handle these deliberately:

- **Dynamic dispatch over a validated allowlist.** The method name is genuinely computed (`send(action)`), and the action set is bounded and checked. Constrain it: define an `ALLOWED_ACTIONS = %i[...].freeze` constant, guard with `raise unless ALLOWED_ACTIONS.include?(action)`, and call `public_send(action)` so the visibility system still applies. If every allowed action is public, the smell is gone.
- **Framework or DSL callbacks.** Some libraries require `send` to reach hooks they themselves marked private. If the method name is dictated by the framework contract, leave the call but add a one-line comment naming the framework and the callback it satisfies.
- **Third-party code you cannot change.** The private method belongs to a gem or an owned-elsewhere class you must not edit. Wrap the reach-in behind a single named adapter method in your own code with a comment stating why (e.g. `# Sends :bar to avoid the gem's private API; remove when upstream exposes a public hook.`). The adapter localizes the violation so it is auditable and removable in one place.

## What does NOT fix it

Switching `send` to `public_send` on a method that is *still private* does not fix anything — it just moves the failure from "silently bypassed encapsulation" to a `NoMethodError` at runtime. Answer the encapsulation question first; only then choose `public_send` (for dynamic dispatch over public methods) or a direct call (once promoted).
