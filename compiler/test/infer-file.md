# Type inference for files

What's the type of a full formed source file?

---

Simple check

> infer("fn () -> Int")

```rust
fn foo() -> Int { 1 }
```

Trait bounds

> infer("fn <A: Eq>(A) -> Bool")

```rust
fn foo<T: Eq>(x: T) -> Bool {
  true
}

fn bar<Y: Eq>(x: Y) -> Bool {
  foo(x)
}
```

Missing constraint

> errorContains("Constraint Eq not satisfied for type Y")

```rust
fn foo<T: Eq>(x: T) -> Bool {
  true
}

fn bar<Y>(x: Y) -> Bool {
  foo(x)
}
```

Methods are resolved in nested function.

> infer("fn () -> Int")

```rust
struct Foo { a: Int }

impl Foo {
  fn method(self) -> Int {
    self.a + 5
  }
}

fn foo() -> Int {
  fn bar() -> Int {
    let x = Foo { a: 1 };
    x.method()
  }

  bar()
}
```

Traits bring functions in scope

> infer("fn () -> Int")

```rust
fn foo() -> Int {
  let x = Eq::equals;
  1
}
```

Instances for built-in types are automatically derived

> infer("fn () -> Bool")

```rust
fn foo() -> Bool {
  Eq::equals(1, 1) as Bool;
  Eq::equals(false, true) as Bool;
  Eq::equals("yo", "bo") as Bool;
  Eq::equals([1], [2,3]) as Bool;
}
```

Deriving will error on functions

> errorContains("Functions can't be compared")

```rust
fn bar() {}

fn foo() -> Bool {
  Eq::equals(bar, bar) as Bool;
}
```

Exhaustive check

> errorContains('exhaustive'); errorContains('missing case: \"Color::Blue\"');

```rust
enum Color { Blue, Red, Green }

fn borgo_main() -> Int {
  let x = Color::Red;
  match x {
    Color::Red => 1,
    Color::Green => 2,
  }
}
```

Exhaustive check works with unqualified constructors.

> errorContains('exhaustive'); errorContains('missing case: \"Foo::B\"');

```rust
enum Foo { A, B, Baz }

fn borgo_main() -> Int {
  match Foo::B {
    A => 1,
    Foo::Baz => 2,
  }
}
```

Type checking still works.

> errorContains('Int or Float')

```rust
enum Foo { A, B, Baz }

fn foo(a: Foo) -> Int { a + 1 }
fn borgo_main() {}
```

Keys in Maps must be hashable

> errorContains("Functions can't be hashed")

```rust
fn foo<K: Hash>(k: K) {}

fn borgo_main() {
  foo(|| {});
  unreachable!();
}
```

Const expressions are type checked

> errorContains('mismatch')

```rust
const a: String = 1;
```

Catch exhaustiveness errors.

> errorContains("constructor Green not found")

```rust
fn borgo_main() {
  enum Color { Red, Blue }

  match Color::Red {
    Red => 1,
    Green => 2,
  };
  ()
}
```

Extern declarations with non existing generics

> errorContains("Type not found: K")

```rust
struct Foo {}
extern "native/Foo" {
  fn bar<T>(x: T, k: K);
}

fn borgo_main() {
  1.assert_eq(2)
}
```

Bools missing arm

> errorContains('missing case: \"true\"')

```rust
fn borgo_main() {
  match false {
    false => (),
  };
}
```

Bools missing arm

> errorContains('missing case: \"true\"')

```rust
fn borgo_main() {
  match false {
    false => (),
  };
}
```

Functions in structs aren't comparable

> errorContains("compared")

```rust
struct Foo {
  bar: fn (Int) -> Int
}

fn borgo_main() {
  let foo = Foo { bar: |x: Int| x + 2 };
  let bar = Foo { bar: |x: Int| x + 2 };
  foo == bar;
  ()
}
```

Functions in enums aren't comparable

> errorContains("compared")

```rust
enum Foo {
  Bar(Int, fn (Int) -> Int)
}

fn borgo_main() {
  let foo = Foo::Bar(0, |x: Int| x + 2);
  let bar = Foo::Bar(0, |x: Int| x + 2);
  foo == bar;
  ()
}
```

Prevent usage of reserved words

> errorContains("reserved")

```rust
fn foo(default: Int) -> Int {
  default
}
```

Inference of numeric operators

> errorContains("Int or Float")

```rust
fn borgo_main() -> Int {
  false > true;
  1
}
```

Declaration ordering doesn't mess up inference.

> errorContains("Ordering")

```rust
fn borgo_main() {
  Seq::max_by as Int;
  ()
}
```

Pattern matching on list literals.

> errorContains("pattern match on list literals")

```rust
fn borgo_main() {
  match [[1]].seq().first() {
    Some(x) => x,
    None => [],
  };

  match xs {
    [1,2] => false,
    _ => true,
  };

  ()
}
```
