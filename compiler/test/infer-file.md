# Type inference for files

What's the type of a full formed source file?

---

Simple check

> infer("fn () -> int")

```rust
fn foo() -> int { 1 }
```

Methods are resolved in nested function.

> infer("fn () -> int")

```rust
struct Foo { a: int }

impl Foo {
  fn method(self) -> int {
    self.a + 5
  }
}

fn foo() -> int {
  fn bar() -> int {
    let x = Foo { a: 1 };
    x.method()
  }

  bar()
}
```

Exhaustive check

> errorContains('exhaustive'); errorContains('missing case: \"Color::Blue\"');

```rust
enum Color { Blue, Red, Green }

fn borgo_main() -> int {
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

fn borgo_main() -> int {
  match Foo::B {
    A => 1,
    Foo::Baz => 2,
  }
}
```

Type checking still works.

> errorContains("mismatch")

```rust
enum Foo { A, B, Baz }

fn foo(a: Foo) -> int { a + 1 }
fn borgo_main() {}
```

Const expressions are type checked

> errorContains('mismatch')

```rust
const a: string = 1;
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

Mod declarations with non existing generics

> errorContains("Type not found: K")

```rust
struct Foo {}

#[package(path = github.com:yo/test, name = test::pkg)]
mod f {
  fn bar<T>(x: T, k: K) { EXT }
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

Prevent usage of reserved words

> errorContains("reserved")

```rust
fn foo(default: int) -> int {
  default
}
```

Inference of numeric operators

> errorContains("numeric type")

```rust
fn borgo_main() -> int {
  false > true;
  1
}
```

Pattern matching on slice literals.

> errorContains("pattern match on slice literals")

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

For loops.

> infer("fn () -> ()")

```rust
fn borgo_main() {
  for x in ["a"] {
    x as string;
  }

  for (index, value) in ["a"].enumerate() {
      index as int;
      value as string;
  }
}
```

Expr in loops must be rangeable.

> errorContains("iterate")

```rust
fn borgo_main() {
  for x in 1 {
  }
}
```

Loops with no condition

> infer("fn () -> ()")

```rust
fn borgo_main() {
  loop {
    break;
  }
}
```

If expression with an expected type must have an else block

> errorContains("if false")

```rust
fn borgo_main() {
  if true {
    let x = "hello";
    1
  }

  let a = if false {
    let y = "yo";
    2
  };

  ()
}
```

Method calls work for enums.

> infer("fn () -> int")

```rust
enum Foo {
  Bar(int)
}

impl Foo {
  fn method(self, s: string, b: int) -> int {
    match self {
      Bar(a) => a + b,
    }
  }
}

fn borgo_main() -> int {
  let f = Foo::Bar(1);
  f.method("a", 2) as int;

  let m = f.method;
  m("a", 2)
}
```

Generics in extern blocks

> infer("fn () -> ()")

```rust
fn borgo_main() {
  let a = reflect.DeepEqual(1, false);
  let b = reflect.DeepEqual(1, 1);
}
```

Parse package info

> infer("fn () -> f::Foo")

```rust
#[package(path = github.com:yo/test, name = test::pkg)]
mod f {
  struct Foo { a: int }
}

use test::pkg;

fn borgo_main() -> f::Foo {
  f::Foo { a: 1 }
}
```

While loops.

> infer("fn () -> ()")

```rust
fn borgo_main() {
  let x = 1;
  while x < 10 {
    x as int;
  }
}
```

Enumeration in loops expect a tuple.

> errorContains("Use tuple literals")

```rust
fn borgo_main() {
  let m = Map::new();
  for e in m {
      e.0;
  }
}
```

Access slices by index

> infer("fn () -> string")

```rust
fn borgo_main() -> string {
    let xs = ["a"];
    xs[0]
}
```

Index must be int for slices

> errorContains("mismatch")

```rust
fn borgo_main() {
    let xs = ["a"];
    xs[false]
}
```

Access maps by index

> infer("fn () -> int")

```rust
fn borgo_main() -> int {
    let xs = Map::new();
    xs.insert("a", 1);
    xs["a"]
}
```

Index must be K for maps

> errorContains("mismatch")

```rust
fn borgo_main() {
    let xs = Map::new();
    xs.insert("a", 1);
    xs[false]
}
```

Parse traits

> infer("fn () -> ()")

```rust
fn borgo_main() {
  trait Foo {
    fn bar(x: string) -> int;
  }

  fn check(f: Foo) -> int {
    f.bar("yo")
  }

  ()
}
```

Check if type implements trait

> infer("fn () -> int")

```rust
fn borgo_main() -> int {
  trait Foo {
    fn bar(x: string) -> int;
  }

  fn check(f: Foo) -> int {
    f.bar("yo")
  }

  struct Baz { x: int }

  impl Baz {
    fn bar(self, _: string) -> int {
      self.x
    }
  }

  check(Baz { x: 1 })
}
```

Trait bounds

> infer("fn () -> int")

```rust
fn borgo_main() -> int {
  trait Foo {
    fn bar(x: string) -> int;
  }

  fn check<T: Foo>(f: T) -> int {
    f.bar("yo")
  }

  struct Baz { x: int }

  impl Baz {
    fn bar(self, _: string) -> int {
      self.x
    }
  }

  check(Baz { x: 1 })
}
```

Trait bounds checked at call site

> errorContains("method foo not found on type int")

```rust
fn borgo_main() {
  trait Foo { fn foo() -> int; }
  fn check<T: Foo>(f: T) {}
  check(1)
}
```

Variadic functions

> infer("fn () -> ()")

```rust
fn borgo_main() {
  fn foo(a: int, b: VarArgs<string>) {}

  foo(1);
  foo(1, "a");
  foo(1, "a", "b", "c");
}
```

Variadic functions arity error

> errorContains("Wrong arity")

```rust
fn borgo_main() {
  fn foo(a: int, b: VarArgs<string>) {}
  foo();
}
```

References are maintained after try call

> infer("fn () -> ()")

```rust
use os;

fn foo() -> Result<()> {
  let f = os.Open("file")?;
  Ok(bar(f))
}

fn bar(f: &mut os::File) {

}

fn borgo_main() {
  foo();
}
```

Result with an implicit E must implement error

> errorContains("method Error not found")

```rust
struct Foo {}

fn foo() -> Result<()> {
  Err(Foo {})
}
```

Type aliases

> infer("fn () -> ()")

```rust
type Foo<V> = Map<int, V>;
type Complex<A, B> = Map<bool, Map<A, Map<B, string>>>;

fn foo(m: Foo<string>) -> string {
    m[1]
}

fn takes_map(m: Map<int, string>) -> string {
    m[1]
}

fn borgo_main() {
    let m: Foo<string> = Map::new();
    m.insert(1, "a");

    foo(m);
    takes_map(m);

    let c: Complex<int, bool> = Map::new();
    c as Map<bool, Map<int, Map<bool, string>>>;
}
```

Tuple structs

> infer("fn () -> Foo")

```rust
struct Foo(int);

struct Bar<T>(int, T);

fn check(f: Foo) -> int {
    f.0
}

fn bar_check(b: Bar<string>) -> (int, string) {
    (b.0, b.1)
}

fn borgo_main() -> Foo {
    let b = Bar(1, false);
    let c = Bar(1, "yo");
    bar_check(c);

    check(Foo(1)) as int;
    Foo(2)
}
```

Type aliases to non existing types

> errorContains("Type not found: Bar")

```rust-skip
// TODO this is still broken
type Foo = Bar;
```
