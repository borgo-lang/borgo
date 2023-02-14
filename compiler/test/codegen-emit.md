# Code generation for files

Process files and emit Go code

---

Standalone expressions

```rust
fn borgo_main() {
  4.inspect();
  ()
}
```

Function calls

```rust
fn foo(b: Bool, x: Int) -> Bool { b }
fn bar(x: Int) -> Int { x }

fn borgo_main() {
  foo(match 1 {
    1 => true,
    _ => false,
  }, bar(5))
  .assert_eq(true);

  ()
}
```

Let bindings

```rust
fn borgo_main() {
  let a = 5 + 5;

  Debug::inspect(a);
  ()
}
```

If statements

```rust
fn borgo_main() {
  let x = if true { 6 } else { 0 };
  x.assert_eq(6);
}
```

Enums

```rust
enum Foo {
  Bar(Int, Bool),
  Baz,
}

fn borgo_main() {
  Foo::Bar(2, false).inspect();
  Foo::Baz.inspect();
  ()
}
```

Structs

```rust
struct Bar<T> {
  name: String,
  age: Int,
  v: T,
}

fn borgo_main() {
  let x = Bar { name: "yo", age: 99, v: false };
  x.inspect();
  ()
}
```

Return statement

```rust
fn foo() -> Int {
  let x = match 1 {
    1 => return 12,
    _ => 5,
  };

  if true {
    x + 40
  } else {
    return 9
  }

  return 4
}

fn borgo_main() {
  foo().assert_eq(12);
}
```

Struct access

```rust
struct Foo {
  a: Int,
  b: String,
  c: Bool,
}

fn borgo_main() {
  let x = Foo { a: 1, b: "hi", c: true };
  x.a.assert_eq(1);
  let y = Foo { a: 5, c: false, ..x };
  y.a.assert_eq(5);
  x.a.assert_eq(1);
  ()
}
```

Try operator

```rust
enum Error { Foo(String) }

fn foo(b: Bool) -> Result<Int> { bar("a") }

fn bar(s: String) -> Result<Int> {
  return Err(Error::Foo("boom"));
  Ok(4)
}

fn baz() -> Result<Int> {
  let _ = foo(false)?;

  unreachable!();
  Ok(1)
}

fn borgo_main() {
  baz().assert_eq(Err(Error::Foo("boom")));
}
```

Destructure function params

```rust
fn foo((_, b): (Int, String)) -> String {
  b
}

fn borgo_main() {
  foo((1, "yo")).assert_eq("yo");
  ()
}
```

Lists

```rust
fn borgo_main() {
  let x = [1, 2, 5 + 5];
  x.len().assert_eq(3);
}
```

Impl blocks

```rust
struct Foo { a: Int }

impl Foo {
  fn bar(self, x: Int) -> Int {
    self.a + x
  }
}

fn borgo_main() {
  let f = Foo { a: 1 };
  f.bar(5).assert_eq(6);
}
```

Rebind variables

```rust
fn borgo_main() {
  let x = 1;
  x.assert_eq(1);

  let x = 1 + 1;
  x.assert_eq(2);

  {
    let x = x + 5;
    x.assert_eq(7);
  }

  x.assert_eq(2);
}
```

Stdlib

```rust
fn borgo_main() {
  let x = [1,2,3];

  x.push(4).assert_eq([1,2,3,4]);
  x.pop().assert_eq([1,2]);
  x.pop().pop().pop().pop().is_empty().assert_eq(true);

  //
  // Sequence
  //
  let s = Seq::infinite(0, |n| n + 3);

  s.take(3).len().assert_eq(3);

  s
    .take(50)
    .to_list()
    .get(4)
    .assert_eq(Some(12));

  x
    .seq()
    .filter(|n| n > 1)
    .map(|n| n * 8)
    .to_list()
    .get(1)
    .inspect();

  s
    .take(10)
    .sum()
    .assert_eq(135);

  s
    .drop(10)
    .take(1)
    .to_list()
    .get(0)
    .assert_eq(Some(30));

  {
    let (first, last) = s.split_at(5);
    first.sum().assert_eq(30);
    last.take(5).to_list().assert_eq([15, 18, 21, 24, 27]);
  }

  {
    let foo = s.chunks(2);
    foo.get(0).unwrap().sum().assert_eq(3);
    foo.get(1).unwrap().sum().assert_eq(15);
    foo.get(2).unwrap().sum().assert_eq(27);

    let bar = s.take(5).chunks(3);
    bar.get(0).unwrap().sum().assert_eq(9);
    bar.get(1).unwrap().sum().assert_eq(21);
    bar.get(2).assert_eq(None);
  }

  {
    let foo = s.windows(2);
    foo.get(0).unwrap().sum().assert_eq(3);
    foo.get(1).unwrap().sum().assert_eq(9);
    foo.get(2).unwrap().sum().assert_eq(15);

    let bar = s.take(5).windows(3);
    bar.get(0).unwrap().sum().assert_eq(9);
    bar.get(1).unwrap().sum().assert_eq(18);
    bar.get(2).is_some().assert_eq(true);
    bar.get(3).is_none().assert_eq(true);
  }

  s
    .find_map(|x| if x == 21 { Some(x) } else { None })
    .assert_eq(Some(21));

  s
    .take(10)
    .max_by(Int::cmp)
    .assert_eq(Some(27));


  {
    let other = ["a", "b", "c"];
    let zipped = s.zip(other.seq());
    zipped.get(1).assert_eq(Some((3, "b")));
    zipped.get(3).assert_eq(None);
  }

  ["a", "b", "c"]
    .seq()
    .enumerate()
    .get(2).unwrap().assert_eq((2, "c"));

  {
    let chained = s.drop(100).take(5)
      .chain(s)
      .take(10)
      .to_list();

    chained
      .get(4)
      .assert_eq(Some(312));

    chained
      .get(6) // one after the initial sequence
      .assert_eq(Some(3));
  }

  //
  // Map
  //
  let m = Map::new()
    .insert("foo", 6)
    .insert("bar", 8);

  m.len().assert_eq(2);
  m.get("bar").assert_eq(Some(8));
  m.get("xyz").assert_eq(None);

  m
    .seq()
    .to_list()
    .get(0)
    .assert_eq(Some(("foo", 6)));

  m
    .seq_keys()
    .to_list()
    .get(1)
    .assert_eq(Some("bar"));

  Map::new()
    .insert(99, "baz")
    .get(99)
    .assert_eq(Some("baz"));

  [("a", 1), ("b", 2)]
  .seq()
  .to_map()
  .len()
  .assert_eq(2);

  //
  // Set
  //
  let set = Set::new()
    .insert(1)
    .insert(2);

  set.contains(1).assert_eq(true);
  set.contains(3).assert_eq(false);

  set.delete(2).assert_eq(Set::new().insert(1));

  [1,2,3].seq().to_set().assert_eq(
    Set::new()
        .insert(1)
        .insert(2)
        .insert(3)
  );


  //
  // Option
  //
  Option::None.unwrap_or_else(|| 4).assert_eq(4);
}
```

Select statement

```rust
fn foo(x: Sender<Int>) {
  x.send(3)
}

fn borgo_main() {
  let (x_send, x_recv) = Channel::new();
  let (_, y_recv) = Channel::new();

  spawn!((|| { x_send.send(5) })());

  let val = x_recv.recv();
  val.unwrap().assert_eq(5);

  spawn!(foo(x_send));

  match select!() {
    ChannelOp::Recv(x_recv, val) => val.unwrap().assert_eq(3),
    ChannelOp::Recv(y_recv, _) => unreachable!(),
  };
}
```

Strings

```rust
fn borgo_main() {
  "hello ".append("world").assert_eq("hello world");
  "a b c".split(" ").to_list().assert_eq(["a", "b", "c"]);
  "foobarbaz".slice(0, 6).assert_eq("foobar");
  "foobarbaz".slice(3, -3).assert_eq("bar");

  {
    let x = "foo".chars().to_list();
    x.len().assert_eq(3);
    x.get(0).assert_eq(Some('f'));
  }

  "foobarbaz".contains("bar").assert_eq(true);
  "abcd".index_of("c").assert_eq(Some(2));
  "abcd".index_of("z").assert_eq(None);

  'f'.to_int().assert_eq(102);
  'f'.to_string().assert_eq("f");
  ()
}
```

Refs

```rust
fn borgo_main() {
  let count = 0.to_ref();
  [0, 0, 0].seq().for_each(|_| count.mutate(|c| c + 1));
  count.get().assert_eq(3);
  ()
}
```

Math with floats

```rust
fn borgo_main() {
  let x = 5.3 * 1.2;
  Debug::assert_eq(true, x > 6.35 && x < 6.36);
}
```

Early returns in blocks.

```rust
fn foo() -> Int {
  {
    return 5;
  }

  999
}

fn borgo_main() {
  foo().assert_eq(5);
}
```

Recursive functions are supported in files.

```rust
file: main.brg

fn foo(n: Int) -> Int {
  if n != 5 {
    return foo(n + 1)
  }

  n
}

fn borgo_main() {
  foo(1).assert_eq(5)
}
```

Ingest enums and structs

```rust
file: main.brg

enum Color { Red, Green }
struct Foo { name: String }
fn borgo_main() -> Int { 1 }
```

Mutually recursive functions

```rust
file: main.brg

fn even(n: Int) -> Bool {
  if n == 0 {
    return true
  }

  odd(Int::abs(n) - 1)
}

fn odd(n: Int) -> Bool {
  if n == 0 {
    return false
  }

  even(Int::abs(n) - 1)
}

fn borgo_main() {
  even(10).assert_eq(true);
}
```

Multiple files

```rust
file: foo.brg
enum Foo {
  X(Bar),
}

file: bar.brg
enum Bar {
  A(Int),
}

fn with_foo(f: Foo, m: Int) -> Int {
  match f {
    Foo::X(b) => match b {
      Bar::A(n) => n + m
    }
  }
}

file: main.brg
fn borgo_main() {
  let bar = Bar::A(2);
  with_foo(Foo::X(bar), 3).assert_eq(5);
}
```

Recursion across files

```rust
file: a.brg
fn a(n: Int) -> Int {
  if n == 100 {
    return n
  }

  b(n + 10)
}

file: b.brg
fn b(n: Int) -> Int {
  if n == 200 {
    return n
  }

  a(n + 20)
}

file: main.brg
fn borgo_main() -> Int { a(40) }
```

Match on structs

```rust
struct Foo { a: Int }

fn borgo_main() {
    let x = Foo { a: 1 };
    let res = match x {
        Foo { a: 2 } => false,
        Foo { a: 1 } => true,
        Foo { a: _ } => false,
    };

    res.assert_eq(true);
}
```

Const expressions are global

```rust
file: main.brg

const a: Int = 1;

fn borgo_main() -> Int { a + 5 }
```

Const expressions are visible from other files

```rust
file: foo.brg

fn check() -> Bool {
  foo == 2
}

file: main.brg

const foo: Int = 1 + 1;
fn borgo_main() -> Bool { check() }
```

Global refs

```rust
file: main.brg

const state: Ref<Int> = 5.to_ref();

fn foo() {
  state.mutate(|x| x + 80);
}

fn borgo_main() -> Int {
  foo();
  state.get()
}
```

Paren expressions

```rust
fn borgo_main() {
  (1 + 4).assert_eq(5);
  ()
}
```

Recursive types

```rust
enum Expr {
  Add(Expr, Expr),
  Number(Int),
}

struct Foo {
  n: String,
  f: Option<Foo>,
}

fn borgo_main() {
  let one = Expr::Number(1);
  let two = Expr::Number(2);
  let e = Expr::Add(one, two);
  e.inspect();

  let f = Foo { n: "a", f: None };
  let ok = Foo { n: "b", f: Some(f) };
  ok.inspect();
  ()
}
```

Recursive functions should work even when not declared at the top-level.

```rust
fn borgo_main() {
  fn foo(n: Int, acc: Int) -> Int {
    if n == 0 {
      return acc
    }

    let new_acc = if n % 2 == 0 {
      acc + n
    } else {
      acc
    };

    foo(n - 1, new_acc)
  }

  foo(10, 0).assert_eq(30)
}
```

Exhaustiveness checking on bools

```rust
fn borgo_main() {
  let x = match false {
    true => unreachable!(),
    false => 2,
  };
  x.assert_eq(2);
}
```

String parsing

```rust
fn borgo_main() {
  "2".parse_int().assert_eq(Some(2));
  "abc".parse_int().assert_eq(None);
  "3.4".parse_float().assert_eq(Some(3.4));
}
```

Primitive types are casted in struct call

```rust
struct Foo {
  bar: Int
}

fn borgo_main() {
  let x = 1;
  let y = Foo { bar: x };
  y.bar.assert_eq(1);
}
```

Newlines in strings

```rust
fn borgo_main() {
  let s = "a
b
c";

  let s = s.split("\n");
  s.to_list().assert_eq(["a", "b", "c"]);
}
```

Match on tuples

```rust
fn borgo_main() {
    let res = match (1, "foo") {
        (3, _) => 5,
        (1, "bar") => 6,
        (x, "foo") => x,
        _ => unreachable!(),
    };

    res.assert_eq(1);

    let res = match () {
      () => 2,
    };

    res.assert_eq(2);
}
```

Enums in tuples

```rust
enum Foo { Bar, Baz }

fn borgo_main() {
    let res = match (Bar, Baz) {
        (Bar, Bar) => 0,
        (Bar, Baz) => 2,
        _ => unreachable!(),
    };

    res.assert_eq(2);
}
```

Let binding same name as function param

```rust
fn foo(xs: List<Int>) -> Int {
  let xs = xs.seq().sum();
  xs + 10
}

fn borgo_main() {
  foo([1,2,3]).assert_eq(16);
}
```

Maps in structs

```rust
struct Foo {
  bar: Map<String, Int>
}

fn borgo_main() {
  let bar = Map::new();
  let foo = Foo { bar };
  foo.bar.len().assert_eq(0);
}
```

Functions in structs

```rust
struct Foo {
  bar: fn (Int) -> Int
}

fn borgo_main() {
  let foo = Foo { bar: |x: Int| x + 2 };
  foo.bar(1).assert_eq(3);
}
```

Hashing with Hash trait

```rust
fn borgo_main() {
  let a = "yo";
  let b = "y".append("o");

  Hash::to_hash(a)
    .assert_eq(Hash::to_hash(b));
}
```

Equality with Eq trait

```rust
struct Foo { a: Int, b: String }

fn borgo_main() {
  let x = Foo { a: 1, b: "yo" };
  let y = Foo { a: 1, b: "yo" };
  Eq::equals(x, y).assert_eq(true);
  Eq::equals(x, Foo { a: 2, ..y}).assert_eq(false);
}
```

Display trait

```rust
struct Foo { a: Int, b: String }
struct Bar { baz: fn () -> Int }
enum Color { Blue, Red }

fn borgo_main() {
  Display::to_string(1).inspect();
  Display::to_string(false).inspect();
  Display::to_string("yo").inspect();
  Display::to_string('a').inspect();

  let x = Foo { a: 1, b: "yo" };
  Display::to_string(x).inspect();

  Display::to_string([1,2,3]).inspect();
  Display::to_string(Ok(Color::Red)).inspect();
  Display::to_string((1, 2.3)).inspect();
  Display::to_string(()).inspect();

  let m = Map::new()
    .insert("a", Color::Blue)
    .insert("b", Color::Red);
  Display::to_string(m).inspect();

  Display::to_string(5.to_ref()).inspect();

  Display::to_string(Bar { baz: || 1 }).inspect();

  Display::to_string([1,2,3].seq()).inspect();

  let s = Seq::infinite(0, |x| x + 1);
  Display::to_string(s).inspect();

  ()
}
```

Records have stable field order.

```rust
struct Foo { x: Int, y: String }

fn borgo_main() {
  (1, "a", true).inspect();
  Foo { x: 1, y: "b" }.inspect();
  ()
}
```
