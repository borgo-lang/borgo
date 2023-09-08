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

impl (f: Foo) {
    fn method() -> int {
        f.a + 5
    }
}

fn foo() -> int {
    fn bar() -> int {
        let x = Foo { a: 1 }
        x.method()
    }

    bar()
}
```

Can use different generic name than declaration.

> infer("fn () -> ()")

```rust
struct Foo<T> {}

impl<Y> (f: Foo<Y>) {
    fn bar(x: int, y: Y) {}
}

fn main() {}
```

Exhaustive check

> errorContains('exhaustive'); errorContains('missing case: \"Color.Blue\"');

```rust
enum Color { Blue, Red, Green }

fn main() -> int {
    let x = Color.Red
    match x {
        Color.Red => 1,
        Color.Green => 2,
    }
}
```

Exhaustive check works with unqualified constructors.

> errorContains('exhaustive'); errorContains('missing case: \"Foo.B\"');

```rust
enum Foo { A, B, Baz }

fn main() -> int {
    match Foo.B {
        A => 1,
        Foo.Baz => 2,
    }
}
```

Type checking still works.

> errorContains("mismatch")

```rust
enum Foo { A, B, Baz }

fn foo(a: Foo) -> int { a + 1 }
fn main() {}
```

Const expressions are type checked

> errorContains('mismatch')

```rust
const a: string = 1
```

Catch exhaustiveness errors.

> errorContains("constructor Green not found")

```rust
enum Color { Red, Blue }

fn main() {
    match Color.Red {
        Red => 1,
        Green => 2,
    }
}
```

Bools missing arm

> errorContains('missing case: \"true\"')

```rust
fn main() {
    match false {
        false => (),
    }
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
fn main() -> int {
    false > true
    1
}
```

Pattern matching on slice literals.

> errorContains("pattern match on slice literals")

```rust
fn main() {
    match [1] {
        [1,2] => false,
        _ => true,
    }
}
```

For loops.

> infer("fn () -> ()")

```rust
fn main() {
    for x in ["a"] {
        @ensure x, string
    }

    for (index, value) in ["a"].Enumerate() {
        @ensure index, int
        @ensure value, string
    }
}
```

Expr in loops must be rangeable.

> errorContains("iterate")

```rust
fn main() {
    for x in 1 {
    }
}
```

Loops with no condition

> infer("fn () -> ()")

```rust
fn main() {
    loop {
        break
    }
}
```

If expression with an expected type must have an else block

> errorContains("if false")

```rust
fn main() {
    if true {
        let x = "hello"
        1
    }

    let a = if false {
        let y = "yo"
        2
    }
}
```

Method calls work for enums.

> infer("fn () -> int")

```rust
enum Foo {
  Bar(int)
}

impl (f: Foo) {
    fn method(s: string, b: int) -> int {
        match f {
          Bar(a) => a + b,
        }
    }
}

fn main() -> int {
    let f = Foo.Bar(1)
    @ensure f.method("a", 2), int

    let m = f.method
    m("a", 2)
}
```

Generics in extern blocks

> infer("fn () -> ()")

```rust
use reflect

fn main() {
    let a = reflect.DeepEqual(1, false)
    let b = reflect.DeepEqual(1, 1)
}
```

While loops.

> infer("fn () -> ()")

```rust
fn main() {
    let x = 1
    while x < 10 {
        @ensure x, int
    }
}
```

Enumeration in loops expect a tuple.

> errorContains("Use tuple literals")

```rust
fn main() {
    let m = Map.new()
    for e in m {
        e.0
    }
}
```

Access slices by index

> infer("fn () -> string")

```rust
fn main() -> string {
    let xs = ["a"]
    xs[0]
}
```

Index must be int for slices

> errorContains("mismatch")

```rust
fn main() {
    let xs = ["a"]
    xs[false]
}
```

Access maps by index

> infer("fn () -> int")

```rust
fn main() -> int {
    let xs = Map.new()
    xs.Insert("a", 1)
    xs["a"]
}
```

Index must be K for maps

> errorContains("mismatch")

```rust
fn main() {
    let xs = Map.new()
    xs.Insert("a", 1)
    xs[false]
}
```

Parse traits

> infer("fn () -> ()")

```rust
fn main() {
    interface Foo {
        fn bar(x: string) -> int
    }

    fn check(f: Foo) -> int {
        f.bar("yo")
    }
}
```

Check if type implements trait

> infer("fn () -> int")

```rust
struct Baz { x: int }

impl (b: Baz) {
    fn bar(_: string) -> int {
      b.x
    }
}

fn main() -> int {
    interface Foo {
        fn bar(x: string) -> int
    }

    fn check(f: Foo) -> int {
        f.bar("yo")
    }

    check(Baz { x: 1 })
}
```

Trait bounds

> infer("fn () -> int")

```rust
struct Baz { x: int }

impl (b: Baz) {
    fn bar(_: string) -> int {
        b.x
    }
}

fn main() -> int {
    interface Foo {
        fn bar(x: string) -> int
    }

    fn check<T: Foo>(f: T) -> int {
        f.bar("yo")
    }

    check(Baz { x: 1 })
}
```

Trait bounds checked at call site

> errorContains("method foo not found on type int")

```rust
fn main() {
    interface Foo { fn foo() -> int }
    fn check<T: Foo>(f: T) {}
    check(1)
}
```

Variadic functions

> infer("fn () -> ()")

```rust
fn main() {
    fn foo(a: int, b: VarArgs<string>) {}

    foo(1)
    foo(1, "a")
    foo(1, "a", "b", "c")
}
```

Variadic functions arity error

> errorContains("Wrong arity")

```rust
fn main() {
    fn foo(a: int, b: VarArgs<string>) {}
    foo()
}
```

References are maintained after try call

> infer("fn () -> ()")

```rust
use os

fn foo() -> Result<()> {
    let f = os.Open("file")?
    Ok(bar(f))
}

fn bar(f: *os.File) {}

fn main() {
    foo()
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
type Foo<V> = Map<int, V>
type Complex<A, B> = Map<bool, Map<A, Map<B, string>>>

fn foo(m: Foo<string>) -> string {
    m[1]
}

fn takes_map(m: Map<int, string>) -> string {
    m[1]
}

fn main() {
    let m: Foo<string> = Map.new()
    m.Insert(1, "a")

    foo(m)
    takes_map(m)

    let c: Complex<int, bool> = Map.new()
    @ensure c, Map<bool, Map<int, Map<bool, string>>>
}
```

Tuple structs

> infer("fn () -> Foo")

```rust
struct Foo(int)

struct Bar<T>(int, T)

fn check(f: Foo) -> int {
    f.0
}

fn bar_check(b: Bar<string>) -> (int, string) {
    (b.0, b.1)
}

fn main() -> Foo {
    let b = Bar(1, false)
    let c = Bar(1, "yo")
    bar_check(c)

    @ensure check(Foo(1)), int
    Foo(2)
}
```

Type aliases to non existing types

> errorContains("Type not found: Bar")

```rust
type Foo = Bar
```

Interfaces as arguments

> infer("fn () -> fn () -> int")

```rust
use bufio
use os

fn main() -> fn () -> int {
    let reader = bufio.NewReader(os.Stdin)
    reader.ReadString
}
```

Impl blocks

> infer("fn () -> ()")

```rust
enum Color { Red, Blue }

impl (c: Color) {
    fn is_red() -> bool {
        c == Color.Red
    }
}

fn main() {
    @ensure Color.is_red(Color.Red), bool
}
```

Impl blocks with generic types

> infer("fn () -> ()")

```rust
enum Foo<T, Y> { Bar(T), Baz(Y) }

fn make_bar<T, Y>(x: T) -> Foo<T, Y> { Foo.Bar(x) }

impl<T, Y> (f: Foo<T, Y>) {
    fn do_stuff(y: Y) -> Y {
        @ensure f, Foo<T, Y>

        match f {
            Foo.Bar(x) => y,
            Foo.Baz(yy) => yy,
        }
    }
}

fn main() {
    let foo: Foo<int, bool> = make_bar(1)
    foo.do_stuff(true)
}
```

Impl for non-existing types

> errorContains("not found")

```rust
impl (u: Unknown) { }
```

Impl with wrong generics

> errorContains("Wrong arity")

```rust
enum Foo<T, Y> { Bar }
impl<T, Y> (f: Foo<T>) { }
```

Generics in impl method

> infer("fn () -> ()")

```rust
struct Foo<T> { bar: T }

impl<T> (f: Foo<T>) {
    fn map(transform: fn (x: T) -> string) -> string {
        transform(f.bar)
    }

    fn other() -> string {
        f.map(|x| "asdf")
    }
}

fn main() {
    let foo = Foo { bar: 12 }
    foo.other()
}
```

Prevent generics from getting instantiated to non existing types

> errorContains("Type not found: K")

```rust
enum Foo<T, Y> { Bar(T), Baz(Y) }

impl<T, Y> (f: Foo<T, Y>) {
    fn do_stuff(y: Y) -> Y {
        @ensure f, Foo<T, K>
        y
    }
}
```

Unknown type in impl with generics

> errorContains("Type not found: T")

```rust
struct Foo<T> {}
impl (f: Foo<T>) {}
```

Type errors in method chains

> errorContains(".foo")

```rust
struct Foo {}
struct Bar {}
struct Baz {}

impl (f: Foo) { fn foo() -> int { 1 } }
impl (b: Bar) { fn bar() -> Baz { Baz{} } }
impl (b: Baz) { fn baz() -> Bar { Bar{} } }

fn main() {
    Bar{}
    .bar()
    .baz()
    .foo()
}
```

Mutable reference as method receiver

> infer("fn () -> ()")

```rust
struct Foo { x: int }

impl (f: *Foo) {
    fn bar() {
        f.x = 5
    }
}

fn main() {
    let mut f = Foo { x: 3 }
    f.bar()
}
```

Static functions

> infer("fn () -> ()")

```rust
struct Foo { x: int }

fn Foo.new(x: int) -> Foo {
    Foo { x }
}

impl (f: Foo) {
    fn check() -> int {
        f.x
    }
}

fn main() {
    let f = Foo.new(1);
    f.check();
}
```

Access struct fields from package

> infer("fn () -> ()")

```rust
use net.http

fn main() {
    let code = http.Get("foo").Unwrap().StatusCode
    @ensure code, int
}
```

Interface check with supertraits

> infer("fn () -> ()")

```rust
use io

fn foo(r: io.Reader) {
}

fn bar() -> io.ReadCloser {
    @rawgo ("")
}

fn main() {
    foo(bar())
}
```

Well formed select

> infer("fn () -> ()")

```rust
fn foo(tx: Sender<string>, rx: Receiver<int>) {
    select {
        let bar = rx.Recv() => {
            @ensure bar, int
        }

        tx.Send("hi") => ()
    }
}

fn main() {}
```
