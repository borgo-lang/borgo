# Parsing expressions

Turn a source string into `ast::Expr`

---

Literals.

> parse('expr')

```rust
1
```

Precedence I

> parse('expr')

```rust
3 * 1 + 2
```

Precedence II

> parse('expr')

```rust
5 + 4 * 7
```

Unary

> parse('expr')

```rust
-2 + 3
```

Unary II

> parse('expr')

```rust
1 + -5
```

Parens

> parse('expr')

```rust
1 * (2 + 3)
```

Ident

> parse('expr')

```rust
1 * b
```

Field selector

> parse('expr')

```rust
a.b + 4
```

Nested field access

> parse('expr')

```rust
1 * a.b.c + 3
```

Function call

> parse('expr')

```rust
foo() + 7
```

Function call with args

> parse('expr')

```rust
foo.bar(!false, 3 * 4)
```

Blocks

> parse('expr')

```rust
{
    foo()
    bar()
}
```

Let statements

> parse('expr')

```rust
{
    let x = 1
}
```

Nested blocks

> parse('expr')

```rust
{
    {}
    let x = {
        foo()
        bar()
    }
    x
}
```

Functions

> parse('expr')

```rust
fn foo<T>(a: T, b: int) -> int {
    b
}
```

Interface bounds

> parse('expr')

```rust
fn bar<T: Reader + Writer>() { }
```

Match expressions

> parse('expr')

```rust
match x {
    Option.Some(x) => 45 + x

    Option.None => {
        foo()
        bar()
    }
}
```

Match expressions II

> parse('expr')

```rust
match Result.Ok(90) {
    Ok(x) => 45 + x
    _ => 2
}
```

If expressions

> parse('expr')

```rust
if x {
    1
}
```

If expressions II

> parse('expr')

```rust
if x.a.b {
    2
} else {
    3
}
```

If expressions III

> parse('expr')

```rust
if u {
    4
} else if z {
    5
} else {
    6
}
```

Return

> parse('expr')

```rust
fn foo() {
    let x = 1
    return x
}
```

Try operator

> parse('expr')

```rust
a.b?.foo()?
```

Lambdas

> parse('expr')

```rust
|x: int, y: Foo<T>| {
    foo(x)
    y
}
```

Lambdas II

> parse('expr')

```rust
|x, y| x + y
```

Struct call

> parse('expr')

```rust
Foo { bar: 1 + 1, baz }
```

Struct call II

> parse('expr')

```rust
pkg.Foo { bar: 1, ..x }
```

Function type annotations

> parse('expr')

```rust
{
    let a: fn (x: int) -> Option<int> = 1
    // bindings in annotations are always required
    // ie. this is not allowed
    // let b: fn (int) = 2
}
```

Tuples

> parse('expr')

```rust
(a, b, 3 + 5)
```

Match on structs

> parse('expr')

```rust
match x {
    Foo { a: 1, b } => 5
}
```

Match on tuples

> parse('expr')

```rust
match x {
    (a, 34) => 2
}
```

Method precedence

> parse('expr')

```rust
Bar{}.foo()?
```

Slices

> parse('expr')

```rust
[1, 2, 3]
```

Strings and chars

> parse('expr')

```rust
("hello", 'a')
```

Enum definition

> parse('file')

```rust
enum Foo<T> {
    Bar,
    Baz(int, Option<T>),
}
```

Struct definition

> parse('file')

```rust
struct Foo<T> {
    bar: Bar,
    baz: Baz<T, int>
}
```

Index expression

> parse('expr')

```rust
a.b[34]
```

Assign statement

> parse('expr')

```rust
{
    let a = 1
    a[10] = 5
}
```

Interface

> parse('file')

```rust
interface Foo<T: Comparable> {
    impl Bar
    impl Baz<T>

    fn A()
    fn B(a: int) -> string
}
```

Const

> parse('file')

```rust
const foo = 120
```

Methods can span multiple lines

> parse('expr')

```rust
foo()
    .bar[4]
    .baz(
        // trailing commas are required when function
        // calls span multiple lines.
        "hi",
    )
```

Impl blocks

> parse('file')

```rust
impl<T, Y> (foo: Foo<T, Y>) {
    fn bar(x: T) -> Y {
        foo.baz()
    }

    fn baz() {}
}
```

One liner block

> parse('expr')

```rust
{ 1 }
```

Slice type

> parse('expr')

```rust
fn foo() -> [int] { [1, 2] }
```

Tuple type

> parse('expr')

```rust
fn foo() -> (int, string) { (3, "bar") }
```

Comments are skipped

> parse('expr')

```rust
{
    // leading comment
    let first = 1

    foo(
        bar,
        // trailing comment
    )
}
```

Let mut statement

> parse('expr')

```rust
{
    let mut a = 1
}
```

Comments at end of file

> parse('file')

```rust
// comment at beginning
fn foo() {}
// comment at end
```

Defer statements

> parse('expr')

```rust
{
    defer db.close()

    foo()
}
```

Standalone returns

> parse('expr')

```rust
{
    return
}
```

Spawn statements

> parse('expr')

```rust
{
    spawn || { sender.send("hi") }()
}
```

Loop statements

> parse('expr')

```rust
{
    for i in [1,2].enumerate() {
        foo(i)
    }

    while x > y {
        // keep going
        continue
    }

    loop {
        i = i + 1
        break
    }
}
```

Inline comments

> parse('expr')

```rust
{
    foo // one
    bar // two
}
```

References and pointers

> parse('file')

```rust
struct Foo {
    a: *Bar,
}

fn bar(f: *Foo) {
    Foo { a: &Bar { b: 123 } }
}
```

Pointer dereference

> parse('expr')

```rust
{
    a.b.* = 123
}
```

Tuple fields

> parse('expr')

```rust
{
    let a = (true, 1)
    a.0
}
```

Inline functions

> parse('file')

```rust
fn foo() {
    fn bar() {
        baz
    }
}
```

Newtypes (tuple structs)

> parse('file')

```rust
struct Foo<T>(Bar<int>)
```

Multistrings

> parse('expr')

```rust
{
    foo(
        \\hello
        \\  this is a
        \\multiline \\ string
    , 123
    )
}
```

Type aliases

> parse('file')

```rust
type Foo<T> = Map<T, int>
```

Not operator precedence

> parse('expr')

```rust
{
    !o.bar()
    !!false
}
```

Semicolon after RAngle

> parse('file')

```rust
interface File {
  fn Read (param0: [byte]) -> Result<int>
  fn Close () -> error
}
```

Functions with dot

> parse('file')

```rust
fn Bar.baz() {}
```

Select statements

> parse('expr')

```rust
{
    select {
        let x = foo.Recv() => {
        }

        bar.Send() => {
        }

        _ => {
        }
    }
}
```
