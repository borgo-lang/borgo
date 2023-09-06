# Type inference for expressions

What's the type of an `ast::Expr`?

---

Literals.

> infer("int")

```rust
1
```

Blocks get the type of the last expression.

> infer("fn () -> ()")

```rust
{ fn foo() {} }
```

Block ends with function

> infer("fn () -> int")

```rust
{ fn foo() -> int { 1 } }
```

Block ends with function 2

> infer("fn (int) -> int")

```rust
{ fn foo(bar: int) -> int { bar } }
```

Block ends with function 3

> infer("fn (int, bool) -> int")

```rust
{ fn foo(bar: int, baz: bool) -> int { bar } }
```

Block ends with function 4

> infer("fn <A>(A, bool) -> A")

```rust
{ fn foo<T>(bar: T, baz: bool) -> T { bar } }
```

Sanity check: types must exist

> errorContains("Type not found: T")

```rust
{ fn foo(x: T) -> T { x } }
```

Block ends with list

> infer("fn () -> [int]")

```rust
{ fn foo() -> [int] { [1] } }
```

Block ends with generic list

> infer("fn <A>(A) -> [A]")

```rust
{ fn foo<Y>(bar: Y) -> [Y] { [bar, bar] } }
```

Generic arguments 1

> errorContains("Wrong arity")

```rust
{ fn foo(bar: Result<string, int, bool>) {} }
```

Generic arguments 2

> infer("fn () -> [[int]]")

```rust
{ fn foo() -> [[int]] { [[1]]} }
```

Generic arguments 3

> errorContains("Wrong arity")

```rust
{ fn foo(bar: int<bool>) {} }
```

HKTs are not supported.

> errorContains("Wrong arity")

```rust
{ fn foo<T>(bar: T<bool>) {} }
```

Populate scope with let bindings.

> infer("int")

```rust
{
  let x = 5
  x
}
```

Function call with no arguments.

> infer("string")

```rust
{ hello() }
```

Function call with arguments

> infer("int")

```rust
{ sum(1, 4) }
```

Wrong arity #1

> errorContains("arity")

```rust
{ hello(1) }
```

Wrong arity #2

> errorContains("arity")

```rust
{ sum(1) }
```

Wrong param type

> errorContains("mismatch")

```rust
{ sum("asdf", 1) }
```

Slice literals

> infer("[int]")

```rust
{ [1,2,3] }
```

Slices are polymorphic

> infer("[string]")

```rust
{
  let x = [1,2,3]
  list_push(x, 4)
  ["hello"]
}
```

It's possible to sanity-check the type of a value.

> infer("[int]")

```rust
{
  let x = [1,2,3]
  @ensure x, [int]
  x
}
```

And the check should work!

> errorContains("mismatch")

```rust
{
  let x = [1,2,3]
  @ensure x, bool
  1
}
```

Functions should get inferred correctly.

> infer("fn <A, B>([A], fn (A) -> B) -> [B]")

```rust
list_map
```

Functions can take functions.

> infer("[int]")

```rust
{
  let x = [1,2,3]
  list_map(x, inc)
}
```

Generics are applied correctly.

> errorContains("mismatch")

```rust
{
  let x = ["hello"]
  list_map(x, inc)
}
```

Functions can be inlined.

> infer("[int]")

```rust
{
  let x = [1,2,3]
  list_map(x, |n| sum(n, 5))
}
```

Closures can be inferred.

> infer("fn (int) -> int")

```rust
|n| sum(n, 1)
```

Functions are in scope after declaration.

> infer("int")

```rust
{
  fn foo(a: int) -> int { sum(a, 4) }
  foo(1)
}
```

Generic functions get instantiated at each use site.

> infer("int")

```rust
{
  fn constant<T, Y>(a: T, b: Y) -> T { a }

  let x = constant("hello", 5)
  @ensure x, string
  let y = sum(constant(1, false), 5)
  @ensure y, int
  y
}
```

If expressions.

> infer("int")

```rust
if true {
  1
} else {
  2
}
```

Condition must be a bool.

> errorContains("mismatch")

```rust
if 12 {
  1
}
```

Both branches should have the same type.

> errorContains("mismatch")

```rust
if true {
  1
} else {
  "hello"
}
```

Match expressions.

> infer("string")

```rust
match 1 {
  1 => "foo",
}
```

Patterns in a match arm should unify with subject.

> errorContains("mismatch")

```rust
match 1 {
  1 => "foo",
  false => "foo",
}
```

Wildcard matches.

> infer("string")

```rust
match 1 {
  1 => "foo",
  _ => "foo",
}
```

All arms should have the same type.

> errorContains("mismatch")

```rust
match 1 {
  1 => "foo",
  2 => false,
}
```

Aliasing in a match arm.

> infer("string")

```rust
match 1 {
  1 => "foo",
  foo => int_to_string(foo),
}
```

Match and If are still expressions.

> infer("int")

```rust
{
  let x = "foo"

  let a = match x {
    "foo" => 1,
    _ => 99,
  }

  let cond = false

  let b = if cond {
    0
  } else {
    1
  }

  sum(a, b)
}
```

Unit type.

> infer("()")

```rust
()
```

Tuples.

> infer("(int, int)")

```rust
(1, 2)
```

Tuples with wrong type annotation.

> errorContains("mismatch")

```rust
@ensure ("foo", 2), (bool, int)
```

Tuples with wrong arity.

> errorContains("mismatch")

```rust
@ensure (1, 2, 3), (int, int)
```

Tuples as function arguments.

> infer("fn ((int, bool)) -> int")

```rust
{
  fn foo(t: (int, bool)) -> int {
    fst(t)
  }
}
```

Tuples as return type.

> infer("fn () -> (string, bool)")

```rust
{
  fn foo() -> (string, bool) {
    ("foo", true)
  }
}
```

Typing callbacks.

> infer("string")

```rust
{
  fn foo( f: fn (i: int) -> string, x: int ) -> string {
    f(x)
  }

  foo(|x| int_to_string(x), 1)
}
```

Callbacks with generics

> infer("[int]")

```rust
{
  fn another_map<A, B>( f: fn (x: A) -> B, xs: [A] ) -> [B] {
    // mimics implementation
    let first = list_head(xs)
    [f(first)]
  }

  let a = another_map(|x| int_to_string(x), [1,2,3])
  @ensure a, [string]

  another_map(|x| sum(x, 1), [1,2,3])
}
```

Enums should introduce a new type in the environment.

> infer("Color")

```rust
{
  enum Color { Red }
  Color.Red
}
```

Enums with generics should get instantiated.

> infer("Maybe<int>")

```rust
{
  enum Maybe<T> { Just(T), None }
  Maybe.Just(1)
}
```

Enum variants get instantiated correctly.

> infer("Maybe<A>")

```rust
{
  enum Maybe<T> { Just(T), None }

  @ensure Maybe.Just(1), Maybe<int>
  @ensure Maybe.Just("foo"), Maybe<string>
  @ensure Maybe.None, Maybe<bool>
  @ensure Maybe.Just(Maybe.Just(1)), Maybe<Maybe<int>>

  Maybe.None
}
```

Pattern matching works on enums.

> infer("string")

```rust
{
  enum Color { Red, Blue(string) }

  let x = Color.Blue("foo")
  match x {
    Color.Red => "bar",
    Color.Blue(c) => str_concat(c, "!"),
  }
}
```

Pattern matching constructor arity.

> errorContains("arity")

```rust
{
  enum Color { Red, Blue(string) }

  match Color.Blue("foo") {
    Color.Red => "bar",
    Color.Blue(c, nope) => c,
  }
}
```

Generic enums can be pattern matched.

> infer("int")

```rust
{
  enum Maybe<T> { Just(T), None }

  let x = Maybe.Just(1)
  match x {
    Maybe.Just(v) => v,
    Maybe.None => 2,
  }
}
```

Nested pattern matching is supported.

> infer("Color")

```rust
{
  enum Maybe<T> { Just(T), None }
  enum Color { Red, Blue(string, bool) }

  let c = Color.Blue("foo", false)
  let x = Maybe.Just(c)

  let n = match x {
    Maybe.Just(Color.Blue(s, b)) => {
      @ensure s, string
      @ensure b, bool
      1
    },
    Maybe.None => 2,
  }

  @ensure n, int

  match x {
    Maybe.Just(x) => x,
    Maybe.None => Color.Red,
  }
}
```

Enums can be returned from functions.

> infer("fn <A>(A) -> Maybe<A>")

```rust
{
  enum Maybe<T> { Just(T), None }

  fn ok<T>(v: T) -> Maybe<T> {
    Maybe.Just(v)
  }

  @ensure ok(1), Maybe<int>
  @ensure ok("foo"), Maybe<string>

  ok
}
```

Enums from functions II.

> infer("int")

```rust
{
  enum Maybe<T> { Just(T), None }

  fn foo(b: int) -> Maybe<int> {
    Maybe.None
  }

  @ensure foo(1), Maybe<int>

  fn bar(b: int) -> Maybe<int> {
    Maybe.Just(b)
  }

  match @ensure foo(2), Maybe<int> {
    Maybe.Just(x) => x,
    Maybe.None => 3,
  }
}
```

Enums from functions (error).

> errorContains("mismatch")

```rust
{
  enum Maybe<T> { Just(T), None }

  fn foo(b: int) -> Maybe<string> {
    Maybe.Just(b)
  }
}
```

Defining structs.

> infer("Foo")

```rust
{
  struct Foo { a: int }

  Foo { a: 1 }
}
```

Generic structs.

> infer("Foo<int>")

```rust
{
  struct Foo<T> { a: T }

  Foo { a: 1 }
}
```

Generic structs with multiple params.

> infer("Foo<int, bool>")

```rust
{
  struct Foo<T, Y> { a: T, b: Y, c: T }

  Foo { a: 1, b: false, c: 3 }
}
```

Structs with not enough arguments.

> errorContains("Missing fields")

```rust
{
  struct Foo { a: int, b: int }

  Foo { a: 1 }
}
```

Structs with too many arguments.

> errorContains("field not found")

```rust
{
  struct Foo { a: int, b: int }

  Foo { a: 1, b: 2, c: 3 }
}
```

Accessing struct fields.

> infer("int")

```rust
{
  struct Foo { a: int }

  let x = Foo { a: 1 }
  x.a
}
```

Error when reassign to wrong type.

> errorContains("mismatch")

```rust
{
  let mut x = 1
  x = "foo"
}
```

Error when updating structs.

> errorContains("mismatch")

```rust
{
  struct Foo { a: int }

  let mut x = Foo { a: 1 }
  x.a = "foo"
}
```

Structs can be updated

> infer("Foo")

```rust
{
  struct Foo { a: int }

  let mut x = Foo { a: 1 }
  x.a = 3
  x
}
```

Structs need mutability

> errorContains("mutable")

```rust
{
  struct Foo { a: int }

  let x = Foo { a: 1 }
  x.a = 5
}
```

Nested struct update

> infer("Bar")

```rust
{
  struct Foo { x: int }
  struct Bar { f: Foo }

  let mut b = Bar { f: Foo { x: 1 } }
  b.f.x = 99
  b
}
```

Spreading a struct into another.

> infer("Foo")

```rust
{
  struct Foo { a: int, b: string }

  let x = Foo { a: 1, b: "foo" }
  Foo { b: "bar", ..x }
}
```

Error when spreading in different structs.

> errorContains("mismatch")

```rust
{
  struct Foo { a: int, b: string }
  struct Bar { a: int, b: string }

  let x = Foo { a: 1, b: "foo" }
  let y = Bar { a: 1, b: "bar" }
  Foo { b: "bar", ..y }
}
```

Nested updates.

> infer("Bar")

```rust
{
  struct Foo { a: int, b: string }
  struct Bar { a: int, b: string, f: Foo }

  let x = Foo { a: 1, b: "foo" }
  let y = Bar { a: 1, b: "bar", f: x }
  Bar { a: 2, b: "baz", f: Foo { a: 4, ..x } }
}
```

Let bindings with annotation.

> errorContains("mismatch")

```rust
{
  let x: [int] = [1,2,3]
  let y: [bool] = [1,2,3]
}
```

Return statements should unify.

> infer("fn () -> int")

```rust
{
  fn foo() -> int {
    let x = 5
    return x
  }
}
```

Early return statements should unify with return type.

> errorContains("mismatch")

```rust
{
  fn foo() -> int {
    if true {
      return "foo"
    }

    return 1
  }
}
```

Multiple early return all unify.

> errorContains("mismatch")

```rust
{
  fn foo() -> int {
    if true {
      return 1
    }

    if true {
      return "bar"
    }

    2
  }
}
```

A function within a function gets its own return type.

> infer("fn () -> int")

```rust
{
  fn foo() -> int {

    fn bar() -> string {
      if true {
        return "foo"
      }

      "bar"
    }


    @ensure bar(), string

    return 2
  }
}
```

Can't have return statements outside of functions.

> errorContains("return")

```rust
{
  return false
}
```

Errors must be of the same type.

> errorContains("mismatch")

```rust
{
  fn foo() -> Result<(), ErrFoo> {
    if true {
      return Err(ErrFoo.A)
    }

    Err(ErrBar.X)
  }
}
```

Function calls with different errors don't unify.

> errorContains("mismatch")

```rust
{
  fn foo1() -> Result<int, ErrFoo> { Err(ErrFoo.A) }
  fn foo2() -> Result<int, ErrFoo> { Err(ErrBar.X) }
  fn bar() -> Result<int, ErrFoo> {
    foo1()?
    foo2()?
    Ok(1)
  }
}
```

Functions with no error get inferred ok.

> infer("[int]")

```rust
{
  list_map([1,2,3], |x| sum(x, 1))
}
```

Callbacks with errors are automatically wrapped into a Result.

> infer("[Result<int, ErrFoo>]")

```rust
{
  fn foo(x: int) -> Result<int, ErrFoo> {
      return Err(ErrFoo.A)
      Ok(x)
  }
  list_map([1,2,3], foo)
}
```

Callbacks with different error types can still be passed to the same function.

> infer("bool")

```rust
{
  fn foo() -> Result<int, ErrFoo> { Err(ErrFoo.A) }
  fn bar() -> Result<int, ErrBar> { Err(ErrBar.X) }

  fn baz(f: fn () -> Result<int, ErrFoo>, g: fn() -> Result<int, ErrBar>) {
    ()
  }

  @ensure foo, fn () -> Result<int, ErrFoo>
  @ensure bar, fn () -> Result<int, ErrBar>

  baz(foo, bar)
  true
}
```

A function that is already wrapped, doesn't get wrapped again.

> infer("bool")

```rust
{
  fn foo(x: int) -> Result<string, ErrFoo> { Err(ErrFoo.A) }

  fn bar(f: fn (x: int) -> Result<string, ErrFoo>) {
    ()
  }

  bar(foo)
  true
}
```

The `?` operator can only be used in functions that return a Result.

> errorContains("mismatch")

```rust
{
  fn foo() -> Result<int, ErrFoo> { Ok(1) }
  fn bar() {
    let x = foo()?
    ()
  }
}
```

Equality

> infer("bool")

```rust
{
  let a = 1
  let b = 2
  a == b
}
```

Can shadow vars in block

> infer("string")

```rust
{
  let a = "mutable"
  {
    let a = "block"
  }
  a
}
```

Math operands on ints

> infer("int")

```rust
{ 1 + 5 }
```

Wrong type in operands

> errorContains("numeric type")

```rust
{ 1 + false }
```

boolean operators

> infer("bool")

```rust
{ (1 == 2) && (false || true) }
```

Match weirdness

> infer("bool")

```rust
{
    match false {
        true => print("asdf"),
    }

    match 1 {
        1 => false,
    }
}
```

Match Type.Fun with Type.Con

> errorContains("mismatch")

```rust
{
  fn bar() -> bool { false }
  fn foo() -> int {
    bar
  }
}
```

Negative numbers

> infer("int")

```rust
5 * -3
```

Negate expressions

> infer("bool")

```rust
!false
```

Neg only works with numbers

> errorContains("numeric type")

```rust
-false
```

Not only works on bools

> errorContains("mismatch")

```rust
!"asf"
```

Generic function arguments should not get instantiated

> errorContains("mismatch")

```rust
{
  fn foo<T>(x: T) -> int {
    @ensure x, T
    match x {
      1 => 1
    }
  }
}
```

Sanity check, can't make up types out of thin air

> errorContains("not found")

```rust
{
  fn foo<T>(x: Y) -> int {
    1
  }
}
```

Characters

> infer("rune")

```rust
'a'
```

Tuple fields

> infer("int")

```rust
{
  let a = (1, "a")
  a.0
}
```

Sanity check: can't invent constructors in pattern match

> errorContains("Green not found")

```rust
{
  enum Color { Red, Blue }

  match Color.Red {
    Red => 1,
    Green => 2,
  }
}
```

Struct inference in lambdas

> infer("[string]")

```rust
{
  struct Foo { a: int, b: string }

  let f = Foo { a: 1, b: "a" }
  list_map([f], |x| str_concat(x.b, "!"))
}
```

Generics should get instantiated to the same type in pattern match

> errorContains("mismatch")

```rust
{
  enum Foo<T> {
    Bar(T),
    Baz(T),
  }

  match Foo.Bar(12) {
    Bar(x) => {
      let a = x + 1
      int_to_string(a)
    },

    Baz(y) => str_concat(y, "doh"),
  }
}
```

Floats

> infer("float64")

```rust
9.12
```

Math ops with floats

> infer("float64")

```rust
1.3 + 5.2
```

Operands between floats and ints

> infer("float64")

```rust
3.1 * 4
```

Plus operator works with strings

> infer("string")

```rust
"a" + "b"
```

Bidirectional inference for closure params

> infer("int")

```rust
{
  struct Foo { a: int }

  fn bar<T, Y>(start: T, f: fn (x: T) -> Y) -> Y {
    f(start)
  }

  bar(Foo { a: 1 }, |x| x.a)
}
```

Struct patterns

> infer("int")

```rust
{
  struct Foo { x: int, y: string }

  let f = Foo { x: 1, y: "a" }
  match f {
    Foo { x, y } => x
  }
}
```

Tuple patterns

> infer("int")

```rust
{
  fn foo((x, y): (int, string)) -> int { x }
  foo((1, "a"))
}
```

Pattern with too many arguments

> errorContains("mismatch")

```rust
{
  fn foo((x, y, z): (bool, bool)) -> bool { x }
  foo((false, false))
}
```

Tuple patterns in let bindings

> infer("int")

```rust
{
  let (x, y) = (1, "a")
  x
}
```

Tuple patterns in match arms

> infer("string")

```rust
{
  match (2, "b") {
    (_, y) => y,
    _ => "c",
  }
}
```

Parse unit type

> infer("fn (()) -> ()")

```rust
{
  fn foo(x: ()) { () }
}
```

Missing methods give the target type.

> errorContains("Foo<int> has no field or method: bar")

```rust
{
  struct Foo<T> { x: T }
  let f = Foo{ x: 34 }
  f.bar()
}
```

Pattern matching on structs

> infer("bool")

```rust
{
  struct Foo { x: int, y: string }

  let f = Foo{ x: 34, y: "yo" }
  match f {
    Foo { x, y: "bar" } => false,
    Foo { x: 34, y: "bar" } => false,
    Foo { x, y } => true,
  }
}
```

Not equal operator

> infer("bool")

```rust
{
  1 != 2
}
```

Tuple fields II

> infer("[string]")

```rust
{
  struct Foo<T> { x: int, y: [T] }
  let a = Foo { x: 1, y: ["yo"] }

  @ensure a.x, int
  a.y
}
```

Struct patterns on generic structs

> infer("[string]")

```rust
{
  struct Foo<T> { x: int, y: [T] }
  let a = Foo { x: 1, y: ["yo"] }

  match a {
    Foo { x: 1, y } => y,
    _ => ["asdf"],
  }
}
```

Unit pattern match arm

> infer("int")

```rust
match () {
  () => 1,
}
```

No method found error

> errorContains(".bar")

```rust
{
  struct Foo {}
  let f = Foo {}

  f.bar()
}
```

Mutable variables

> infer("int")

```rust
{
  let mut x = 0
  x = x + 1
  x
}
```

Only mutable variables can be mutated

> errorContains("y is not declared as mutable")

```rust
{
  let y = 0
  y = 3
}
```

Functions can take mutable params

> infer("int")

```rust
{
  fn foo (a: int) -> int {
    a = a + 1
    a
  }

  foo(1)
}
```

If statements skip unification

> infer("int")

```rust
{
  let n = 0

  if true {
    n + 1
  }

  if false {
  }

  1
}
```

Reference types

> infer("int")

```rust
{
  struct Bar { x: int }

  fn foo(a: *Bar) -> int {
    a.x
  }

  foo(&Bar{ x: 1 })
}
```

Slice syntax in types

> infer("fn ([int]) -> ()")

```rust
{
  fn foo(x: [int]) {}
}
```

Result type must have one type param.

> errorContains("Wrong arity")

```rust
{
    fn foo() -> Result {
      Ok(1)
    }
}
```

Statements can't be returned

> errorContains("Type mismatch")

```rust
{
    fn foo() -> Result<(), string> {
        for i in [] {}
    }
}
```

Explicit return value when function returns Result<()>

> errorContains("Type mismatch")

```rust
fn foo() -> Result<(), string> {
    let a = 1
}
```
