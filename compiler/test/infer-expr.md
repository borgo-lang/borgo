# Type inference for expressions

What's the type of an `ast::Expr`?

---

Literals.

> infer("Int")

```rust
1
```

Blocks get the type of the last expression.

> infer("fn () -> ()")

```rust
{ fn foo() {} }
```

Block ends with function

> infer("fn () -> Int")

```rust
{ fn foo() -> Int { 1 } }
```

Block ends with function 2

> infer("fn (Int) -> Int")

```rust
{ fn foo(foo: Int) -> Int { foo } }
```

Block ends with function 3

> infer("fn (Int, Bool) -> Int")

```rust
{ fn foo(foo: Int, bar: Bool) -> Int { foo } }
```

Block ends with function 4

> infer("fn <A>(A, Bool) -> A")

```rust
{ fn foo<T>(foo: T, bar: Bool) -> T { foo } }
```

Sanity check: types must exist

> errorContains("Type not found: T")

```rust
{ fn foo(x: T) -> T { x } }
```

Block ends with list

> infer("fn () -> List<Int>")

```rust
{ fn foo() -> List<Int> { [1] } }
```

Block ends with generic list

> infer("fn <A>(A) -> List<A>")

```rust
{ fn foo<Y>(foo: Y) -> List<Y> { [foo, foo] } }
```

Generic arguments 1

> errorContains("Wrong arity")

```rust
{ fn foo(foo: List<Int, Bool>) {} }
```

Generic arguments 2

> infer("fn () -> List<List<Int>>")

```rust
{ fn foo() -> List<List<Int>> { [[1]]} }
```

Generic arguments 3

> errorContains("Wrong arity")

```rust
{ fn foo(foo: Int<Bool>) {} }
```

HKTs are not supported.

> errorContains("Wrong arity")

```rust
{ fn foo<T>(foo: T<Bool>) {} }
```

Populate scope with let bindings.

> infer("Int")

```rust
{
  let x = 5;
  x
}
```

Function call with no arguments.

> infer("String")

```rust
{ hello() }
```

Function call with arguments

> infer("Int")

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

List literals

> infer("List<Int>")

```rust
{ [1,2,3] }
```

Lists are polymorphic

> infer("List<String>")

```rust
{
  let x = [1,2,3];
  list_push(x, 4);
  ["hello"]
}
```

It's possible to sanity-check the type of a value.

> infer("List<Int>")

```rust
{
  let x = [1,2,3];
  x as List<Int>
}
```

And the check should work!

> errorContains("mismatch")

```rust
{
  let x = [1,2,3];
  x as Bool;
  1
}
```

Functions should get inferred correctly.

> infer("fn <A, B>(List<A>, fn (A) -> B) -> List<B>")

```rust
list_map
```

Functions can take functions.

> infer("List<Int>")

```rust
{
  let x = [1,2,3];
  list_map(x, inc)
}
```

Generics are applied correctly.

> errorContains("mismatch")

```rust
{
  let x = ["hello"];
  list_map(x, inc)
}
```

Functions can be inlined.

> infer("List<Int>")

```rust
{
  let x = [1,2,3];
  list_map(x, |n| sum(n, 5))
}
```

Closures can be inferred.

> infer("fn (Int) -> Int")

```rust
|n| sum(n, 1)
```

Functions are in scope after declaration.

> infer("Int")

```rust
{
  fn foo(a: Int) -> Int { sum(a, 4) }
  foo(1)
}
```

Generic functions get instantiated at each use site.

> infer("Int")

```rust
{
  fn constant<T, Y>(a: T, b: Y) -> T { a }

  let x = constant("hello", 5);
  x as String;
  let y = sum(constant(1, false), 5);
  y as Int
}
```

If expressions.

> infer("Int")

```rust
if true {
  1
} else {
  2
}
```

Condition must be a Bool.

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

> infer("String")

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

> infer("String")

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

> infer("String")

```rust
match 1 {
  1 => "foo",
  foo => int_to_string(foo),
}
```

Match and If are still expressions.

> infer("Int")

```rust
{
  let x = "foo";

  let a = match x {
    "foo" => 1,
    _ => 99,
  };

  let cond = false;

  let b = if cond {
    0
  } else {
    1
  };

  sum(a, b)
}
```

Unit type.

> infer("()")

```rust
()
```

Tuples.

> infer("(Int, Int)")

```rust
(1, 2)
```

Tuples with wrong type annotation.

> errorContains("mismatch")

```rust
("foo", 2) as (Bool, Int)
```

Tuples with wrong arity.

> errorContains("mismatch")

```rust
(1, 2, 3) as (Int, Int)
```

Tuples as function arguments.

> infer("fn ((Int, Bool)) -> Int")

```rust
{
  fn foo(t: (Int, Bool)) -> Int {
    fst(t)
  }
}
```

Tuples as return type.

> infer("fn () -> (String, Bool)")

```rust
{
  fn foo() -> (String, Bool) {
    ("foo", true)
  }
}
```

Typing callbacks.

> infer("String")

```rust
{
  fn foo( f: fn (i: Int) -> String, x: Int ) -> String {
    f(x)
  }

  foo(|x| int_to_string(x), 1)
}
```

Callbacks with generics

> infer("List<Int>")

```rust
{
  fn another_map<A, B>( f: fn (x: A) -> B, xs: List<A> ) -> List<B> {
    // mimics implementation
    let first = list_head(xs);
    [f(first)]
  }

  let a = another_map(|x| int_to_string(x), [1,2,3]);
  a as List<String>;

  another_map(|x| sum(x, 1), [1,2,3])
}
```

Enums should introduce a new type in the environment.

> infer("Color")

```rust
{
  enum Color { Red }
  Color::Red
}
```

Enums with generics should get instantiated.

> infer("Maybe<Int>")

```rust
{
  enum Maybe<T> { Just(T), None }
  Maybe::Just(1)
}
```

Enum variants get instantiated correctly.

> infer("Maybe<A>")

```rust
{
  enum Maybe<T> { Just(T), None }

  Maybe::Just(1) as Maybe<Int>;
  Maybe::Just("foo") as Maybe<String>;
  Maybe::None as Maybe<Bool>;
  Maybe::Just(Maybe::Just(1)) as Maybe<Maybe<Int>>;

  Maybe::None
}
```

Pattern matching works on enums.

> infer("String")

```rust
{
  enum Color { Red, Blue(String) }

  let x = Color::Blue("foo");
  match x {
    Color::Red => "bar",
    Color::Blue(c) => str_concat(c, "!"),
  }
}
```

Pattern matching constructor arity.

> errorContains("arity")

```rust
{
  enum Color { Red, Blue(String) }

  match Color::Blue("foo") {
    Color::Red => "bar",
    Color::Blue(c, nope) => c,
  }
}
```

Generic enums can be pattern matched.

> infer("Int")

```rust
{
  enum Maybe<T> { Just(T), None }

  let x = Maybe::Just(1);
  match x {
    Maybe::Just(v) => v,
    Maybe::None => 2,
  }
}
```

Nested pattern matching is supported.

> infer("Color")

```rust
{
  enum Maybe<T> { Just(T), None }
  enum Color { Red, Blue(String, Bool) }

  let c = Color::Blue("foo", false);
  let x = Maybe::Just(c);

  let n = match x {
    Maybe::Just(Color::Blue(s, b)) => {
      s as String;
      b as Bool;
      1
    },
    Maybe::None => 2,
  };

  n as Int;

  match x {
    Maybe::Just(x) => x,
    Maybe::None => Color::Red,
  }
}
```

Enums can be returned from functions.

> infer("fn <A>(A) -> Maybe<A>")

```rust
{
  enum Maybe<T> { Just(T), None }

  fn ok<T>(v: T) -> Maybe<T> {
    Maybe::Just(v)
  }

  ok(1) as Maybe<Int>;
  ok("foo") as Maybe<String>;

  ok
}
```

Enums from functions II.

> infer("Int")

```rust
{
  enum Maybe<T> { Just(T), None }

  fn foo(b: Int) -> Maybe<Int> {
    Maybe::None
  }

  foo(1) as Maybe<Int>;

  fn bar(b: Int) -> Maybe<Int> {
    Maybe::Just(b)
  }

  match foo(2) as Maybe<Int> {
    Maybe::Just(x) => x,
    Maybe::None => 3,
  }
}
```

Enums from functions (error).

> errorContains("mismatch")

```rust
{
  enum Maybe<T> { Just(T), None }

  fn foo(b: Int) -> Maybe<String> {
    Maybe::Just(b)
  }
}
```

Defining structs.

> infer("Foo")

```rust
{
  struct Foo { a: Int }

  Foo { a: 1 }
}
```

Generic structs.

> infer("Foo<Int>")

```rust
{
  struct Foo<T> { a: T }

  Foo { a: 1 }
}
```

Generic structs with multiple params.

> infer("Foo<Int, Bool>")

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
  struct Foo { a: Int, b: Int }

  Foo { a: 1 }
}
```

Structs with too many arguments.

> errorContains("field not found")

```rust
{
  struct Foo { a: Int, b: Int }

  Foo { a: 1, b: 2, c: 3 }
}
```

Accessing struct fields.

> infer("Int")

```rust
{
  struct Foo { a: Int }

  let x = Foo { a: 1 };
  x.a
}
```

Error when reassign to wrong type.

> errorContains("mismatch")

```rust
{
  let x = 1;
  x = "foo"
}
```

Error when updating structs.

> errorContains("re-assign instead")

```rust
{
  struct Foo { a: Int }

  let x = Foo { a: 1 };
  x.a = "foo"
}
```

Spreading a struct into another.

> infer("Foo")

```rust
{
  struct Foo { a: Int, b: String }

  let x = Foo { a: 1, b: "foo" };
  Foo { b: "bar", ..x }
}
```

Error when spreading in different structs.

> errorContains("mismatch")

```rust
{
  struct Foo { a: Int, b: String }
  struct Bar { a: Int, b: String }

  let x = Foo { a: 1, b: "foo" };
  let y = Bar { a: 1, b: "bar" };
  Foo { b: "bar", ..y }
}
```

Nested updates.

> infer("Bar")

```rust
{
  struct Foo { a: Int, b: String }
  struct Bar { a: Int, b: String, f: Foo }

  let x = Foo { a: 1, b: "foo" };
  let y = Bar { a: 1, b: "bar", f: x };
  Bar { a: 2, b: "baz", f: Foo { a: 4, ..x } }
}
```

Let bindings with annotation.

> errorContains("mismatch")

```rust
{
  let x: List<Int> = [1,2,3];
  let y: List<Bool> = [1,2,3];
}
```

Methods.

> infer("String")

```rust
{
  1.int_to_string() as String;
  let x = 5;
  x.sum(3) as Int;
  hello().str_concat("foo")
}
```

Wrong arguments in methods.

> errorContains("arity")

```rust
{
  hello().str_concat()
}
```

Return statements should unify.

> infer("fn () -> Int")

```rust
{
  fn foo() -> Int {
    let x = 5;
    return x
  }
}
```

Early return statements should unify with return type.

> errorContains("mismatch")

```rust
{
  fn foo() -> Int {
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
  fn foo() -> Int {
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

> infer("fn () -> Int")

```rust
{
  fn foo() -> Int {

    fn bar() -> String {
      if true {
        return "foo"
      }

      "bar"
    }


    bar() as String;

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
  fn foo() -> Result<()> {
    if true {
      return Err(ErrFoo::A);
    }

    Err(ErrBar::X)
  }
}
```

Function calls with different errors don't unify.

> errorContains("mismatch")

```rust
{
  fn foo1() -> Result<Int, ErrFoo> { Err(ErrFoo::A) }
  fn foo2() -> Result<Int, ErrFoo> { Err(ErrBar::X) }
  fn bar() -> Result<Int> {
    foo1()?;
    foo2()?;
    Ok(1)
  }
}
```

Functions with no error get inferred ok.

> infer("List<Int>")

```rust
{
  list_map([1,2,3], |x| sum(x, 1))
}
```

Callbacks with errors are automatically wrapped into a Result.

> infer("List<Result<Int, ErrFoo>>")

```rust
{
  fn foo(x: Int) -> Result<Int> { return Err(ErrFoo::A); Ok(x) }
  list_map([1,2,3], foo)
}
```

Callbacks with different error types can still be passed to the same function.

> infer("Bool")

```rust
{
  fn foo() -> Result<Int, ErrFoo> { Err(ErrFoo::A) }
  fn bar() -> Result<Int, ErrBar> { Err(ErrBar::X) }

  fn baz(f: fn () -> Result<Int, ErrFoo>, g: fn() -> Result<Int, ErrBar>) {
    ()
  }

  foo as fn () -> Result<Int, ErrFoo>;
  bar as fn () -> Result<Int, ErrBar>;

  baz(foo, bar);
  true
}
```

A function that is already wrapped, doesn't get wrapped again.

> infer("Bool")

```rust
{
  fn foo(x: Int) -> Result<String, ErrFoo> { Err(ErrFoo::A) }

  fn bar(f: fn (x: Int) -> Result<String, ErrFoo>) {
    ()
  }

  bar(foo);
  true
}
```

The `?` operator can only be used in functions that return a Result.

> errorContains("mismatch")

```rust
{
  fn foo() -> Result<Int, ErrFoo> { Ok(1) }
  fn bar() {
    let x = foo()?;
    ()
  }
}
```

Equality

> infer("Bool")

```rust
{
  let a = 1;
  let b = 2;
  a == b
}
```

Can shadow vars in block

> infer("String")

```rust
{
  let a = "mutable";
  {
    let a = "block";
  }
  a
}
```

Math operands on Ints

> infer("Int")

```rust
{ 1 + 5 }
```

Wrong type in operands

> errorContains("mismatch")

```rust
{ 1 + false }
```

Boolean operators

> infer("Bool")

```rust
{ (1 == 2) && (false || true) }
```

Impl blocks

> infer("Bool")

```rust
{
    enum Color { Red, Blue }

    impl Color {
        fn is_red(self) -> Bool {
            self == Color::Red
        }
    }

    Color::Red.is_red()
}
```

Impl blocks with generic types

> infer("Bool")

```rust
{
    enum Foo<T, Y> { Bar(T), Baz(Y) }

    impl<T, Y> Foo<T, Y> {
        fn make_bar(x: T) -> Foo<T, Y> { Foo::Bar(x) }

        fn do_stuff(self, y: Y) -> Y {
            self as Foo<T, Y>;

            match self {
                Foo::Bar(x) => y,
                Foo::Baz(yy) => yy,
            }
        }
    }

    let foo: Foo<Int, Bool> = Foo::make_bar(1);
    foo.do_stuff(true)
}
```

Impl for non-existing types

> errorContains("not found")

```rust
{
    impl Unknown { }
}
```

Impl with wrong generics

> errorContains("Wrong arity")

```rust
{
    enum Foo<T, Y> { Bar }
    impl<T, Y> Foo<T> { }
}
```

Match weirdness

> infer("Bool")

```rust
{
    match false {
        true => print("asdf"),
    };

    match 1 {
        1 => false,
    }
}
```

Match Type::Fun with Type::Con

> errorContains("mismatch")

```rust
{
  fn bar() -> Bool { false }
  fn foo() -> Int {
    bar
  }
}
```

Negative numbers

> infer("Int")

```rust
5 * -3
```

Negate expressions

> infer("Bool")

```rust
!false
```

Neg only works with numbers

> errorContains("Int or Float")

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
  fn foo<T>(x: T) -> Int {
    x as T;
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
  fn foo<T>(x: Y) -> Int {
    1
  }
}
```

Characters

> infer("Char")

```rust
'a'
```

Tuple fields

> infer("Int")

```rust
{
  let a = (1, "a");
  a.0
}
```

Sanity check: can't invent constructors in pattern match

> errorContains("Green not found")

```rust
{
  enum Color { Red, Blue }

  match Color::Red {
    Red => 1,
    Green => 2,
  }
}
```

Struct inference in lambdas

> infer("List<String>")

```rust
{
  struct Foo { a: Int, b: String }

  let f = Foo { a: 1, b: "a" };
  list_map([f], |x| str_concat(x.b, "!"))
}
```

Generics in impl method

> infer("String")

```rust
{
  struct Foo<T> { bar: T }
  impl<T> Foo<T> {
    fn map(self, f: fn (x: T) -> String) -> String {
      f(self.bar)
    }

    fn other(self) -> String {
      self.map(|x| "asdf")
    }
  }

  let foo = Foo { bar: 12 };
  foo.other()
}
```

Prevent generics from getting instantiated to non existing types

> errorContains("Type not found: K")

```rust
{
    enum Foo<T, Y> { Bar(T), Baz(Y) }

    impl<T, Y> Foo<T, Y> {
        fn make_bar(x: T) -> Foo<T, Y> { Foo::Bar(x) }

        fn do_stuff(self, y: Y) -> Y {
            self as Foo<T, K>;
            y
        }
    }
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

  match Foo::Bar(12) {
    Bar(x) => {
      let a = x + 1;
      a.int_to_string()
    },

    Baz(y) => str_concat(y, "doh"),
  }
}
```

Floats

> infer("Float")

```rust
9.12
```

Math ops with floats

> infer("Float")

```rust
1.3 + 5.2
```

Can't mix floats with ints

> errorContains("mismatch")

```rust
3.1 * 4
```

Can't use binary ops with random types

> errorContains("Int or Float")

```rust
"a" + "b"
```

Bidirectional inference for closure params

> infer("Int")

```rust
{
  struct Foo { a: Int }

  fn bar<T, Y>(start: T, f: fn (x: T) -> Y) -> Y {
    f(start)
  }

  bar(Foo { a: 1 }, |x| x.a)
}
```

Struct patterns

> infer("Int")

```rust
{
  struct Foo { x: Int, y: String }

  let f = Foo { x: 1, y: "a" };
  match f {
    Foo { x, y } => x
  }
}
```

Tuple patterns

> infer("Int")

```rust
{
  fn foo((x, y): (Int, String)) -> Int { x }
  foo((1, "a"))
}
```

Pattern with too many arguments

> errorContains("mismatch")

```rust
{
  fn foo((x, y, z): (Bool, Bool)) -> Bool { x }
  foo((false, false))
}
```

Tuple patterns in let bindings

> infer("Int")

```rust
{
  let (x, y) = (1, "a");
  x
}
```

Tuple patterns in match arms

> infer("String")

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

Unknown type in impl with generics

> errorContains("Type not found: T")

```rust
{
  struct Foo<T> {}
  impl Foo<T> {}
}
```

Can use different generic name than declaration.

> infer("Int")

```rust
{
  struct Foo<T> {}

  impl<Y> Foo<Y> {
    fn bar<X>(x: X, y: Y) {}
  }

  2
}
```

Missing methods give the target type.

> errorContains("Foo<Int> has no method: bar")

```rust
{
  struct Foo<T> { x: T }
  let f = Foo{ x: 34 };
  f.bar();
}
```

Pattern matcihng on structs

> infer("Bool")

```rust
{
  struct Foo { x: Int, y: String }

  let f = Foo{ x: 34, y: "yo" };
  match f {
    Foo { x, y: "bar" } => false,
    Foo { x: 34, y: "bar" } => false,
    Foo { x, y } => true,
  }
}
```

Not equal operator

> infer("Bool")

```rust
{
  1 != 2
}
```

Tuple fields II

> infer("List<String>")

```rust
{
  struct Foo<T> { x: Int, y: List<T> }
  let a = Foo { x: 1, y: ["yo"] };

  a.x as Int;
  a.y
}
```

Struct patterns on generic structs

> infer("List<String>")

```rust
{
  struct Foo<T> { x: Int, y: List<T> }
  let a = Foo { x: 1, y: ["yo"] };

  match a {
    Foo { x: 1, y } => y,
    _ => ["asdf"],
  }
}
```

Unit pattern match arm

> infer("Int")

```rust
match () {
  () => 1,
}
```

Type errors in method chains

> errorContains(".foo")

```rust
{
  fn foo(x: Int) -> Int { 1 }
  fn bar(x: String) -> String { x }
  fn baz(x: String) -> String { x }

  let input = "yo";

  input
  .bar()
  .baz()
  .foo()
}
```

No method found error

> errorContains(".bar")

```rust
{
  struct Foo {}
  let f = Foo {};

  f.bar()
}
```

Mutable variables

> infer("Int")

```rust
{
  let mut x = 0;
  x = x + 1;
  x
}
```

Only mutable variables can be mutated

> errorContains("y is not declared as mutable")

```rust
{
  let y = 0;
  y = 3;
}
```

Functions can take mutable params

> infer("Int")

```rust
{
  fn foo (mut a: Int) -> Int {
    a = a + 1;
    a
  }

  foo(1)
}
```

If statements skip unification

> infer("Int")

```rust
{
  let n = 0;

  if true {
    n + 1;
  }

  if false {
  }

  1
}
```
