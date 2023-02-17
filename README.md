# The Borgo Programming Language

**Borgo** is a Rusty functional language that compiles to Go.

The Borgo compiler is written in Rust. Check out the
**[online playground](https://borgo-lang.github.io/playground/)** for some
examples.

- [Goals](#goals)
- [Language Tour](#tour)
- [Stdlib](#stdlib)
- [Usage](#usage)
- [Tests](#running-tests)

## Goals

**Simple but powerful type system** - Good type inference like Elm, Haskell,
Rust.

**Garbage collected** - Optimize for developer experience over extreme
performance. Leverage Go's garbage collector.

**Functional** - Algebraic data types, exhaustive pattern matching, immutable
values and persistent collections. No loops, no imperative constructs.

**`Result<T, E>` error handling and `?` operator** - Just like in Rust.

**Uniform Function Call Syntax** - All functions can be used as methods, by
dispatching on the type of the first argument.

## Tour

Defining types and functions is similar to Rust.

```rust
enum User {
    Verified(String),
    NotVerified,
}

fn is_verified(u: User) -> Bool {
    match u {
        User::Verified(_) => true,
        User::NotVerified => false,
    }
}

struct Point {
    x: Int,
    y: Float,
}

let alice = User::Verified("alice");
let a_point = Point { x: 1, y: 5.2 };

Debug::assert_eq(is_verified(alice), true)
```

All functions can be invoked as methods.

```rust
alice.is_verified()
```

Which means you can rewrite the original function call as a method chain.

```rust
Debug::assert_eq(is_verified(alice), true)

// same as

alice
    .is_verified()
    .assert_eq(true)
```

`impl` blocks are just syntactic sugar for defining functions.

```rust
impl User {
    fn get_name(self) -> Option<String> {
        match u {
            User::Verified(name) => Some(name),
            User::NotVerified => None,
        }
    }
}

User::get_name(alice)

// same as

alice.get_name()
```

Lists are first class, so there's no need for a constructor like `vec![]`.

```rust
let numbers = [1, 2, 3];
```

Maps and Sets are also available.

```rust
let users = Map::new()
    .insert("alice", alice)
    .insert("bob", User::NotVerified);

let results = Set::new()
    .insert(1)
    .insert(2)
    .insert(1);
```

Collections (like all other values) are immutable.

```rust
let new_numbers = numbers.insert(4);

// Original is not modified
numbers.assert_eq([1,2,3])

new_numbers.assert_eq([1,2,3,4])
```

Collections can be iterated by turning them into lazy sequences (`Seq<T>`).

```rust
numbers
    .seq()           // turn List<T> into Seq<T>
    .map(|n| n + 5)
    .to_list()       // turn Seq<T> back into List<T>
    .assert_eq([6,7,8])
```

> Sequences are similar to Rust's `Iterator`, but they're not implemented as a
> trait.
>
> Note also how there's no polymorphic `.collect()` method. Instead, functions
> like `to_list`, `to_map` etc. turn a `Seq<T>` back into a concrete collection.

Sequences are lazy, meaning items are not evaluated until needed.

This allows creating (potentially infinite) ranges.

```rust
Seq::infinite(0, |n| n + 1)
    .drop(50)
    .take(200)
    .reduce(0, |acc, n| if n % 2 == 0 { acc + n } else { acc })
```

There is no `null` value, use `Option<T>` instead. Error handling works much
like Rust, with `Result<T>` and the `?` operator.

```rust
enum Error {
    InvalidName(String),
    InvalidAge(Int),
}

struct Person {
    name: String,
    age: Int,
}

impl Person {
    fn validate_name(s: String) -> Result<String, Error> {
        if s.len() < 0 || s.len() > 100 {
            return Err(Error::InvalidName(s));
        }

        Ok(s)
    }

    fn validate_age(n: Int) -> Result<Int, Error> {
        if n % 2 == 0 {
            return Err(Error::InvalidAge(n));
        }

        Ok(n)
    }
}

fn test() -> Result<Person, Error> {
    let name = Person::validate_name("bob")?;
    let age = Person::validate_age(99)?;
    Ok(Person { name, age })
}

fn borgo_main() {
    let p = test().unwrap();
    p.name.assert_eq("bob");
    ()
}
```

Tuples are supported, with `.N` syntax to access the nth element.

```rust
let a = ("foo", 1);
let b = ("bar", 2);

(a.1 + b.1).assert_eq(3)
```

Mutability can still be achieved with `Ref<T>` (a mutable reference to a value
of type `T`).

```rust
fn add_to_list(xs: Ref<List<Int>>, n: Int) {
    if n == 0 {
        return;
    }

    // Mutate the list
    xs.mutate(|prev| prev.push(n));

    add_to_list(xs, n - 1)
}

fn borgo_main() {
    // Create a Ref
    let xs = [].to_ref();

    // Add 10 elements to the list
    add_to_list(xs, 10);

    // Get the inner value inside the Ref
    let inner = xs.get();

    inner.assert_eq([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
}
```

## Stdlib

Primitive types

| **Borgo** | **Go**                                                     |
| --------- | ---------------------------------------------------------- |
| `Int`     | `int64`                                                    |
| `Float`   | `float64`                                                  |
| `Bool`    | `bool`                                                     |
| `String`  | `string`                                                   |
| `Char`    | `rune`                                                     |
| `List<T>` | [immutable.List](https://github.com/benbjohnson/immutable) |

The stdlib can be found at
[runtime/std/core.brg](https://github.com/borgo-lang/borgo/blob/main/runtime/std/core.brg).

## Usage

Requirements:

- Rust toolchain
- Go toolchain
- Deno (if you want to run tests)

Run this script to build the compiler and set up a fresh project:

```bash
$ ./init-project PROJECT_NAME

$ ls PROJECT_NAME
app.brg   # write source code here
borgo     # compiler
runtime   # Go runtime
std       # stdlib
```

Compile and run:

```bash
$ ./borgo build && go run .
"Hello world"
```

## Running tests

```bash
$ deno task test-runner [TEST_SUITE]
```

Where `TEST_SUITE` is one of:

- `infer` - run type checking on expressions
- `infer-file` - run type checking on files
- `emit` - compile one or more files to Go
- `eval` - just like `emit` but will use a tree-walking interpreter
- `examples` - run all the examples in the playground

Omitting `TEST_SUITE` will run all tests.
