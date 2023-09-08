# Code generation for files

Process files and emit Go code

---

Standalone expressions

```rust
use fmt

fn main() {
    fmt.Printf("hello %v", 4)
}
```

Function calls

```rust
fn foo(b: bool, x: int) -> bool { b }
fn bar(x: int) -> int { x }
fn baz() {}

fn main() {
    let val = foo(match 1 {
        1 => true
        _ => false
    }, bar(5))

    assertEq(val, true)
}
```

Let bindings

```rust
fn main() {
    let a = 5 + 5
    assertEq(a, 10)
}
```

If statements

```rust
fn main() {
    let x = if true { 6 } else { 0 }
    assertEq(x, 6)
}
```

Enums

```rust
enum Foo {
    Bar(int, bool),
    Baz,
}

fn main() {
    inspect(Foo.Bar(2, false))
    inspect(Foo.Baz)
}
```

Structs

```rust
struct Bar<T> {
    name: string,
    age: int,
    v: T,
}

fn main() {
    let x = Bar { name: "yo", age: 99, v: false }
    inspect(x)
}
```

Return statement

```rust
fn foo() -> int {
    let x = match 1 {
        1 => return 12
        _ => 5
    }

    if true {
        let _ = x + 40
    } else {
        return 9
    }

    return 4
}

fn bar() {
    if false {
        return
    }
}

fn main() {
    assertEq(foo(), 12)
}
```

Struct access

```rust
struct Foo {
    a: int,
    b: string,
    c: bool,
}

fn main() {
    let x = Foo { a: 1, b: "hi", c: true }
    assertEq(x.a, 1)
    let y = Foo { a: 5, c: false, ..x }
    assertEq(y.a, 5)
    assertEq(x.a, 1)
}
```

Try operator

```rust
use errors

fn foo(b: bool) -> Result<int> { bar("a") }

fn bar(s: string) -> Result<int> {
    Err(errors.New("boom"))
}

fn baz() -> Result<int> {
    let _ = foo(false)?

    @unreachable()
    Ok(1)
}

fn as_value() -> Result<int> {
    let a = baz()
    a
}

fn as_param(r: Result<int>) -> bool {
    r.IsOk()
}

fn main() {
    assertEq(baz(), Err(errors.New("boom")))
    assertEq(as_value().IsOk(), false)
    assertEq(as_param(Ok(1)), true)
}
```

Destructure function params

```rust
fn foo((_, b): (int, string)) -> string {
    b
}

fn main() {
    assertEq(foo((1, "yo")), "yo")
}
```

Lists

```rust
fn main() {
    let mut x = [1, 2, 5 + 5]

    inspect(x)
    assertEq(x.Len(), 3)
    assertEq(x[1], 2)

    x[1] = 8
    assertEq(x[1], 8)

    x = x.Append(9)
    assertEq(x[3], 9)
}
```

Impl blocks

```rust
struct Foo { a: int }

impl (f: Foo) {
    fn bar(x: int) -> int {
        f.a + x
    }
}

fn main() {
    let f = Foo { a: 1 }
    assertEq(f.bar(5), 6)
}
```

Rebind variables

```rust
fn main() {
    let x = 1
    assertEq(x, 1)

    let x = 1 + 1
    assertEq(x, 2)

    {
        let x = x + 5
        assertEq(x, 7)
    }

    assertEq(x, 2)
}
```

Concurrency

```rust
use sync
use fmt

fn main() {
    let (sender, receiver) = Channel.new()

    fn foo(x: int) {
        sender.Send(x)
    }

    spawn (|| { sender.Send(5) })()

    let val = receiver.Recv()
    assertEq(val, 5)

    spawn foo(10)

    let val = receiver.Recv()
    assertEq(val, 10)

    {
        let desired = 5

        let wg: sync.WaitGroup = zeroValue()
        wg.Add(desired)

        let (done_tx, done_rx) = Channel.new()

        // receiver goroutine
        spawn (|| {
            let mut count = 0

            for n in receiver {
                count = count + n
            }

            assertEq(count, 10)
            fmt.Printf("count: %v", count)

            done_tx.Send(())
        })()

        let mut i = 0

        // start `desired` goroutines
        while (i < desired) {
            spawn (|i| {
                sender.Send(i)
                wg.Done()
            })(i)

          i = i + 1
        }

        wg.Wait()
        sender.Close() // close(sender)
        done_rx.Recv() // <-done
    }
}
```

Math with floats

```rust
fn main() {
    let x = 5.3 * 1.2
    assertEq(true, x > 6.35 && x <= 6.36)
}
```

Early returns in blocks.

```rust
fn foo() -> int {
    {
        return 5
    }

    999
}

fn main() {
    assertEq(foo(), 5)
}
```

Recursive functions are supported in files.

```rust
file: main.brg

fn foo(n: int) -> int {
    if n != 5 {
        return foo(n + 1)
    }

    n
}

fn main() {
    assertEq(foo(1), 5)
}
```

Mutually recursive functions

```rust
file: main.brg

fn abs(n: int) -> int {
    if n < 0 { return -n }
    n
}

fn even(n: int) -> bool {
    if n == 0 {
        return true
    }

    odd(abs(n) - 1)
}

fn odd(n: int) -> bool {
    if n == 0 {
        return false
    }

    even(abs(n) - 1)
}

fn main() {
    assertEq(even(10), true)
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
    A(int),
}

fn with_foo(f: Foo, m: int) -> int {
    match f {
        Foo.X(b) => match b {
            Bar.A(n) => n + m
        }
    }
}

file: main.brg
fn main() {
    let bar = Bar.A(2)
    assertEq(with_foo(Foo.X(bar), 3), 5)
}
```

Recursion across files

```rust
file: a.brg
fn a(n: int) -> int {
    if n == 100 {
        return n
    }

    b(n + 10)
}

file: b.brg
fn b(n: int) -> int {
    if n == 200 {
        return n
    }

    a(n + 20)
}

file: main.brg
fn main() { assertEq(a(40), 100) }
```

Match on structs

```rust
struct Foo { a: int }

fn main() {
    let x = Foo { a: 1 }
    let res = match x {
        Foo { a: 2 } => false
        Foo { a: 1 } => true
        Foo { a: _ } => false
    }

    assertEq(res, true)
}
```

Const expressions are global

```rust
file: main.brg

const a: int = 1

fn main() { assertEq(a + 5, 6) }
```

Const expressions are visible from other files

```rust
file: foo.brg

fn check() -> bool {
    foo == 2
}

file: main.brg

const foo: int = 1 + 1
fn main() { assertEq(check(), true) }
```

Paren expressions

```rust
fn main() {
    assertEq((1 + 4), 5)
}
```

Recursive types

```rust
enum Expr {
    Add(*Expr, *Expr),
    Number(int),
}

impl (e: Expr) {
    fn sum() -> int {
        match e {
            Add(a, b) => a.sum() + b.sum()
            Number(n) => n
        }
    }
}

struct Foo {
    n: string,
    f: *Option<Foo>,
}

fn main() {
    let one = Expr.Number(1)
    let two = Expr.Number(2)
    let e = Expr.Add(&one, &two)
    assertEq(e.sum(), 3)

    let f1 = None
    let nope = Foo { n: "a", f: &f1 }
    let f2 = Some(nope)
    let yep = Foo { n: "b", f: &f2 }
    assertEq(yep.n, "b")
}
```

Recursive functions should work even when not declared at the top-level.

```rust
fn main() {
    fn foo(n: int, acc: int) -> int {
        if n == 0 {
            return acc
        }

        let new_acc = if n % 2 == 0 {
            acc + n
        } else {
            acc
        }

        foo(n - 1, new_acc)
    }

    assertEq(foo(10, 0), 30)
}
```

Exhaustiveness checking on bools

```rust
fn main() {
    let x = match false {
        true => @unreachable()
        false => 2
    }
    assertEq(x, 2)
}
```

Primitive types are casted in struct call

```rust
struct Foo {
    bar: int,
}

fn main() {
    let x = 1
    let y = Foo { bar: x }
    assertEq(y.bar, 1)
}
```

Match on tuples

```rust
fn main() {
    let res = match (1, "foo") {
        (3, _) => 5
        (1, "bar") => 6
        (x, "foo") => x
        _ => @unreachable()
    }

    assertEq(res, 1)

    let res = match () {
        () => 2
    }

    assertEq(res, 2)
}
```

Enums in tuples

```rust
enum Foo { Bar, Baz }

fn main() {
    let res = match (Bar, Baz) {
        (Bar, Bar) => 0
        (Bar, Baz) => 2
        _ => @unreachable()
    }

    assertEq(res, 2)
}
```

Let binding same name as function param

```rust
fn foo(xs: [int]) -> int {
    // TODO asdf make sure params are put in scope so they can be rebound
    // let xs = xs.Len()
    // xs + 10

    let xxxs = xs.Len()
    xxxs + 10
}

fn main() {
    assertEq(foo([1,2,3]), 13)
}
```

Maps in structs

```rust
struct Foo {
    bar: Map<string, int>
}

fn main() {
    let mut bar = Map.new()
    let foo = Foo { bar }
    assertEq(foo.bar.Len(), 0)

    bar.Insert("yo", 1)
    assertEq(foo.bar.Len(), 1)

    assertEq(bar.Get("yo"), Some(1))
    assertEq(bar.Get("nope"), None)
    assertEq(bar["yo"], 1)

    bar["yo"] = 3
    assertEq(bar["yo"], 3)
}
```

Functions in structs

```rust
struct Foo {
    bar: fn (a: int) -> int,
}

fn main() {
    let foo = Foo { bar: |x: int| x + 2 }
    assertEq(foo.bar(1), 3)
}
```

Records have stable field order.

```rust
struct Foo { x: int, y: string }

fn main() {
    inspect((1, "a", true))
    inspect(Foo { x: 1, y: "b" })
}
```

Using for loops.

```rust
fn main() {
    {
        let mut sum = 0

        // this should iterate over values
        for x in [1, 2, 3] {
            sum = sum + x
        }

        assertEq(sum, 6)
    }

    {
        let mut sum = 0

        for (i, x) in [1, 2, 3].Enumerate() {
            sum = sum + i + x
        }

        assertEq(sum, 9)
    }

    {
        let str = "asdf"
        let mut check = ""

        for c in str {
            check = check + string(c)
        }

        assertEq(str, check)
        let mut check = ""

        for (index, c) in str.Enumerate() {
            inspect(index)
            check = check + string(c)
        }

        assertEq(str, check)
    }

    let mut n = 20

    match true {
        true => {
            n = 25
        }
        false => ()
    }

    loop {
        if n > 27 {
            break
        }
        inspect(n)
        n = n + 1
    }

    n = 0
    while n < 10 {
        n = n + 1
    }
    inspect(n)

    let m = Map.new()
    m.Insert("a", 1)

    for (k, v) in m {
        inspect(k)
        inspect(v)
    }
}
```

Control flow in loops

```rust
fn main() {
    let mut n = 0
    let mut check = false

    loop {
        if n <= 5 {
            n = n + 1
            assertEq(check, false)
            continue
        }

        check = true
        break
    }

    assertEq(check, true)
    assertEq(n, 6)

    n = 0

    for x in [1,2,3] {
        if x == 2 {
            continue
        }
        n = n + 1
    }

    assertEq(n, 2)
}
```

Mutating vars

```rust
fn foo(a: int) -> int {
    loop {
        if a > 5 { break }
        a = a + 1
    }

    a
}

fn main() {
    let mut x = 1
    x = x + 3
    assertEq(x, 4)

    {
        let x = 5
        assertEq(x, 5)
    }

    assertEq(foo(0), 6)

    // TODO this doesn't type check :/
    // x = x + 6
    // x.assertEq(10)
}
```

Nested if and match maintain context.

```rust
fn foo() -> int {
    if 1 > 2 {
        1
    } else if 2 > 3  {
        2
    } else {
        3
    }
}

fn bar() -> int {
    match 1 {
        1 => match 2 {
            3 => 4
            _ => 5
        }
        _ => 9
    }
}

fn main() {
    assertEq(foo(), 3)
    assertEq(bar(), 5)
}
```

Lambda signature

```rust
fn compute(f: fn(a: int, b: int) -> int) -> int {
	f(3, 4)
}

fn main() {
    assertEq(compute(|a, b| a + b), 7)
}
```

Read file

```rust
use os
use fmt

fn read() -> Result<()> {
    let f = os.ReadFile("go.mod")?
    fmt.Println(string(f))
    Ok(())
}

fn main() {
    read().Unwrap()
}
```

Native call to result

```rust
use os
use fmt

fn main() {
    match os.ReadFile("go.mod") {
        Ok(s) => fmt.Println(string(s))
        Err(_) => @unreachable()
    }
}
```

Compile references

```rust
use fmt

struct Foo {
    x: int,
}

fn bar(f: *Foo) -> int {
    f.x + 2
}

fn baz(i: *int) {
    i.* = 99
}

fn main() {
    fmt.Printf("%v", bar(&Foo { x: 1 }))

    let mut n = 1
    baz(&n)
    assertEq(n, 99)
}
```

References in structs

```rust
use fmt

struct Foo {
    x: int,
}

struct Bar {
    f: *Foo,
}

fn update(b: *Bar) {
    b.f.x = 99
}

fn main() {
    let f = Foo { x: 1 }
    let mut b = Bar { f: &f }
    update(&b)
    assertEq(b.f.x, 99)
    fmt.Printf("%+v", b.f)
}
```

References in methods

```rust
struct Foo {
    x: int,
}

impl (f: *Foo) {
    fn update(x: int) {
        f.x = x
    }
}

fn main() {
    let f = Foo { x: 1 }
    f.update(5)
    assertEq(f.x, 5)
}
```

Interfaces

```rust
interface Foo {
    fn foo() -> int
}

struct Bar { x: int }

impl (b: Bar) {
    fn foo() -> int {
        b.x
    }
}

fn baz(f: Foo) -> int {
    f.foo() + 4
}

interface Composite {
    impl Foo
}

fn check_composite(c: Composite) {
    c.foo()
}

fn main() {
    assertEq(baz(&Bar { x: 6 }), 10)
}
```

Types implement interfaces

```rust
use fmt

struct Foo {}

impl (f: Foo) {
    fn Write(bytes: [byte]) -> Result<int> {
        Ok(3)
    }
}

fn main() {
    let n = fmt.Fprintf(&Foo{}, "%d", 1).Unwrap()
    assertEq(n, 3)
}
```

Net http

```rust
use fmt
use net.http
use net.http.httptest
use io
use sync

struct Counter { m: sync.Mutex, count: int }

impl (c: *Counter) {
    fn ServeHTTP(w: http.ResponseWriter, r: *http.Request) {
        c.m.Lock()
        c.count = c.count + 1
        fmt.Fprintf(w, "<h1>count %d</h1>", c.count)
        c.m.Unlock()
    }
}

fn main() {
    let c = Counter { m: zeroValue(), count: 0 }

    let ts = httptest.NewServer(&c)
    defer ts.Close()

    let res = http.Get(ts.URL).Unwrap()
    let body = io.ReadAll(res.Body).Unwrap()
    res.Body.Close()

    fmt.Println(string(body))
}
```

Trait bounds without methods are not checked

```rust
// Let the Go compiler figure out if a type satisfies a constraint
fn foo<T: comparable>(x: T, y: T) -> bool {
    x == y
}

interface Foo {
    fn check() -> int
}

struct Bar {}
impl (b: Bar) {
    fn check() -> int { 3 }
}

// Check that multiple constraints are correctly emitted
fn bar<T: comparable + Foo>(x: T, y: T) -> int {
    if x == y {
        x.check()
    } else {
        -1
    }
}

fn main() {
    assertEq(foo(1.0, 2.0), false)
    assertEq(foo(1, 1), true)
    assertEq(bar(Bar{}, Bar{}), 3)
}
```

Variadic function calls

```rust
use fmt

fn foo(a: bool, x: VarArgs<int>) {}

fn main() {
    fmt.Printf("yep %s, %d", "hi", 3)
    foo(false, 1, 2)
}
```

Package structs

```rust
use os

fn main() {
    let _ = os.Process { Pid: 99 }
}
```

Options

```rust
use os

fn foo() -> Option<int> {
    Some(3)
}

fn main() {
    let x = foo()
    assertEq(x.IsSome(), true)

    match os.LookupEnv("HOME") {
        Some(_) => ()
        None => @unreachable()
    }
}
```

Errors as custom types

```rust
fn foo() -> Result<(), FooErr> {
    Ok(())
}

struct FooErr {}

impl (f: FooErr) {
    fn Error() -> string { "b" }
}

fn bar() -> Result<(), error> {
    let x = foo()?
    Ok(x)
}

fn main() {
    let x = foo()
    let y = bar()
    assertEq(x, Ok(()))
    assertEq(y, Ok(()))
}
```

Blocks used as expressions

```rust
use math

fn main() {
    let block_result = {
        let a = math.Pi
        let b = 2.01
        a + b
    }
    assertEq(block_result > 4.0, true)
}
```

Defer statements

```rust
use fmt

fn main() {
    fmt.Println("first")
    defer fmt.Println("defer 1")

    fmt.Println("second")
    defer (|| { fmt.Println("defer 2") })()
}
```

Newline characters

```rust
use fmt

fn main() {
    let x = '\n'
    fmt.Printf("a%sb\n", x)

    let y = \\a string
        \\ with " quotes and \ backslashes
    fmt.Println(y)
}
```

Referenced packages are imported

```rust
use fmt
use os

fn main() {
    let dir = os.ReadDir(".").Unwrap()
    fmt.Printf("%v", dir[0])
}
```

Unwrapped call in loop

```rust
use fmt
use os

fn foo() -> Result<()> {
    for f in os.ReadDir(".")? {
        if f.Name() == "go.mod" {
            fmt.Println("ok")
        }
    }

    Ok(())
}

fn main() {
    foo().Unwrap()
}
```

Math with newtypes

```rust
use fmt
use time

fn main() {
    time.Sleep(0 * time.Second)
    fmt.Println(2 * time.Second)
}
```

Select statement

```rust
use fmt

fn main() {
    let (tx, rx) = Channel.new()

    spawn (|| {
        tx.Send(1)
    })()

    select {
        let foo = rx.Recv() => {
            fmt.Println("foo", foo)
        }
    }

    spawn (|| {
        let bar = rx.Recv()
        fmt.Println("bar", bar)
    })()

    select {
        tx.Send(5) => fmt.Println("sending")
    }

    select {
        _ => fmt.Println("default")
    }
}
```
