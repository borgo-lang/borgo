# The Borgo Programming Language

![Borgo sits between Go and Rust](https://raw.githubusercontent.com/borgo-lang/borgo-lang.github.io/main/borgo.jpg)

---

![build](https://github.com/borgo-lang/borgo/actions/workflows/ci.yml/badge.svg)

I want a language for writing applications that is more expressive than Go but
less complex than Rust.

Go is simple and straightforward, but I often wish it offered more type safety.
Rust is very nice to work with (at least for single threaded code) but it's too
broad and complex, sometimes painfully so.

**Borgo is a new language that transpiles to Go**. It's fully compatible with
existing Go packages.

Borgo syntax is similar to Rust, with optional semi-colons.

# Tutorial

Check out the **[online playground](https://borgo-lang.github.io/)** for a tour
of the language.

You can also take a look at test files for working Borgo code:

- [codegen-emit.md](compiler/test/codegen-emit.md)
- [infer-expr.md](compiler/test/infer-expr.md)
- [infer-file.md](compiler/test/infer-file.md)

# Features

## Algebraic data types and pattern matching

```rust
use fmt

enum NetworkState {
    Loading,
    Failed(int),
    Success(string),
}

let msg = match state {
    NetworkState.Loading => "still loading",
    NetworkState.Failed(code) => fmt.Sprintf("Got error code: %d", code),
    NetworkState.Success(res) => res,
}
```

---

## `Option<T>` instead of `nil`

```rust
// import packages from Go stdlib
use fmt
use os

let key = os.LookupEnv("HOME")

match key {
    Some(s) => fmt.Println("home dir:", s),
    None => fmt.Println("Not found in env"),
}
```

---

## `Result<T, E>` instead of multiple return values

```rust
use fmt
use net.http

fn makeRequest() -> Result<int, error> {
    let request = http.Get("http://example.com")

    match request {
        Ok(resp) => Ok(resp.StatusCode),
        Err(err) => Err(fmt.Errorf("failed http request %w", err))
    }
}
```

---

## Error handling with `?` operator

```rust
use fmt
use io
use os

fn copyFile(src: string, dst: string) -> Result<(), error> {
    let stat = os.Stat(src)?

    if !stat.Mode().IsRegular() {
        return Err(fmt.Errorf("%s is not a regular file", src))
    }

    let source = os.Open(src)?
    defer source.Close()

    let destination = os.Create(dst)?
    defer destination.Close()

    // ignore number of bytes copied
    let _ = io.Copy(destination, source)?

    Ok(())
}
```

---

## Guessing game example

Small game from the Rust book, implemented in Borgo.

Things to note:

- import packages from Go stdlib
- `strconv.Atoi` returns an `Option<int>`
- `Reader.ReadString` returns a `Result<string, error>` (which can be unwrapped)

```rust
use bufio
use fmt
use math.rand
use os
use strconv
use strings
use time

fn main() {
    let reader = bufio.NewReader(os.Stdin)

    let secret = rand.Intn(100) + 1

    loop {
        fmt.Println("Please input your guess.")

        let text = reader.ReadString('\n').Unwrap()
        let text = strings.TrimSpace(text)

        let guess = match strconv.Atoi(text) {
            Ok(n) => n,
            Err(_) => continue,
        }

        fmt.Println("You guessed: ", guess)

        if guess < secret {
            fmt.Println("Too small!")
        } else if guess > secret {
            fmt.Println("Too big!")
        } else {
            fmt.Println("Correct!")
            break
        }
    }
}
```

## Running locally

Borgo is written in Rust, so you'll need `cargo`.

To compile all `.brg` files in the current folder:

```bash
$ cargo run -- build
```

The compiler will generate `.go` files, which you can run as normal:

```bash
# generate a go.mod file if needed
# $ go mod init foo
$ go run .
```
