# The Borgo Programming Language

Borgo is a high-level, garbage collected language for application development.

It transpiles to Go and aims to be fully compatible with existing Go packages.

> Borgo syntax _currently_ looks like Rust.
>
> That's only to speed up development and skip writing a parser for a custom
> syntax. Borgo has no lifetimes nor borrow checker – it just _borrows_ Rust
> syntax for convenience.

**Features:**

- Union types and pattern matching
- Error handling with `?` operator
- `null` safe, use `Option` and `Result`
- Compatible with Go packages
  - functions that return `T,bool` become `Option<T>`
  - functions that return `T,error` become `Result<T, error>`

## Tutorial

Check out the **[online playground](https://borgo-lang.github.io/)** for a tour
of the language.

## Example

Guessing game example from the Rust book, implemented in Borgo.

Things to note:

- import packages from Go stdlib
- `strconv.Atoi` returns an `Option<int>`
- `Reader.ReadString` returns a `Result<string, error>` (which can be unwrapped)

```rust
use bufio;
use fmt;
use math::rand;
use os;
use strconv;
use strings;
use time;

fn main() {
    let reader = bufio.NewReader(os.Stdin);

    rand.Seed(time.Now().UnixNano());
    let secret = rand.Intn(100) + 1;

    loop {
        fmt.Println("Please input your guess.");

        let text = reader.ReadString('\n').unwrap();
        let text = strings.TrimSpace(text);

        let guess = match strconv.Atoi(text) {
            Ok(n) => n,
            Err(_) => continue,
        };

        fmt.Println("You guessed: ", guess);

        if guess < secret {
            fmt.Println("Too small!");
        } else if guess > secret {
            fmt.Println("Too big!");
        } else {
            fmt.Println("Correct!");
            break;
        }
    }
}
```

## Running locally

Borgo is written in Rust, so you'll need `cargo`.

If you want to set up a new project quickly, use the `init-project` script.

```bash
$ just init-project some-folder
$ cd some-folder
$ ./borgo build && go run .
```

Or run the compiler from anywhere in the repo.

```bash
# build all *.brg files
# needs std/ to be in cwd
$ cargo run -- build

# run as usual
# needs `go mod init`
$ go run .
```
