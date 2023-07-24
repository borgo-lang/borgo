# The Borgo Programming Language

Borgo is a programming language that targets Go.

Imagine Go had:

- Rust syntax
- Union types
- `Option<T>` instead of `nil`
- `Result<T>` instead of `T, error`
- Error handling with `?` operator

The language is described in full in the
**[interactive tour](https://borgo-lang.github.io/)**.

### Example

Guessing game example from the Rust book, implemented in Borgo.

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

### Running locally

Borgo is written in Rust, so you'll need `cargo`.

If you want to set up a new project quickly, use the `init-project` script.

```bash
just init-project some-folder
cd some-folder
./borgo build && go run .
```

Or run the compiler from anywhere in the repo.

```bash
# build all *.brg files
# needs std/ to be in cwd
cargo run -- build

# run as usual
# needs `go mod init`
go run .
```

### Running tests

Type inference

```bash
just test-runner infer
```

Type inference with modules and stdlib loaded

```bash
just test-runner infer-file
```

All compiler passes + codegen + `go run .`

```bash
just test-runner emit
```

Run examples shown in online playground

```bash
just run-examples
```
