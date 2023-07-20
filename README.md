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
