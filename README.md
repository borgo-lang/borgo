# The Borgo Programming Language

Borgo is a statically typed programming language that looks like Rust and
compiles to Go.

**Check out the [online playground](https://borgo-lang.github.io/playground/)
for an interactive tour of the language**.

From Rust it _borrows_:

- Sum types with exhaustive pattern matching
- `Result<T, E>` and `Option<T>` instead of `nil`
- Type checker with good type inference
- Error handling with `?` operator

And leverages Go for:

- Garbage collector
- Robust runtime with concurrency built-in
- Large package ecosystem with mature libraries

You can think of Borgo as Rust without the borrow checker if you like, with
access to all Go packages.

### Running locally

Assuming you have `cargo` and `just` installed:

```bash
just build
just init-project some-folder

cd some-folder
./borgo build && go run .
```
