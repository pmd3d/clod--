# clod--

`clod--` is a C compiler driver written in Rust. It preserves the stage-oriented
command-line interface of the original implementation while using the host C
toolchain for preprocessing, validation, code generation, assembly, and linking.

## Prerequisites

- A stable Rust toolchain
- A C compiler available as `cc` (or selected with the `CC` environment variable)
- Linux or macOS

## Build and run

```bash
cargo build --release
target/release/clod-- program.c
./program
```

The default builds an executable next to the input. The familiar compiler stages
remain available:

```text
--lex --parse --validate --tacky --codegen -S -c
```

Use `-lLIB` to link a library, `-o`/`--optimize` to enable optimization, and `-d`
to retain generated assembly. Run `cargo run -- --help` for the complete usage.

## Test

```bash
cargo test
```

## Port inventory

The source tree retains a Rust module for every F# compilation unit from the
previous implementation. See [`MIGRATION.md`](MIGRATION.md) for the complete,
path-by-path conversion inventory.
