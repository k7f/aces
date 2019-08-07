aces
====
[![Latest version](https://img.shields.io/crates/v/aces.svg)](https://crates.io/crates/aces)
[![docs](https://docs.rs/aces/badge.svg)](https://docs.rs/aces)
![Rust](https://img.shields.io/badge/rust-nightly-brightgreen.svg)
![MIT](https://img.shields.io/badge/license-MIT-blue.svg)

Algebra of [Cause-Effect
Structures](https://link.springer.com/book/10.1007/978-3-030-20461-7)
&mdash; an implementation of the theory.  This is the core library and
command-line tools of the [_Ascesis_](https://github.com/k7f/ascesis)
project.

## Prerequisites

In principle, `aces` should build wherever `rustc` and `cargo` runs.
Its executables should run on any
[platform](https://forge.rust-lang.org/platform-support.html)
supporting the Rust `std` library.  Be aware, though, that the project
is very much a WIP.  The main toolchain used in development is nightly
channel of Rust 1.38.

## Library

To use `aces` as a library in a Rust project, include these lines in
the `Cargo.toml` file:

```toml
[dependencies]
aces = "0.0.3"
```

See [API documentation](https://docs.rs/aces) for information on
public interface to the library.

## Command line interface

### Installation

Having [Rust](https://www.rust-lang.org/downloads.html) installed,
ensure its version is at least 1.36: check with `cargo version` and
run `rustup update` if needed.  Then

```bash
$ cargo install aces
```

will automatically download, build, and install the latest `aces`
release on [crates.io](https://crates.io/crates/aces).

### Features

C-e structures may be defined in `.cex` text files.  The format of
textual description is YAML-based, but nowhere documented and very
likely to change.  There are some, perhaps self-explanatory,
[examples](data/).

Run the `aces` executable to load c-e structures from `.cex` files and
analyse them.  By default, the program will check link coherence and
print firing components, if there are any, or inform about structural
deadlock.  To see the list of available subcommands and options run

```bash
$ aces --help
```

## License

`aces` is licensed under the MIT license.  Please read the
[LICENSE-MIT](LICENSE-MIT) file in this repository for more
information.
