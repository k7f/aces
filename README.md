aces
====
[![Latest version](https://img.shields.io/crates/v/aces.svg)](https://crates.io/crates/aces)
[![docs](https://docs.rs/aces/badge.svg)](https://docs.rs/aces)
![Rust](https://img.shields.io/badge/rust-nightly-brightgreen.svg)
![MIT](https://img.shields.io/badge/license-MIT-blue.svg)

[Algebra of Cause-Effect
Structures](https://link.springer.com/book/10.1007/978-3-030-20461-7)
&mdash; an implementation of the theory.  This is the core library of
the [_Ascesis_](https://github.com/k7f/ascesis) project.

## Installation

In principle, `aces` should build wherever `rustc` and `cargo` runs.
Be aware, though, that the project is very much a WIP.  The main
toolchain used in development is nightly channel of Rust 1.46.

To use `aces` as a library in a Rust project, include these lines in
the `Cargo.toml` file:

```toml
[dependencies]
aces = "0.0.13"
```

See [API documentation](https://docs.rs/aces) for information on
public interface to the library.

## License

`aces` is licensed under the MIT license.  Please read the
[LICENSE-MIT](LICENSE-MIT) file in this repository for more
information.
