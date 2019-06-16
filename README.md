aces
====
[![Latest version](https://img.shields.io/crates/v/aces.svg)](https://crates.io/crates/aces)
[![docs](https://docs.rs/aces/badge.svg)](https://docs.rs/aces)
![Rust](https://img.shields.io/badge/rust-nightly-brightgreen.svg)
![MIT](https://img.shields.io/badge/license-MIT-blue.svg)

Algebra of [Cause-Effect
Structures](https://link.springer.com/book/10.1007/978-3-030-20461-7).

## Features

Building c-e structures from `.ces` files, checking link coherence...

## Prerequisites

In principle, `aces` should build wherever `rustc` and `cargo` runs.
Its executables should run on any
[platform](https://forge.rust-lang.org/platform-support.html)
supporting Rust `std` library.

Be aware, though, that the project is very much WIP.  Currently, the
main toolchain used in development is nightly channel of Rust 1.37.

## Installation

Having [Rust](https://www.rust-lang.org/downloads.html) installed,
ensure its version is at least 1.37: check with `cargo version` and
run `rustup update` if needed.  Then

```bash
$ cargo install aces
```

will automatically download, build, and install the latest `aces`
release on [crates.io](https://crates.io/crates/aces).

## License

`aces` is licensed under the MIT license.  Please read the
[LICENSE-MIT](LICENSE-MIT) file in this repository for more
information.
