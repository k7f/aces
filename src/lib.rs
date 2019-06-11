#[macro_use]
extern crate lazy_static;

mod context;
mod atom;
mod polynomial;
mod ces;
mod spec;
mod error;
pub mod sat;
pub mod cli;

pub use context::Context;
pub use atom::{Atom, Port, Face, Link};
pub use polynomial::{Monomial, Polynomial};
pub use ces::CES;
