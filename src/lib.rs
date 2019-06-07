#[macro_use]
extern crate lazy_static;

mod nodes;
mod atoms;
mod context;
mod ces;
mod spec;
mod error;
pub mod sat;
pub mod cli;

pub use context::Context;
pub use atoms::{Atom, Source, Sink, Link};
pub use ces::CES;
