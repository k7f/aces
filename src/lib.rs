#[macro_use]
extern crate lazy_static;

mod nodes;
mod atoms;
mod ces;
mod spec;
mod error;
pub mod cli;

pub use nodes::NodeSpace;
pub use atoms::{AtomSpace, Atom, Source, Sink, Link};
pub use ces::CES;
