#[macro_use]
extern crate lazy_static;

mod context;
mod node;
mod atom;
mod polynomial;
mod ces;
mod spec;
mod error;
pub mod sat;
pub mod cli;

pub use context::Context;
pub use node::NodeID;
pub use atom::{Port, Face, Link, PortID, LinkID};
pub use polynomial::{Monomial, Polynomial};
pub use ces::CES;

use std::num::NonZeroUsize;
pub(crate) type ID = NonZeroUsize;
