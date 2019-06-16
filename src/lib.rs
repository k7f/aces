#![allow(clippy::toplevel_ref_arg)]

#[macro_use]
extern crate lazy_static;

mod name;
mod node;
mod atom;
mod context;
mod polynomial;
mod ces;
mod spec;
mod error;
pub mod sat;
pub mod cli;

pub use context::{Context, Contextual, WithContext};
pub use node::NodeID;
pub use atom::{Port, Face, Link, PortID, LinkID};
pub use polynomial::{Monomial, Polynomial};
pub use ces::CES;

use std::num::NonZeroUsize;

/// A generic one-based serial identifier.
///
/// Used as a common internal type backing the conversion between
/// vector indices and specific identifiers, such as [`NodeID`],
/// [`PortID`], and [`LinkID`].
pub(crate) type ID = NonZeroUsize;
