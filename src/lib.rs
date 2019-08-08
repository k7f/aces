#![allow(clippy::toplevel_ref_arg)]

#[macro_use]
extern crate log;
#[macro_use]
extern crate lazy_static;

mod error;
mod name;
pub mod node;
mod atom;
pub mod monomial;
mod polynomial;
mod ces;
mod context;
mod content;
mod yaml_script;
pub mod sat;
mod logging;
pub mod cli;

pub use error::AcesError;
pub use context::{Context, ContextHandle, Contextual, InContext, InContextMut};
pub use content::{Content, ContentOrigin};
pub use node::NodeID;
pub use atom::{Port, Link, PortID, LinkID, Atomic};
pub use polynomial::{Polynomial, Monomials};
pub use ces::CES;
pub use logging::Logger;

use std::num::NonZeroUsize;

/// A generic one-based serial identifier.
///
/// Used as a common internal type backing the conversion between
/// vector indices and specific identifiers, such as [`NodeID`],
/// [`PortID`], and [`LinkID`].
pub(crate) type ID = NonZeroUsize;
