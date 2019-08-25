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
mod firing;
mod context;
mod content;
mod yaml_script;
pub mod sat;
mod logging;
pub mod cli;

pub use error::AcesError;
pub use context::{Context, ContextHandle, Contextual, InContext, InContextMut};
pub use content::{Content, ContentOrigin, PartialContent, CompilableAsContent};
pub use node::NodeID;
pub use atom::{Port, Link, Fork, Join, PortID, LinkID, ForkID, JoinID, Atomic};
pub use polynomial::{Polynomial, Monomials};
pub use ces::CEStructure;
pub use firing::FiringComponent;
pub use logging::Logger;

use std::num::NonZeroUsize;

/// A generic one-based serial identifier.
///
/// Used as a common internal type backing the conversion between
/// vector indices and specific identifiers, such as [`NodeID`],
/// [`PortID`], [`LinkID`], [`ForkID`], and [`JoinID`].
pub(crate) type ID = NonZeroUsize;
