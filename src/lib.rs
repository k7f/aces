#![feature(bool_to_option)]
#![allow(clippy::toplevel_ref_arg)]

#[macro_use]
extern crate log;
#[macro_use]
extern crate lazy_static;

mod error;
mod name;
mod node;
mod atom;
mod multiset;
mod polynomial;
mod ces;
mod context;
mod content;
mod yaml_script;
pub mod sat;
mod solver;
mod state;
mod firing;
mod runner;
mod vis;
mod logging;

pub use error::{AcesError, AcesErrorKind};
pub use context::{Context, ContextHandle, Contextual, ExclusivelyContextual, InContext, InContextMut};
pub use content::{
    Content, ContentFormat, InteractiveFormat, PartialContent, Compilable, CompilableMut,
    CompilableAsContent, CompilableAsDependency,
};
pub use yaml_script::YamlFormat;
pub use node::{NodeID, Face};
pub use atom::{Port, Link, Harc, Fork, Join, AtomID, PortID, LinkID, ForkID, JoinID, Atomic};
pub use polynomial::{Polynomial, Monomials};
pub use multiset::{Multiplicity, Capacity, Weight};
pub use ces::CEStructure;
pub use solver::{Solver, Solution};
pub use firing::{FiringComponent, FiringSet, FiringSequence};
pub use state::{State, Goal, Semantics};
pub use runner::Runner;
pub use logging::Logger;

use std::num::NonZeroUsize;

/// A generic one-based serial identifier.
///
/// Used as a common internal type backing the conversion between
/// vector indices and specific identifiers, such as [`NodeID`],
/// [`PortID`], [`LinkID`], [`ForkID`], and [`JoinID`].
pub(crate) type ID = NonZeroUsize;
