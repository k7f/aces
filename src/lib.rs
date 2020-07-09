//! [Algebra of Cause-Effect
//! Structures](https://link.springer.com/book/10.1007/978-3-030-20461-7)
//! &mdash; an implementation of the theory.  This is the core library
//! of the [_Ascesis_](https://docs.rs/ascesis) project.

#![feature(bool_to_option)]
#![allow(clippy::toplevel_ref_arg)]

#[macro_use]
extern crate log;
#[macro_use]
extern crate lazy_static;

mod error;
mod name;
mod domain;
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
pub use domain::{DotId, Polarity};
pub use atom::{
    Port, Link, Wedge, Fork, Join, Fuset, AtomId, PortId, LinkId, ForkId, JoinId, FusetId, Atomic,
};
pub use polynomial::{Polynomial, Monomials};
pub use multiset::{Multiplicity, Capacity, Weight, WedgeWeights};
pub use ces::CEStructure;
pub use solver::{Solver, Solution};
pub use firing::{FiringComponent, FiringSet, FiringSubset, FiringSequence};
pub use state::{State, Goal, Semantics};
pub use runner::{Runner, StopCondition};
pub use logging::Logger;

use std::num::NonZeroUsize;

/// A generic one-based serial identifier.
///
/// Used as a common internal type backing the conversion between
/// vector indices and specific identifiers, such as [`DotId`],
/// [`PortId`], [`LinkId`], [`ForkId`], and [`JoinId`].
pub(crate) type AnyId = NonZeroUsize;
