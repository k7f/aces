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
mod atom;
mod domain;
mod link;
mod wedge;
mod multiset;
mod frame;
mod fuset;
mod context;
mod content;
mod yaml_script;
pub mod sat;
mod solver;
mod state;
mod firing;
mod runner;
mod vis;
mod symmetry;
mod logging;

pub use error::{AcesError, AcesErrorKind};
pub use context::{Context, ContextHandle, Contextual, ExclusivelyContextual, InContext, InContextMut};
pub use content::{
    Content, ContentFormat, InteractiveFormat, PartialContent, Compilable, CompilableMut,
    CompilableAsContent, CompilableAsDependency,
};
pub use yaml_script::YamlFormat;
pub use atom::{AtomId, PortId, LinkId, ForkId, JoinId, FusetId, Atomic};
pub use domain::{DotId, Polarity, Port};
pub use link::Link;
pub use wedge::{Wedge, Fork, Join};
pub use frame::{Frame, FrameIter};
pub use multiset::{Multiplicity, Capacity, Weight, WedgeWeights};
pub use fuset::{Fuset, FusetHolder};
pub use solver::{Solver, Solution};
pub use firing::{FiringComponent, FiringSet, FiringSubset, FiringSequence};
pub use state::{State, Goal, Semantics};
pub use runner::{Runner, StopCondition};
pub use symmetry::Permutation;
pub use logging::Logger;

use std::num::NonZeroUsize;

/// A generic one-based serial identifier.
///
/// Used as a common internal type backing the conversion between
/// vector indices and specific identifiers, such as [`DotId`],
/// [`PortId`], [`LinkId`], [`ForkId`], and [`JoinId`].
pub(crate) type AnyId = NonZeroUsize;
