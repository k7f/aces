#![feature(bool_to_option)]
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
mod solver;
mod state;
mod firing;
mod runner;
mod logging;
pub mod cli;

pub use error::AcesError;
pub use context::{Context, ContextHandle, Contextual, ExclusivelyContextual, InContext, InContextMut};
pub use content::{
    Content, ContentOrigin, PartialContent, Compilable, CompilableMut, CompilableAsContent,
    CompilableAsDependency,
};
pub use node::NodeID;
pub use atom::{Port, Link, Split, Fork, Join, AtomID, PortID, LinkID, ForkID, JoinID, Atomic};
pub use polynomial::{Polynomial, Monomials};
pub use ces::CEStructure;
pub use solver::{Solver, Solution};
pub use firing::{FiringComponent, FiringSet, FiringSequence};
pub use state::{State, Semantics};
pub use runner::Runner;
pub use logging::Logger;

use std::{
    fmt::{self, Write},
    num::NonZeroUsize,
};

/// A generic one-based serial identifier.
///
/// Used as a common internal type backing the conversion between
/// vector indices and specific identifiers, such as [`NodeID`],
/// [`PortID`], [`LinkID`], [`ForkID`], and [`JoinID`].
pub(crate) type ID = NonZeroUsize;

/// A scalar type common for monomial weight, node capacity and state.
///
/// Valid multiplicities are nonnegative integers or _&omega;_.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Multiplicity(u64);

impl Multiplicity {
    #[inline]
    pub const fn omega() -> Self {
        Multiplicity(u64::max_value())
    }

    pub fn finite(value: u64) -> Option<Self> {
        if value < u64::max_value() {
            Some(Multiplicity(value))
        } else {
            None
        }
    }

    #[inline]
    pub const fn zero() -> Self {
        Multiplicity(0)
    }

    #[inline]
    pub const fn one() -> Self {
        Multiplicity(1)
    }

    #[inline]
    pub fn is_omega(self) -> bool {
        self.0 == u64::max_value()
    }

    #[inline]
    pub fn is_zero(self) -> bool {
        self.0 == 0
    }

    #[inline]
    pub fn is_finite(self) -> bool {
        self.0 < u64::max_value()
    }

    #[inline]
    pub fn is_positive(self) -> bool {
        self.0 > 0
    }
}

impl fmt::Debug for Multiplicity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Multiplicity({})", self)
    }
}

impl fmt::Display for Multiplicity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_finite() {
            self.0.fmt(f)
        } else {
            f.write_char('ω')
        }
    }
}
