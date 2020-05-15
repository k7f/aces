//! [Algebra of Cause-Effect
//! Structures](https://link.springer.com/book/10.1007/978-3-030-20461-7)
//! &mdash; an implementation of the theory.  This is the core library
//! of the [_Ascesis_](https://docs.rs/ascesis) project.
//!
//! In an attempt to clarify some of the obscure parts of the
//! implementation, several definitions are collected below.  They
//! are, in a way, a draft of a low-level reformulation of the theory.
//!
//! # Flow set
//!
//! Given any non-empty set _X_ (a set of nodes in the book), a _fork_
//! over _X_ is any pair _(host, suit)_, and a _join_ is any pair
//! _(suit, host)_, where _host_ is an element of _X_ and _suit_ is a
//! non-empty subset of _X_.
//!
//! A _flow set_ over _X_ (_aka_ fork-join hypergraph, _aka_ BF-graph)
//! is any set of forks and joins over _X_.  A _harc_ is any element
//! of a flow set (a fork or a join).
//!
//! The _pre-set_ (_post-set_) of a flow set _f_ is the set of hosts
//! of all forks (joins) in _f_.  The _carrier_ of a flow set is the
//! union of its pre-set and its post-set.  A flow set is _oriented_
//! iff its pre-set and post-set are disjoint.
//!
//! # Coherent flow set
//!
//! A flow set _f_ is _coherent_ iff
//!
//! * the union of suits of all joins in _f_ is equal to the pre-set
//!   of _f_, and
//! * the union of suits of all forks in _f_ is equal to the post-set
//!   of _f_.
//!
//! A flow set _e_ is a _star_ of a flow set _f_ iff _e_ is an
//! oriented subset of _f_ and _e_ has a singleton carrier.  A flow
//! set is _singular_ iff all its stars are singletons.  The _star
//! partition_ of a flow set _f_ is the set of all maximal stars of
//! _f_.
//!
//! FIXME isomorphism between the family of all coherent flow sets
//! over _X_ and the set of all c-e structures over _X_.
//!
//! FIXME isomorphism between the star partition of a coherent flow
//! set and the union of cause and effect functions of the
//! corresponding c-e structure.
//!
//! # Core set
//!
//! A _core set_ over _X_ is any minimal oriented coherent singular
//! flow set over _X_.  Given a flow set _f_, a _core subset_ of _f_
//! is any core set which is a subset of _f_, and the _core family_ of
//! _f_ is the set of all core subsets of _f_.
//!
//! FIXME isomorphism between the family of all core sets over _X_
//! and the set of all firing components over _X_.
//!
//! # Core graph
//!
//! The _core graph_ of a family _F_ of core sets over _X_ is the
//! bipartite digraph linking any element _x_ of _X_ to any core set
//! _f_ of _F_, such that _x_ is the host of any of the forks of _f_,
//! and linking any core set _g_ of _F_ to any element _x_ of _X_,
//! such that _x_ is the host of any of the joins of _g_.  The
//! _carrier_ of the core graph of _F_ is the union of carriers of all
//! core sets in _F_ (i.e. the set of all elements of _X_ which aren't
//! isolated in the graph).  The _co-carrier_ of a core graph is the
//! neighborhood of its carrier.
//!
//! The core graph _induced_ by a flow set _f_ is the core graph of
//! the family of all core subsets of _f_.
//!
//! # Shell graph
//!
//! A _shell graph_ over _X_ is any bipartite digraph _d_ linking
//! elements of _X_ to pairs of disjoint non-empty subsets of _X_,
//! such that for any pair _q_ = _(s, t)_, where _s_ and _t_ are
//! disjoint non-empty subsets of _X_, _q_ isn't isolated in _d_ iff
//! there is an arc to _q_ from all elements of _s_ and there is an
//! arc from _q_ to all elements of _t_, and there are no other arcs
//! incident to _q_.  The _carrier_ of a shell graph _d_ is the set of
//! all elements of _X_ which aren't isolated in _d_, and the
//! _co-carrier_ is the neighborhood of the carrier.
//!
//! The _shell graph_ of a family _F_ of core sets over _X_ is the
//! (only) shell graph whose co-carrier is the set of (pre-set,
//! post-set) pairs of all core sets in _F_.  The shell graph
//! _induced_ by a flow set _f_ is the shell graph of the family of
//! all core subsets of _f_.

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
pub use node::{NodeId, Face};
pub use atom::{
    Port, Link, Harc, Fork, Join, FlowSet, AtomId, PortId, LinkId, ForkId, JoinId, FlowSetId,
    Atomic,
};
pub use polynomial::{Polynomial, Monomials};
pub use multiset::{Multiplicity, Capacity, Weight, CoreWeight, ShellWeight};
pub use ces::CEStructure;
pub use solver::{Solver, Solution};
pub use firing::{FiringComponent, FiringSet, FiringSequence};
pub use state::{State, Goal, Semantics};
pub use runner::{Runner, StopCondition};
pub use logging::Logger;

use std::num::NonZeroUsize;

/// A generic one-based serial identifier.
///
/// Used as a common internal type backing the conversion between
/// vector indices and specific identifiers, such as [`NodeId`],
/// [`PortId`], [`LinkId`], [`ForkId`], and [`JoinId`].
pub(crate) type AnyId = NonZeroUsize;
