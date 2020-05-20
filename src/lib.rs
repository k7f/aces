//! [Algebra of Cause-Effect
//! Structures](https://link.springer.com/book/10.1007/978-3-030-20461-7)
//! &mdash; an implementation of the theory.  This is the core library
//! of the [_Ascesis_](https://docs.rs/ascesis) project.
//!
//! In an attempt to clarify some of the obscure parts of the
//! implementation, several definitions are collected below.  They
//! are, in a way, a draft of a low-level reformulation of the theory.
//!
//! # Flow sets
//!
//! Given any non-empty _domain_ _X_ (a set of nodes in the book), a
//! _fork_ over _X_ is any pair _(h, m)_, and a _join_ is any pair
//! _(m, h)_, where _h_ is an element of _X_, called a _host_ of a
//! fork (or join), and _m_ is a non-empty subset of _X_, called a
//! _suit_ of a fork (or join).  A fork (join) is a _loop_ iff its
//! host is a member of its suit.
//!
//! A _flow set_ over _X_ is any set of forks and joins over _X_.  A
//! _harc_ is any element of a flow set: a fork or a join.
//!
//! The _pre-set_ (_post-set_) of a flow set _f_ is the set of hosts
//! of all forks (joins) in _f_.  The _carrier_ of a flow set is the
//! union of its pre-set and its post-set, and the _interior_ is the
//! intersection.  The _co-pre-set_ (_co-post-set_) of a flow set _f_
//! is the union of suits of all forks (joins) in _f_.  The
//! _co-carrier_ of a flow set is the union of all its suits and
//! _co-interior_ &mdash; the intersection.
//!
//! A _source_ of a flow set is a pre-set element not in the post-set,
//! and a _sink_ &mdash; a post-set element not in the pre-set.  A
//! source (sink) is _strong_, if it is in the co-post-set
//! (co-pre-set), or _weak_, otherwise.  If a weak source (sink) is in
//! the co-pre-set (co-post-set), then it is a _pseudo-source_
//! (_pseudo-sink_).
//!
//! A flow set is _flat_ iff it has empty interior, i.e. if its
//! pre-set and post-set are disjoint.  It is convenient to say _flat
//! set_ (_subset_, _path_, _track_, etc.), instead of _flat flow set_
//! (_subset_, _path_, _track_, etc.).
//!
//! A flow set _e_ is a _star_ of a flow set _f_ iff _e_ is a flat
//! subset of _f_ and _e_ has a singleton carrier.  A flow set is
//! _singular_ iff all its stars are singletons.  The _star partition_
//! of a flow set _f_ is the set of all maximal stars of _f_.
//!
//! ### Fork-join hypergraphs
//!
//! An alternative description of flow sets is also possible &mdash; a
//! hint to this possibility is the name _harc_ introduced to denote
//! both variants, a fork or a join (another borrowed term is _star_).
//! Instead of defining forks and joins over a fixed domain common to
//! all flow sets, one may want to replace the notion of a flow set
//! with a class of labeled directed hypergraphs, and define forks and
//! joins over node sets of individual hypergraphs.
//!
//! A _fork-join hypergraph_ may be defined as a directed hypergraph
//! with a hyperedge-labeling function separating forks from joins,
//! and satisfying the property that all forks are singleton-tail
//! hyperedges (F-edges) and all joins are singleton-head hyperedges
//! (B-edges).  In this setting, for example, co-pre-set is the
//! neighborhood of the pre-set, co-post-set is the neighborhood of
//! the post-set, and co-carrier is the neighborhood of the carrier.
//!
//! # Coherence
//!
//! Given a flow set _f_ over _X_ and two elements _x_ and _y_ of _X,
//! we say_ that
//!
//! * _y_ is _fork-hosted_ (_join-hosted_) by _x_ iff _x_ is the host
//!   and _y_ is in the suit of some fork (join) of _f_;
//!
//! * _y_ _weakly follows_ _x_ iff _y_ is fork-hosted by _x_, or _x_
//!   is join-hosted by _y_, but not both; equivalently, we say that
//!   _y_ is a _weak follower_ of _x_;
//!
//! * _y_ _strongly follows_ _x_ iff _y_ is fork-hosted by _x_ and _x_
//!   is join-hosted by _y_; equivalently, we say that _y_ is a
//!   _strong follower_ of _x_.
//!
//! A flow set _f_ over _X_ is _coherent_ iff it doesn't contain any
//! weak followers.
//!
//! ### Set equality condition
//!
//! For a flow set to be coherent it is necessary that its pre-set and
//! co-post-set be equal, and that its post-set and co-pre-set be
//! equal (note, that given any fork of a coherent flow set, its head
//! must be join-hosted by all suit members, etc.).  This is the _set
//! equality condition_ of coherence.
//!
//! However, set equality isn't a sufficient condition of coherence.
//! For example, it is possible to find a coherent flow set _f_ over
//! some domain _X_, and a fork, not a member of _f_, such that its
//! host _x_ is in the pre-set of _f_ and all elements of its suit are
//! in the co-pre-set of _f_, but not all are hosted by _x_ in _f_.
//! Then, if _f_ is augmented with such a fork to form a flow set _g_,
//! the set equality condition is satisfied by _g_, but _g_ isn't
//! coherent, because _x_ has a weak follower in _g_.
//!
//! # Flows
//!
//! Given a flow set _f_ over _X_, an element _y_ of _X_ is
//! _fork-connected_ (_join-connected_) in _f_ to an element _x_ of
//! _X_ iff _y_ is fork-hosted (join-hosted) by _x_ or by some other
//! element of _X_ fork-connected (join-connected) to _x_ in _f_.  A
//! subset _Y_ of _X_ is fork-connected (join-connected) in _f_ to an
//! element _x_ of _X_ iff every element of _Y_ is fork-connected
//! (join-connected) to _x_ in _f_.
//!
//! Given elements _s_ and _t_ of _X_, a _flow path_ _&pi;_ from _s_
//! to _t_ is a singular flow set over _X_, such that the post-set of
//! _&pi;_ is fork-connected to _s_ in _&pi;_ and the pre-set of
//! _&pi;_ is join-connected to _t_ in _&pi;_.  A flow set _f_ over
//! _X_ forms a _flow cycle_ iff there is an _x_ in _X_ such that _f_
//! is a flow path from _x_ to _x_.
//!
//! A flow path may not be coherent.  An incoherent flow path may have
//! weak sources or weak sinks, but FIXME
//!
//! Given a flow set _f_ over _X_, and two subsets _Y_, _Z_ of _X_,
//! _Z_ is _hyper-connected_ to _Y_ in _f_ iff any element of _Z_ is
//! fork-connected to some element of _Y_ and any element of _Y_ is
//! join-connected to some element of _Z_.
//!
//! Given subsets _S_ and _T_ of _X_, a _flow track_(or simply a
//! _flow_) _&phi;_ from _S_ to _T_ is a singular coherent flow set
//! over _X_, such that the post-set of _&phi;_ is hyper-connected to
//! _S_ in _&phi;_ and the pre-set of _&phi;_ is hyper-connected to
//! _T_ in _&phi;_.  A flow track is _cycle-free_ iff it doesn't
//! contain any flow cycles.
//!
//! Note, that all flows are minimal, i.e. a flow track never includes
//! another flow track as a proper subset.
//!
//! Note also, that for a flow set to be a flat track it is necessary
//! and sufficient that it is a minimal singular coherent flat set.
//!
//! # Core graph
//!
//! The _core graph_ of a family _F_ of flat tracks over _X_ is the
//! bipartite digraph linking any element _x_ of _X_ to any flat track
//! _f_ of _F_, such that _x_ is the host of any of the forks of _f_,
//! and linking any flat track _g_ of _F_ to any element _x_ of _X_,
//! such that _x_ is the host of any of the joins of _g_.  The
//! _carrier_ of the core graph of _F_ is the union of carriers of all
//! flat tracks in _F_ (i.e. the set of all elements of _X_ which aren't
//! isolated in the graph).  The _co-carrier_ of a core graph is the
//! neighborhood of its carrier.
//!
//! Given a flow set _f_, a _core subset_ of _f_ is any flat track
//! which is a subset of _f_, and the _core family_ of _f_ is the set
//! of all core subsets of _f_.  The core graph _induced_ by a flow
//! set _f_ is the core graph of the family of all core subsets of
//! _f_.

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
