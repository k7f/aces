//! [Algebra of Cause-Effect
//! Structures](https://link.springer.com/book/10.1007/978-3-030-20461-7)
//! &mdash; an implementation of the theory.  This is the core library
//! of the [_Ascesis_](https://docs.rs/ascesis) project.
//!
//! In an attempt to clarify some of the obscure parts of the
//! implementation of _aces_ library, several definitions are
//! collected below.
//!
//! These notes are, in a way, a draft of a &ldquo;lower level&rdquo;
//! reformulation of the theory, not as concise and elegant as the
//! original but, the claim is, better suited for the design and
//! analysis of implementation details &mdash; for instance, the
//! algorithms of structural decomposition.
//!
//! # Flow sets
//!
//! Given any non-empty _domain_ _X_ (a set of nodes in the book), a
//! _fork_ over _X_ is any pair _(h, w)_, and a _join_ is any pair
//! _(w, h)_, where _h_ is an element of _X_, called a _host_ of a
//! fork (or join), and _w_ is a non-empty subset of _X_, called a
//! _wing_ of a fork (or join).  A fork (join) is a _loop_ iff its
//! host is a member of its wing.
//!
//! A _flow set_ over _X_ is any set of forks and joins over _X_.  A
//! _harc_ is any element of a flow set: a fork or a join.
//!
//! We say that a domain element _y_ is _hosted_ (_fork-hosted_ or
//! _join-hosted_) by a domain element _x_ in a flow set _&phi;_, and
//! that _x_ is winged (_fork-winged_ or _join-winged_) by _y_, iff
//! _x_ is the host of some harc (fork or join) of _&phi;_, and _y_ is
//! in the wing of the same harc.  These two (asymmetric) relations
//! may be represented in symbolic notation:
//!
//! > _x_ &rarr;<sub>_&phi;_</sub> _y_, which denotes that _y_ is fork-hosted by _x_ in _&phi;_,<br>
//! > _x_ &larr;<sub>_&phi;_</sub> _y_, which denotes that _y_ is join-hosted by _x_ in _&phi;_,
//!
//! and their complements are
//!
//! > _x_ &nrarr;<sub>_&phi;_</sub> _y_, which holds if _y_ isn't fork-hosted by _x_ in _&phi;_, and<br>
//! > _x_ &nlarr;<sub>_&phi;_</sub> _y_, which holds if _y_ isn't join-hosted by _x_ in _&phi;_.
//!
//! The subscript may be omitted, when it is clear what flow set
//! happens to be under consideration.
//!
//! ### Classification of domain elements
//!
//! The _pre-set_ (_post-set_) of a flow set _&phi;_ is the set of
//! hosts of all forks (joins) in _&phi;_.  The _carrier_ of a flow
//! set is the set of all hosts, i.e. the union of its pre-set and
//! post-set, and the _interior_ is the intersection.
//!
//! The _under-set_ (_over-set_) of a flow set _&phi;_ is the union of
//! wings of all forks (joins) in _&phi;_.  The _wingset_ of a flow
//! set is the set of all its wings and the _range_ &mdash; the union
//! of all wings, i.e. the union of under-set and over-set.  The
//! _co-interior_ is the intersection of under-set and over-set.
//! Finally, the _residue_ contains all non-carrier elements of the
//! range.
//!
//! The four basic classes may be defined symbolically,
//!
//! > <tt>Pre</tt>(_&phi;_)<tt>&nbsp;&nbsp;</tt>&nbsp;=&nbsp;{_x_ &in; _X_ | &exist;<sub>_y_</sub>&nbsp;_x_ &rarr;<sub>_&phi;_</sub> _y_},<br>
//! > <tt>Post</tt>(_&phi;_)<tt>&nbsp;</tt>&nbsp;=&nbsp;{_x_ &in; _X_ | &exist;<sub>_y_</sub>&nbsp;_x_ &larr;<sub>_&phi;_</sub> _y_},<br>
//! > <tt>Under</tt>(_&phi;_)&nbsp;=&nbsp;{_x_ &in; _X_ | &exist;<sub>_y_</sub>&nbsp;_y_ &rarr;<sub>_&phi;_</sub> _x_},<br>
//! > <tt>Over</tt>(_&phi;_)<tt>&nbsp;</tt>&nbsp;=&nbsp;{_x_ &in; _X_ | &exist;<sub>_y_</sub>&nbsp;_y_ &larr;<sub>_&phi;_</sub> _x_},
//!
//! and similarly,
//!
//! > <tt>Range</tt>(_&phi;_)&nbsp;=&nbsp;<tt>Over</tt>(_&phi;_) &cup; <tt>Under</tt>(_&phi;_)&nbsp;=&nbsp;{_x_ &in; _X_ | &exist;<sub>_y_</sub>&nbsp;_y_ &larr;<sub>_&phi;_</sub> _x_ &or; _y_ &rarr;<sub>_&phi;_</sub> _x_}, and<br>
//! > <tt>Wings</tt>(_&phi;_)&nbsp;=&nbsp;{_w_ &subset; _X_ | &exist;<sub>_x_</sub>&nbsp;(_x_, _w_) &in; _&phi;_ &or; (_w_, _x_) &in; _&phi;_}.
//!
//! Altogether, any flow set generates a partition of the domain into
//! some number (up to 23) of disjoint classes listed in the cells of
//! the following _domain element classification table_.
//!
//! | <p align="right">_wing member_</p>_host_ | <center>not a member<br>(not in range)</center> | <center>join's wing member<br>(in over-set)</center> | <center>fork's wing member<br>(in under-set)</center> | <center>both<br>(in co-interior)</center> |
//! |-----------------------------------------|:------------:|:--------------:|:--------------:|:-------------:|
//! | **not a host**<br>**(not in carrier)**  | **isolated** | upper residual | lower residual | full residual |
//! | **host of a fork**<br>**(in pre-set)**  | weak<br>source | broken or **strong**<br>**source** | weak<br>pseudo-source | broken or strong<br>pseudo-source |
//! | **host of a join**<br>**(in post-set)** | weak<br>sink | weak<br>pseudo-sink | broken or **strong**<br>**sink** | broken or strong<br>pseudo-sink |
//! | **both**<br>**(in interior)**           | weak<br>internal | broken or strong<br>upper internal | broken or strong<br>lower internal | broken or **strong**<br>full **internal** |
//!
//! In particular, a _source_ of a flow set is a pre-set element that
//! is neither in the post-set nor in the under-set, and a _sink_
//! &mdash; a post-set element that is neither in the pre-set nor in
//! the over-set.  A source, a sink, or an internal element is _weak_,
//! if it is not in the range.  A pre-set element that is in the
//! under-set, but not in the interior, is a _pseudo-source_, and a
//! post-set element that is in the over-set, but not in the interior
//! &mdash; a _pseudo-sink_.  A pseudo-source or pseudo-sink is
//! _weak_, if it is not in the co-interior.
//!
//! Seven cells of the table are split into &ldquo;broken&rdquo; and
//! &ldquo;strong&rdquo; variants.  A carrier element _x_ is _broken_
//! iff there are two domain elements such that _x_ is fork-winged
//! (join-winged) by them both, but _x_ is join-hosted (fork-hosted)
//! by exactly one of them.  A carrier element is _strong_ iff it is
//! neither weak nor broken.
//!
//! The formulae for a strong and for a broken domain element _x_ may
//! be written as
//!
//! > <tt>IsStrong</tt>(_x_)&nbsp;&equiv;&nbsp;&forall;<sub>_y_</sub>&nbsp; (_x_ &rarr; _y_ &hArr; _y_ &larr; _x_) &nbsp;&and;&nbsp; (_x_ &larr; _y_ &hArr; _y_ &rarr; _x_),<br>
//! > <tt>IsBroken</tt>(_x_)&nbsp;&equiv;&nbsp;(&exist;<sub>_y_,_z_</sub>&nbsp; _x_ &rarr; _y_ &and; _z_ &larr; _x_ &and; _y_ &nlarr; _x_)&nbsp;
//! > &or; &nbsp;(&exist;<sub>_y_,_z_</sub>&nbsp; _x_ &larr; _y_ &and; _z_ &rarr; _x_ &and; _y_ &nrarr; _x_)
//!
//!
//! A domain element is _isolated_ in a flow set _&phi;_ iff it is neither
//! in the carrier nor in the residue of _&phi;_.  We say, that any
//! non-isolated element _occurs_ in _&phi;_.
//!
//! ### Classification of flow sets
//!
//! A flow set is _flat_ iff it has empty interior, i.e. if its
//! pre-set and post-set are disjoint.  It is convenient to say _flat
//! set_ (_subset_, _path_, _track_, etc.), instead of _flat flow set_
//! (_subset_, _path_, _track_, etc.).
//!
//! A flow set _&alpha;_ is a _star_ of a flow set _&phi;_ iff
//! _&alpha;_ is a flat subset of _&phi;_ and _&alpha;_ has a
//! singleton carrier.  A flow set is _singular_ iff all its stars are
//! singletons.  The _star partition_ of a flow set _&phi;_ is the set
//! of all maximal stars of _&phi;_.
//!
//! Given a flow set _&phi;_, the maximal star of _&phi;_ which is
//! fork-hosted by _x_, i.e. for which _x_ is the only pre-set
//! element, is called the _lower star_ of _x_ in _&phi;_, and is
//! denoted by _x_<sub>&star;</sub>(_&phi;_), or simply by
//! _x_<sub>&star;</sub>.  Similarly, _x_<sup>&star;</sup>(_&phi;_)
//! denotes the _upper star_ of _x_ in _&phi;_ &mdash; the maximal
//! star join-hosted by _x_.  Of course, the sets
//! <tt>Pre</tt>(_x_<sup>&star;</sup>),
//! <tt>Post</tt>(_x_<sub>&star;</sub>),
//! <tt>Under</tt>(_x_<sup>&star;</sup>), and
//! <tt>Over</tt>(_x_<sub>&star;</sub>) are all empty for any domain
//! element _x_ in any flow set.
//!
//! Two flow sets are _colliding_ iff they have overlapping wingsets
//! (i.e. their wingsets are incomparable by inclusion but
//! intersecting).
//!
//! > <tt>Wings</tt>(_&phi;_) &cap; <tt>Wings</tt>(_&psi;_) 
//!
//! ### Fork-join hypergraphs
//!
//! An alternative description of flow sets is also possible, although
//! apparently less convenient in practice.  Some of the names
//! introduced above, like _harc_ and _star_, are hints to this
//! possibility.
//!
//! Instead of defining forks and joins over a fixed domain common to
//! all flow sets, one may want to replace the notion of a flow set
//! with a variant of labeled directed hypergraphs, and define forks
//! and joins over node sets of individual hypergraphs.
//!
//! A _fork-join hypergraph_ may thus be defined as a directed
//! hypergraph with a hyperedge-labeling function separating forks
//! from joins, and satisfying the property, that all forks are
//! singleton-tail hyperedges (F-edges) and all joins are
//! singleton-head hyperedges (B-edges).  In this setting, for
//! example, under-set is the neighborhood of the pre-set, over-set is
//! the neighborhood of the post-set, and range is the neighborhood of
//! the carrier.
//!
//! # Coherence
//!
//! Given a flow set _&phi;_ over _X_ and two elements _x_ and _y_ of _X,
//! we say_ that
//!
//! * _y_ _weakly follows_ _x_ iff _y_ is fork-hosted by _x_ or _x_ is
//! join-hosted by _y_, but not both; synonymously, _y_ is a _weak
//! follower_ of _x_;
//!
//! * _y_ _strongly follows_ _x_ iff _y_ is fork-hosted by _x_ and _x_
//!   is join-hosted by _y_; synonymously, _y_ is a _strong follower_
//!   of _x_.
//!
//! A flow set _&phi;_ over _X_ is _coherent_ iff it doesn't contain any
//! weak followers.
//!
//! As may be shown by examination of the domain element
//! classification table, a coherent flow set generates the domain
//! partition containing no more than the four classes listed in bold
//! face in the main diagonal of the table.  Thus, only strong domain
//! elements may occur in a coherent flow set: strong sources, strong
//! sinks, and strong internals.
//!
//! A _strong patching_ of a domain element _x_ in a flow set _&phi;_ is a
//! pair of domain subsets (<tt><b>P</b></tt><sub>_&phi;_</sub>(_x_),
//! <tt><b>p</b></tt><sub>_&phi;_</sub>(_x_)), where the components
//! <tt><b>P</b></tt><sub>_&phi;_</sub>(_x_)&nbsp;=&nbsp;{_y_ | _x_ &larr;<sub>_&phi;_</sub> _y_ &and; _y_ &rarr;<sub>_&phi;_</sub> _x_} and
//! <tt><b>p</b></tt><sub>_&phi;_</sub>(_x_)&nbsp;=&nbsp;{_y_ | _x_ &rarr;<sub>_&phi;_</sub> _y_ &and; _y_ &larr;<sub>_&phi;_</sub> _x_} are the _upper_ and _lower strong patching_ of _x_ in _&phi;_.
//!
//! A _weak patching_ of a domain element _x_ in a flow set _&phi;_ is a
//! 4-tuple of domain subsets (<tt><b>W</b></tt><sub>_&phi;_</sub>, <tt><b>w</b></tt><sub>_&phi;_</sub>,
//! <tt><b>H</b></tt><sub>_&phi;_</sub>, <tt><b>h</b></tt><sub>_&phi;_</sub>), where the components
//! <tt><b>W</b></tt><sub>_&phi;_</sub>&nbsp;=&nbsp;{_y_ | _x_ &larr;<sub>_&phi;_</sub> _y_ &and; _y_ &nrarr;<sub>_&phi;_</sub> _x_},
//! <tt><b>w</b></tt><sub>_&phi;_</sub>&nbsp;=&nbsp;{_y_ | _x_ &rarr;<sub>_&phi;_</sub> _y_ &and; _y_ &nlarr;<sub>_&phi;_</sub> _x_},
//! <tt><b>H</b></tt><sub>_&phi;_</sub>&nbsp;=&nbsp;{_y_ | _y_ &rarr;<sub>_&phi;_</sub> _x_ &and; _x_ &nlarr;<sub>_&phi;_</sub> _y_}, and
//! <tt><b>h</b></tt><sub>_&phi;_</sub>&nbsp;=&nbsp;{_y_ | _y_ &larr;<sub>_&phi;_</sub> _x_ &and; _x_ &nrarr;<sub>_&phi;_</sub> _y_},
//! are the _upper_ and _lower wing patching_ of _x_ in _&phi;_ (the
//! wing members hosted by _x_), and the _upper_ and _lower host
//! patching_ of _x_ in _&phi;_ (the hosts winged by _x_).
//!
//! The following _weak patching pattern table_ lists possible
//! component patterns of weak patching of a domain element, depending
//! on its class.  Non-empty components are represented by the
//! corresponding letters: <tt><b>W</b></tt>, <tt><b>w</b></tt>,
//! <tt><b>H</b></tt>, and <tt><b>h</b></tt>.  Empty components are
//! omitted, unless all four components are empty, in which case the
//! pattern is represented by the symbol <tt>&empty;</tt>.
//!
//! | <p align="right">_wing member_</p>_host_ | <center>not a member<br>(not in range)</center> | <center>join's wing member<br>(in over-set)</center> | <center>fork's wing member<br>(in under-set)</center> | <center>both<br>(in co-interior)</center> |
//! |-----------------------------------------|:----:|:----------------------:|:----------------------:|:----------------------:|
//! | **not a host**<br>**(not in carrier)**  | <tt>&empty;</tt> | <tt><b>h</b></tt> | <tt><b>H</b></tt> | <tt><b>Hh</b></tt> |
//! | **host of a fork**<br>**(in pre-set)**  | <tt><b>w</b></tt> | <tt><b>wh</b></tt>, <tt><b>w</b></tt>, <tt><b>h</b></tt>, <tt>&empty;</tt> | <tt><b>wH</b></tt> | <tt><b>wHh</b></tt>, <tt><b>wH</b></tt>, <tt><b>Hh</b></tt>, <tt><b>H</b></tt> |
//! | **host of a join**<br>**(in post-set)** | <tt><b>W</b></tt> | <tt><b>Wh</b></tt> | <tt><b>WH</b></tt>, <tt><b>W</b></tt>, <tt><b>H</b></tt>, <tt>&empty;</tt> | <tt><b>WHh</b></tt>, <tt><b>Wh</b></tt>, <tt><b>Hh</b></tt>, <tt><b>h</b></tt> |
//! | **both**<br>**(in interior)**           | <tt><b>Ww</b></tt> | <tt><b>Wwh</b></tt>, <tt><b>Ww</b></tt>, <tt><b>Wh</b></tt>, <tt><b>W</b></tt> | <tt><b>WwH</b></tt>, <tt><b>Ww</b></tt>, <tt><b>wH</b></tt>, <tt><b>w</b></tt> | any pattern |
//!
//! ### Algebra
//!
//! Given any two coherent flow sets, their union is a coherent flow
//! set.  FIXME However, neither intersection, nor set difference has
//! to be.
//!
//! ### Set equality condition
//!
//! For a flow set to be coherent, it is necessary that its pre-set be
//! equal to over-set and its post-set be equal to under-set.  This is
//! the _set equality condition_ of coherence.
//!
//! To see that this is the case, note that if a flow set _&phi;_
//! contains a fork with the head not in the over-set of _&phi;_, then
//! all members of the fork's wing are weak followers of the head;
//! conversely, if a flow set _&phi;_ contains a join with head _y_
//! and some wing member _x_ not in the pre-set of _&phi;_, then _y_
//! is a weak follower of _x_.  Hence, if a flow set doesn't contain
//! weak followers, then its pre-set and over-set must be equal.  The
//! other equality is supported by the symmetric argument.
//!
//! However, set equality isn't a sufficient condition of coherence.
//! For example, it is possible to find a coherent flow set _&phi;_
//! over some domain _X_, and a fork (_h_, _w_), not a member of
//! _&phi;_, such that _h_ is in <tt>Pre</tt>(_&phi;_), _w_ is a
//! subset of <tt>Under</tt>(_&phi;_), but not a subset of
//! <tt>Under</tt>(_h_<sub>&star;</sub>(_&phi;_)).  Then, if _&phi;_
//! is augmented with such a fork to form a flow set _&psi;_, the set
//! equality condition is satisfied by _&psi;_, but _&psi;_ isn't
//! coherent, because any element of _w_ not in
//! <tt>Under</tt>(_h_<sub>&star;</sub>(_&phi;_)) is a weak follower
//! of _h_ in _&psi;_.
//!
//! # Flows
//!
//! Given a flow set _&phi;_ over _X_, an element _y_ of _X_ is
//! _fork-connected_ (_join-connected_) in _&phi;_ to an element _x_ of
//! _X_ iff _y_ is fork-hosted (join-hosted) by _x_ or by some other
//! element of _X_ which is fork-connected (join-connected) to _x_ in
//! _&phi;_.  The two relations are notated as
//!
//! > _x_ &Rarr;<sub>_&phi;_</sub> _y_, which holds if _y_ is fork-connected to _x_ in _&phi;_, and<br>
//! > _x_ &Larr;<sub>_&phi;_</sub> _y_, which holds if _y_ is join-connected to _x_ in _&phi;_.
//!
//! A subset _Y_ of _X_ is fork-connected (join-connected) in _&phi;_ to
//! an element _x_ of _X_ iff every element of _Y_ is fork-connected
//! (join-connected) to _x_ in _&phi;_.
//!
//! ### Flow paths
//!
//! Given elements _s_ and _t_ of _X_, a _flow path_ _&pi;_ from _s_
//! to _t_ is a minimal non-empty flow set over _X_, such that the
//! post-set of _&pi;_ is fork-connected to _s_ in _&pi;_ and the
//! pre-set of _&pi;_ is join-connected to _t_ in _&pi;_.
//!
//! > _s_ &Rarr;<sub>_&pi;_</sub> <tt>Post</tt>(_&pi;_) &and; _t_ &Larr;<sub>_&pi;_</sub> <tt>Pre</tt>(_&pi;_)
//!
//! A flow set _&phi;_ over _X_ forms a _flow cycle_ iff there is an _x_
//! in _X_ such that _&phi;_ is a flow path from _x_ to _x_.
//!
//! FIXME Given a path _&pi;<sub>1</sub>_ from _s_ to _r_ and a path
//! _&pi;<sub>2</sub>_ from _r_ to _t_
//!
//! FIXME If a domain element _r_ is internal in a path _&pi;_ from
//! _s_ to _t_
//!
//! FIXME all flow paths are singular.
//!
//! ### Flow tracks
//!
//! Given a flow set _&phi;_ over _X_, and two subsets _Y_, _Z_ of
//! _X_, _Z_ is _hyper-connected_ to _Y_ in _&phi;_ iff any element of
//! _Z_ is fork-connected to some element of _Y_ and any element of
//! _Y_ is join-connected to some element of _Z_.
//!
//! Given subsets _S_ and _T_ of _X_, a _flow track_ _&tau;_ from _S_
//! to _T_ is a singular coherent flow set over _X_, such that the
//! post-set of _&tau;_ is hyper-connected to _S_ in _&tau;_ and the
//! pre-set of _&tau;_ is hyper-connected to _T_ in _&tau;_.  A flow
//! track is _cycle-free_ iff it doesn't contain any flow cycles.
//!
//! Note, that all flows tracks are minimal, i.e. a flow track never
//! includes another flow track as a proper subset.
//!
//! Note also, that for a flow set to be a flat track it is necessary
//! and sufficient that it is a minimal singular coherent flat set.
//!
//! # Core graph
//!
//! The _core graph_ of a family _&Phi;_ of flat tracks over _X_ is
//! the bipartite digraph linking any element _x_ of _X_ to any flat
//! track _&phi;_ of _&Phi;_, such that _x_ is the host of any of the
//! forks of _&phi;_, and linking any flat track _&psi;_ of _&Phi;_ to
//! any element _x_ of _X_, such that _x_ is the host of any of the
//! joins of _&psi;_.  The _carrier_ of the core graph of _&Phi;_ is
//! the union of carriers of all flat tracks in _&Phi;_ (i.e. the set
//! of all elements of _X_ which aren't isolated in the graph).  The
//! _range_ of a core graph is the neighborhood of its carrier.
//!
//! Given a flow set _&phi;_, a _core subset_ of _&phi;_ is any flat
//! track which is a subset of _&phi;_, and the _core family_ of
//! _&phi;_ is the set of all core subsets of _&phi;_.  The core graph
//! _induced_ by a flow set _&phi;_ is the core graph of the family of
//! all core subsets of _&phi;_.

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
