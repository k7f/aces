//! A common representation of node capacities, weight labels and
//! state.
//!
//! # Three types of weight labeling
//!
//! The implementation supports three different ways of
//! weight-labeling of c-e structures.  The purpose of all three is to
//! attach weights to arcs of core graphs, i.e. to monomial causes and
//! effects of nodes in firing components.
//!
//! * Core-weight: individual weight label explicitly attached to an
//!   arc of a specific [core graph](../index.html#core-graph).
//!
//! * Shell-weight: generic weight attached to an arc of a [shell
//!   graph](../index.html#shell-graph).
//!
//! * Flow-weight: generic weight of monomial causes or effects of a
//!   node.
//!
//!   Flow-weights represent the weight labeling used in the standard
//!   notation for polynomials.  Implementation-wise, they are
//!   attached to elements of a [flow set](../index.html#flow-sets)
//!   (forks and joins) and inherited by core-weights of all
//!   corresponding arcs of the induced core graph.
//!
//!   Note that, in general, fork (join) wings are contained in, not
//!   equal to, post-sets (pre-sets) of the corresponding [flat
//!   tracks](../index.html#core-graph).
//!
//! # Behavior
//!
//! A finite weight of a node _x_ occurring in the pre-set of a
//! transaction _t_ indicates the number of tokens to take out of node
//! _x_ if transaction _t_ fires.  A finite weight of a node _x_
//! occurring in the post-set of a transaction _t_ indicates the
//! number of tokens to put into node _x_ if transaction _t_ fires.
//!
//! The finite positive weights also imply constraints a state must
//! satisfy for a transaction to fire: the minimal number of tokens to
//! be supplied by pre-set nodes, so that token transfer may happen,
//! and the maximal number of tokens in post-set nodes, so that node
//! capacities aren't exceeded.
//!
//! If weight zero is attached to a pre-set node _x_ of a transaction
//! _t_, then tokens aren't taken out of _x_ when _t_ fires.  However,
//! transaction _t_ never fires, unless there is a token in node _x_.
//! Another special case is the _&omega;_ (infinite) weight attached
//! to a pre-set node, which indicates that the presence of a token in
//! that node inhibits firing.

use std::{
    str::FromStr,
    fmt::{self, Write},
};
use crate::{FlowSetId, Context, ExclusivelyContextual, AcesError, AcesErrorKind, node::NodeSetId};

/// A scalar type common for node capacity, weight labels and state.
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

    #[inline]
    pub fn is_multiple(self) -> bool {
        self.0 > 1
    }

    pub fn checked_add(self, other: Self) -> Option<Self> {
        if self.0 == u64::max_value() {
            Some(self)
        } else if other.0 == u64::max_value() {
            Some(other)
        } else {
            self.0.checked_add(other.0).and_then(|result| {
                if result == u64::max_value() {
                    None
                } else {
                    Some(Multiplicity(result))
                }
            })
        }
    }

    pub fn checked_sub(self, other: Self) -> Option<Self> {
        if self.0 == u64::max_value() {
            Some(self)
        } else if other.0 == u64::max_value() {
            if self.0 == 0 {
                Some(self)
            } else {
                None
            }
        } else {
            self.0.checked_sub(other.0).map(Multiplicity)
        }
    }

    /// Note: don't use this for evaluation of state transition; call
    /// [`checked_add()`] instead.
    ///
    /// [`checked_add()`]: Multiplicity::checked_add()
    pub fn saturating_add(self, other: Self) -> Self {
        if self.0 == u64::max_value() {
            self
        } else {
            let result = self.0.saturating_add(other.0);

            if result == u64::max_value() {
                Multiplicity(u64::max_value() - 1)
            } else {
                Multiplicity(result)
            }
        }
    }

    /// Note: don't use this for evaluation of state transition; call
    /// [`checked_sub()`] instead.
    ///
    /// [`checked_sub()`]: Multiplicity::checked_sub()
    pub fn saturating_sub(self, other: Self) -> Self {
        if self.0 == u64::max_value() {
            self
        } else if other.0 == u64::max_value() {
            if self.0 == 0 {
                self
            } else {
                Multiplicity(0)
            }
        } else {
            let result = self.0.saturating_sub(other.0);

            Multiplicity(result)
        }
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

impl FromStr for Multiplicity {
    type Err = AcesError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.eq_ignore_ascii_case("omega") || s == "ω" || s == "Ω" {
            Ok(Multiplicity::omega())
        } else {
            match s.parse::<u64>() {
                Ok(value) => Multiplicity::finite(value).ok_or_else(|| {
                    AcesError::from(AcesErrorKind::MultiplicityOverflow("parsing".into()))
                }),
                Err(err) => Err(AcesError::from(AcesErrorKind::from(err))),
            }
        }
    }
}

/// A maximum number of tokens a node may hold.
///
/// This is the type of values of the function _cap<sub>U</sub>_,
/// which maps nodes in the carrier of a c-e structure _U_ to their
/// capacities.
pub type Capacity = Multiplicity;

/// Multiplicity of a node's monomial cause or effect in a firing
/// component.
///
/// This type is used in the representation of the three types of
/// weight labeling.  The implementation stores flow-weight labeling
/// in the variable [`Context::flow_weights`] which maps harcs to
/// [`Weight`]s.  See also [`CoreWeight`] and [`ShellWeight`] for the
/// other two weight labelings.
///
/// [`Context::flow_weights`]: crate::Context::flow_weights
pub type Weight = Multiplicity;

/// Explicit weight attached to an arc of a specific core graph.
///
/// The implementation stores core-weight labeling in the variable
/// [`Context::core_weights`] which maps nodes to sets of
/// [`CoreWeight`]s.  See also [`ShellWeight`].
///
/// [`Context::core_weights`]: crate::Context::core_weights
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct CoreWeight {
    pub(crate) flow_set: FlowSetId,
    pub(crate) weight:   Weight,
}

impl CoreWeight {
    #[inline]
    pub fn new(flow_set: FlowSetId, weight: Weight) -> Self {
        CoreWeight { flow_set, weight }
    }
}

impl ExclusivelyContextual for CoreWeight {
    fn format_locked(&self, ctx: &Context) -> Result<String, AcesError> {
        Ok(format!("{}:{}", self.weight, self.flow_set.format_locked(ctx)?,))
    }
}

/// Generic weight attached to an arc of a shell graph.
///
/// The implementation stores shell-weight labeling in the variable
/// [`Context::shell_weights`] which maps nodes to sets of
/// `ShellWeight`s.  See also [`CoreWeight`].
///
/// [`Context::shell_weights`]: crate::Context::shell_weights
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct ShellWeight {
    pub(crate) pre_set:  NodeSetId,
    pub(crate) post_set: NodeSetId,
    pub(crate) weight:   Weight,
}

impl ShellWeight {
    #[inline]
    pub fn new(pre_set: NodeSetId, post_set: NodeSetId, weight: Weight) -> Self {
        ShellWeight { pre_set, post_set, weight }
    }
}
