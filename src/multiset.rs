use std::{
    str::FromStr,
    fmt::{self, Write},
};
use crate::{NodeID, AtomID, AcesError, AcesErrorKind};

/// A scalar type common for node capacity, harc weight and state.
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

/// Multiplicity of node in a firing component.
///
/// A finite weight of node occurring in the pre-set of a transaction
/// indicates the number of tokens to take out of that node if a
/// transaction fires.  A finite weight of a node occurring in the
/// post-set of a transaction indicates the number of tokens to put
/// into that node if a transaction fires.
///
/// The finite positive weights imply constraints a state must satisfy
/// for a transaction to fire: a minimal number of tokens to be
/// supplied by pre-set nodes, so that token transfer may happen, and
/// a maximal number of tokens in post-set nodes, so that node
/// capacities aren't exceeded.
///
/// If weight zero is attached to a pre-set node, then tokens aren't
/// taken out when a transaction fires.  However, a transaction never
/// fires, unless there is a token in that node.  Another special case
/// is the _&omega;_ weight attached to a pre-set node, which
/// indicates that the presence of a token in that node inhibits
/// firing.
pub type Weight = Multiplicity;

/// Generic weight of monomial causes or effects of a node.
///
/// `HyperWeight`s represent the weight labeling used in standard
/// notation for polynomials.  Conceptually, they are attached to arcs
/// of a BF-hypergraph (hyperarcs _aka_ harcs), and inherited by all
/// corresponding arcs of the induced flow relation.
///
/// See [`FlowWeight`] for implementation details.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct HyperWeight {
    pub(crate) harc_id: AtomID,
    pub(crate) weight:  Weight,
}

impl HyperWeight {
    #[inline]
    pub fn new(harc_id: AtomID, weight: Weight) -> Self {
        HyperWeight { harc_id, weight }
    }
}

/// Explicit weight attached to an arc of a flow relation.
///
/// Flow relation is a bijective digraph of nodes and transitions.
/// This type represents a pair _(transition, weight)_ as a triple
/// _(pre-set, post-set, weight)_.
///
/// The implementation stores generic weights in the [`Context`]
/// variable [`hyper_weights`] _and_ stores directly specified weights
/// of individual arcs in the [`Context`] variable [`flow_weights`],
/// which maps nodes to sets of `FlowWeight`s.
///
/// Note that, in general, hyperarc nodesets are contained in (not
/// equal to) pre-sets or post-sets of corresponding transitions.
///
/// [`Context`]: crate::Context
/// [`hyper_weights`]: crate::Context::hyper_weights
/// [`flow_weights`]: crate::Context::flow_weights
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct FlowWeight {
    pub(crate) pre_set:  Vec<NodeID>,
    pub(crate) post_set: Vec<NodeID>,
    pub(crate) weight:   Weight,
}

impl FlowWeight {
    #[inline]
    pub fn new(pre_set: Vec<NodeID>, post_set: Vec<NodeID>, weight: Weight) -> Self {
        FlowWeight { pre_set, post_set, weight }
    }
}
