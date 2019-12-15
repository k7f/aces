use std::fmt::{self, Write};

/// A scalar type common for node capacity, split weight and state.
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

/// A maximum number of tokens a node may hold.
///
/// This is the type of values of the function _cap<sub>U</sub>_,
/// which maps nodes in the carrier of a c-e structure _U_ to their
/// capacities.
pub type Capacity = Multiplicity;

/// Multiplicity of a split (a monomial when attached to a node).
///
/// Weight of a monomial occurring in effects of a node indicates the
/// number of tokens to take out of that node if a corresponding
/// transaction fires.  Weight of a monomial occurring in causes of a
/// node indicates the number of tokens to put into that node if a
/// corresponding transaction fires.  Accordingly, the weights imply
/// constraints a state must satisfy for a transaction to fire: a
/// minimal number of tokens to be supplied by pre-set nodes, so that
/// token transfer may happen, and a maximal number of tokens in
/// post-set nodes, so that node capacities aren't exceeded.  The
/// _&omega;_ weight attached to effects of a node indicates that the
/// presence of any token in that node inhibits firing.
pub type Weight = Multiplicity;
