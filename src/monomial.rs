/// Multiplicity of a monomial.
///
/// Valid weights are positive integers or _&omega;_ (inhibiting
/// weight).  The default value is 1.
///
/// Weight attached to a monomial in effects of a node indicates the
/// number of tokens to take out of that node if a corresponding
/// transaction fires.  Weight attached to a monomial in causes of a
/// node indicates the number of tokens to put into that node if a
/// corresponding transaction fires.  Accordingly, the weights imply
/// constraints a state must satisfy for a transaction to fire: a
/// minimal number of tokens to be supplied by pre-set nodes, so that
/// token transfer may happen, and a maximal number of tokens in
/// post-set nodes, so that node capacities aren't exceeded.  The
/// _&omega;_ weight attached to effects of a node indicates that any
/// token in that node inhibits firing.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct Weight(u64);

impl Weight {
    pub fn new_omega() -> Self {
        Weight(0)
    }

    pub fn new_finite(value: u64) -> Option<Self> {
        if value > 0 {
            Some(Weight(value))
        } else {
            None
        }
    }

    pub fn is_omega(self) -> bool {
        self.0 == 0
    }

    pub fn is_finite(self) -> bool {
        self.0 > 0
    }
}

impl Default for Weight {
    fn default() -> Self {
        Weight(1)
    }
}
