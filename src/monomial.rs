/// Multiplicity of a monomial when attached to a node.
///
/// Valid weights are nonnegative integers or _&omega;_ (inhibiting
/// weight).  The default value is 1.
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
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct Weight(u64);

impl Weight {
    pub fn new_omega() -> Self {
        Weight(u64::max_value())
    }

    pub fn new_finite(value: u64) -> Option<Self> {
        if value < u64::max_value() {
            Some(Weight(value))
        } else {
            None
        }
    }

    pub fn is_omega(self) -> bool {
        self.0 == u64::max_value()
    }

    pub fn is_finite(self) -> bool {
        self.0 < u64::max_value()
    }
}

impl Default for Weight {
    fn default() -> Self {
        Weight(1)
    }
}
