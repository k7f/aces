use crate::Multiplicity;

/// Multiplicity of a monomial when attached to a node.
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
