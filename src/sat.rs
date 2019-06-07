use std::convert::TryInto;
pub use varisat::{CnfFormula, ExtendFormula, Var, Lit};

pub(crate) trait FromAtomID {
    fn from_atom_id(atom_id: usize, negated: bool) -> Self;
}

impl FromAtomID for Lit {
    fn from_atom_id(atom_id: usize, negated: bool) -> Self {
        Self::from_var(Var::from_dimacs(atom_id.try_into().unwrap()), negated)
    }
}

pub(crate) trait AddPolynomial {
    fn add_polynomial(&mut self, port_lit: Lit, poly: &Vec<Vec<usize>>);
}

impl AddPolynomial for CnfFormula {
    fn add_polynomial(&mut self, port_lit: Lit, poly: &Vec<Vec<usize>>) {
    }
}
