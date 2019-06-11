use std::convert::TryInto;
use crate::Polynomial;

pub use varisat::{Solver, CnfFormula, ExtendFormula, Var, Lit};

pub(crate) trait FromAtomID {
    fn from_atom_id(atom_id: usize, negated: bool) -> Self;
}

impl FromAtomID for Lit {
    fn from_atom_id(atom_id: usize, negated: bool) -> Self {
        Self::from_var(Var::from_dimacs(atom_id.try_into().unwrap()), negated)
    }
}

pub(crate) trait IntoAtomID {
    fn into_atom_id(&self) -> (usize, bool);
}

impl IntoAtomID for Lit {
    fn into_atom_id(&self) -> (usize, bool) {
        let lit = self.to_dimacs();
        (lit.abs().try_into().unwrap(), lit > 0)
    }
}

pub(crate) trait AddPolynomial {
    fn add_polynomial(&mut self, port_lit: Lit, poly: &Polynomial);
}

impl AddPolynomial for CnfFormula {
    fn add_polynomial(&mut self, port_lit: Lit, poly: &Polynomial) {
        for (mono_links, other_links) in poly.iter() {

            let clause = mono_links.iter()
                .map(|&id| Lit::from_atom_id(id, false))
                .chain(
                    other_links.iter()
                        .map(|&id| Lit::from_atom_id(id, true)));
            let mut clause: Vec<_> = clause.collect();
            clause.push(port_lit);

            println!("add mono clause {:?}", clause);
            self.add_clause(&clause);
        }
    }
}
