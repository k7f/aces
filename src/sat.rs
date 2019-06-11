use std::convert::TryInto;
use varisat::ExtendFormula;
use crate::Polynomial;

pub use varisat::{Solver, CnfFormula, Var, Lit};

pub(crate) trait CESLit {
    fn from_atom_id(atom_id: usize, negated: bool) -> Self;
    fn into_atom_id(&self) -> (usize, bool);
}

impl CESLit for Lit {
    fn from_atom_id(atom_id: usize, negated: bool) -> Self {
        Self::from_var(Var::from_dimacs(atom_id.try_into().unwrap()), negated)
    }

    fn into_atom_id(&self) -> (usize, bool) {
        let lit = self.to_dimacs();
        (lit.abs().try_into().unwrap(), lit > 0)
    }
}

pub(crate) trait CESFormula {
    fn add_internal_node(&mut self, port_lit: Lit, antiport_lit: Lit);
    fn add_polynomial(&mut self, port_lit: Lit, poly: &Polynomial);
}

impl CESFormula for CnfFormula {
    fn add_internal_node(&mut self, port_lit: Lit, antiport_lit: Lit) {
        let clause = &[port_lit, antiport_lit];

        println!("add internal node clause {:?}", clause);
        self.add_clause(clause);
    }

    fn add_polynomial(&mut self, port_lit: Lit, poly: &Polynomial) {
        for (mono_links, other_links) in poly.iter() {

            let clause = mono_links.iter()
                .map(|&id| Lit::from_atom_id(id, false))
                .chain(
                    other_links.iter()
                        .map(|&id| Lit::from_atom_id(id, true)));
            let mut clause: Vec<_> = clause.collect();
            clause.push(port_lit);

            println!("add monomial clause {:?}", clause);
            self.add_clause(&clause);
        }
    }
}
