use std::{collections::BTreeSet, convert::TryInto};
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
    fn add_polynomial(&mut self, port_lit: Lit, poly: &Vec<Vec<usize>>);
}

impl AddPolynomial for CnfFormula {
    fn add_polynomial(&mut self, port_lit: Lit, poly: &Vec<Vec<usize>>) {
        // FIXME in-poly, updated during construction
        let mut all_links = BTreeSet::new();
        for mono in poly {
            for &link_id in mono {
                all_links.insert(link_id);
            }
        }
        
        for mono in poly {
            let mono_links: BTreeSet<_> = mono.iter().map(|&id| id).collect();
            let other_links = all_links.difference(&mono_links);
            let clause = mono_links.iter()
                .map(|&id| Lit::from_atom_id(id, false))
                .chain(
                    other_links
                        .map(|&id| Lit::from_atom_id(id, true)));
            let mut clause: Vec<_> = clause.collect();
            clause.push(port_lit);

            println!("add mono clause {:?}", clause);
            self.add_clause(&clause);
        }
    }
}
