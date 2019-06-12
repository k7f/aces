use std::{
    sync::{Mutex, Arc},
    convert::TryInto,
    fmt::Write,
};
use varisat::ExtendFormula;
use crate::{Context, Polynomial, Face};

pub use varisat::{Solver, CnfFormula, Var, Lit};

#[derive(Default, Debug)]
pub struct Solution {
    pre_set:  Vec<Lit>,
    post_set: Vec<Lit>,
}

impl Solution {
    pub fn from_model<I: IntoIterator<Item = Lit>>(ctx: &Arc<Mutex<Context>>, model: I) -> Self {
        let mut solution = Self::default();
        let ctx = ctx.lock().unwrap();

        for lit in model {
            if lit.is_positive() {
                let (atom_id, _) = lit.into_atom_id();
                if let Some(port) = ctx.get_port(atom_id) {
                    if port.get_face() == Face::Tx {
                        solution.pre_set.push(lit);
                    } else {
                        solution.post_set.push(lit);
                    }
                }
            }
        }

        solution
    }

    pub fn show(&self, ctx: &Arc<Mutex<Context>>) -> String {
        let mut result = String::new();

        if self.pre_set.is_empty() {
            result.push_str("{} => {");
        } else {
            result.push('{');
            for lit in self.pre_set.iter() {
                result.push(' ');
                result.push_str(&lit.show(ctx));
            }
            result.push_str(" } => {");
        }
        if self.post_set.is_empty() {
            result.push('}');
        } else {
            for lit in self.post_set.iter() {
                result.push(' ');
                result.push_str(&lit.show(ctx));
            }
            result.push_str(" }");
        }

        result
    }
}

pub(crate) trait CESLit {
    fn from_atom_id(atom_id: usize, negated: bool) -> Self;
    fn into_atom_id(&self) -> (usize, bool);
    fn show(&self, ctx: &Arc<Mutex<Context>>) -> String;
}

impl CESLit for Lit {
    fn from_atom_id(atom_id: usize, negated: bool) -> Self {
        Self::from_var(Var::from_dimacs(atom_id.try_into().unwrap()), negated)
    }

    fn into_atom_id(&self) -> (usize, bool) {
        let lit = self.to_dimacs();
        (lit.abs().try_into().unwrap(), lit > 0)
    }

    fn show(&self, ctx: &Arc<Mutex<Context>>) -> String {
        let mut result = String::new();
        let (atom_id, is_negated) = self.into_atom_id();

        if is_negated {
            result.push('~');
        }

        let ctx = ctx.lock().unwrap();

        if let Some(port) = ctx.get_port(atom_id) {
            result.write_fmt(format_args!("{}", port)).unwrap();
        } else if let Some(link) = ctx.get_link(atom_id) {
            result.write_fmt(format_args!("{}", link)).unwrap();
        } else {
            result.push_str("???");
        }

        result
    }
}

pub(crate) trait CESFormula {
    fn add_internal_node(&mut self, port_lit: Lit, antiport_lit: Lit);
    fn add_polynomial(&mut self, port_lit: Lit, poly: &Polynomial);
    fn show(&self, ctx: &Arc<Mutex<Context>>) -> String;
}

impl CESFormula for CnfFormula {
    fn add_internal_node(&mut self, port_lit: Lit, antiport_lit: Lit) {
        let clause = &[port_lit, antiport_lit];

        println!("add internal node clause {:?}", clause);
        self.add_clause(clause);
    }

    fn add_polynomial(&mut self, port_lit: Lit, poly: &Polynomial) {
        for (mono_links, other_links) in poly.iter() {
            let clause = mono_links
                .iter()
                .map(|&id| Lit::from_atom_id(id, false))
                .chain(other_links.iter().map(|&id| Lit::from_atom_id(id, true)));
            let mut clause: Vec<_> = clause.collect();
            clause.push(port_lit);

            println!("add monomial clause {:?}", clause);
            self.add_clause(&clause);
        }
    }

    fn show(&self, ctx: &Arc<Mutex<Context>>) -> String {
        let mut result = String::new();

        let mut first_clause = true;

        for clause in self.iter() {
            if first_clause {
                first_clause = false;
            } else {
                result.push_str(" /^\\ ");
            }

            let mut first_lit = true;

            for lit in clause {
                if first_lit {
                    first_lit = false;
                } else {
                    result.push_str(" || ");
                }

                result.push_str(&lit.show(ctx));
            }
        }

        result
    }
}
