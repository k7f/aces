use std::{sync::{Mutex, Arc}, convert::TryInto, fmt::Write};
use varisat::ExtendFormula;
use crate::{Context, Polynomial, Face};

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

pub(crate) trait CESModel {
    fn show(&self, ctx: &Arc<Mutex<Context>>) -> String;
}

impl CESModel for [Lit] {
    fn show(&self, ctx: &Arc<Mutex<Context>>) -> String {
        let mut solution = (Vec::new(), Vec::new());
        let ctx = ctx.lock().unwrap();
        for lit in self {
            if lit.is_positive() {
                let (atom_id, _) = lit.into_atom_id();
                if let Some(port) = ctx.get_port(atom_id) {
                    if port.get_face() == Face::Tx {
                        solution.0.push(port);
                    } else {
                        solution.1.push(port);
                    }
                }
            }
        }

        let mut result = String::new();

        if solution.0.is_empty() {
            result.push_str("{} => {");
        } else {
            result.push('{');
            for atom in solution.0 {
                result.write_fmt(format_args!(" {}", atom)).unwrap();
            }
            result.push_str(" } => {");
        }
        if solution.1.is_empty() {
            result.push('}');
        } else {
            for atom in solution.1 {
                result.write_fmt(format_args!(" {}", atom)).unwrap();
            }
            result.push_str(" }");
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

    fn show(&self, ctx: &Arc<Mutex<Context>>) -> String {
        let mut result = String::new();
        let ctx = ctx.lock().unwrap();

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

                let (atom_id, is_negated) = lit.into_atom_id();

                if is_negated {
                    result.push('~');
                }

                if let Some(port) = ctx.get_port(atom_id) {
                    result.push_str(&format!("{}", port));
                } else if let Some(link) = ctx.get_link(atom_id) {
                    result.push_str(&format!("{}", link));
                } else {
                    result.push_str("???");
                }
            }
        }

        result
    }
}
