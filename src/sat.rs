use std::{
    collections::BTreeSet,
    sync::{Mutex, Arc},
    convert::TryInto,
    fmt::Write,
};
use varisat::{CnfFormula, ExtendFormula, solver::SolverError};
use crate::{Context, Polynomial, Face};

pub use varisat::{Var, Lit};

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

#[derive(Default, Debug)]
pub struct Formula {
    cnf:       CnfFormula,
    variables: BTreeSet<Var>,
}

impl Formula {
    pub fn new() -> Self {
        Default::default()
    }

    fn add_clause(&mut self, clause: &[Lit], info: &str) {
        println!("Add {} clause: {:?}.", info, clause);
        self.cnf.add_clause(clause);
        self.variables.extend(clause.iter().map(|lit| lit.var()));
    }

    pub fn add_internal_node(&mut self, port_lit: Lit, antiport_lit: Lit) {
        let clause = &[port_lit, antiport_lit];

        self.add_clause(clause, "internal node");
    }

    pub fn add_polynomial(&mut self, port_lit: Lit, poly: &Polynomial) {
        for (mono_links, other_links) in poly.iter() {
            let clause = mono_links
                .iter()
                .map(|&id| Lit::from_atom_id(id, false))
                .chain(other_links.iter().map(|&id| Lit::from_atom_id(id, true)));
            let mut clause: Vec<_> = clause.collect();
            clause.push(port_lit);

            self.add_clause(&clause, "monomial");
        }
    }

    fn get_variables(&self) -> &BTreeSet<Var> {
        &self.variables
    }

    pub fn show(&self, ctx: &Arc<Mutex<Context>>) -> String {
        let mut result = String::new();

        let mut first_clause = true;

        for clause in self.cnf.iter() {
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

#[derive(Default)]
pub struct Solver<'a> {
    solver:    varisat::Solver<'a>,
    variables: BTreeSet<Var>,
}

impl<'a> Solver<'a> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn add_formula(&mut self, formula: &Formula) {
        self.solver.add_formula(&formula.cnf);
        self.variables.extend(formula.get_variables());
    }

    pub fn inhibit_empty_solution(&mut self) {
        let clause: Vec<_> = self.variables.iter().map(|&var| Lit::from_var(var, true)).collect();

        self.solver.add_clause(&clause);
    }

    pub fn solve(&mut self) -> Result<bool, SolverError> {
        self.solver.solve()
    }

    pub fn assume(&mut self, assumptions: &[Lit]) {
        self.solver.assume(assumptions);
    }

    pub fn get_model(&self) -> Option<Vec<Lit>> {
        self.solver.model()
    }

    pub fn get_solution(&self, ctx: &Arc<Mutex<Context>>) -> Option<Solution> {
        if let Some(model) = self.solver.model() {
            Some(Solution::from_model(ctx, model))
        } else {
            None
        }
    }
}

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
