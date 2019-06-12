use std::{
    collections::BTreeSet,
    sync::{Mutex, Arc},
    convert::TryInto,
    fmt::Write,
};
use varisat::{Var, Lit, CnfFormula, ExtendFormula, solver::SolverError};
use crate::{Context, Polynomial, Face};

trait CESVar {
    fn from_atom_id(atom_id: usize) -> Self;
    fn into_atom_id(&self) -> usize;
    fn show(&self, ctx: &Arc<Mutex<Context>>) -> String;
}

impl CESVar for Var {
    fn from_atom_id(atom_id: usize) -> Self {
        Var::from_dimacs(atom_id.try_into().unwrap())
    }

    fn into_atom_id(&self) -> usize {
        let var = self.to_dimacs();
        var.try_into().unwrap()
    }

    fn show(&self, ctx: &Arc<Mutex<Context>>) -> String {
        let mut result = String::new();
        let atom_id = self.into_atom_id();

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

trait CESLit {
    fn from_atom_id(atom_id: usize, negated: bool) -> Self;
    fn into_atom_id(&self) -> (usize, bool);
    fn show(&self, ctx: &Arc<Mutex<Context>>) -> String;
}

impl CESLit for Lit {
    fn from_atom_id(atom_id: usize, negated: bool) -> Self {
        Self::from_var(Var::from_atom_id(atom_id), !negated)
    }

    fn into_atom_id(&self) -> (usize, bool) {
        let lit = self.to_dimacs();
        (lit.abs().try_into().unwrap(), lit < 0)
    }

    fn show(&self, ctx: &Arc<Mutex<Context>>) -> String {
        if self.is_negative() {
            format!("~{}", self.var().show(ctx))
        } else {
            self.var().show(ctx)
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Literal(Lit);

impl Literal {
    pub fn from_atom_id(atom_id: usize, negated: bool) -> Self {
        Self(Lit::from_atom_id(atom_id, negated))
    }

    pub fn into_atom_id(&self) -> (usize, bool) {
        self.0.into_atom_id()
    }

    pub fn show(&self, ctx: &Arc<Mutex<Context>>) -> String {
        self.0.show(ctx)
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
        println!("Add (to formula) {} clause: {:?}.", info, clause);
        self.cnf.add_clause(clause);
        self.variables.extend(clause.iter().map(|lit| lit.var()));
    }

    pub fn add_internal_node(&mut self, port_lit: Literal, antiport_lit: Literal) {
        let clause = &[port_lit.0, antiport_lit.0];

        self.add_clause(clause, "internal node");
    }

    // FIXME
    pub fn add_polynomial(&mut self, port_lit: Literal, poly: &Polynomial) {
        for (mono_links, other_links) in poly.iter() {
            let clause = mono_links
                .iter()
                .map(|&id| Lit::from_atom_id(id, false))
                .chain(other_links.iter().map(|&id| Lit::from_atom_id(id, true)));
            let mut clause: Vec<_> = clause.collect();
            clause.push(port_lit.0);

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
                    result.push_str(" | ");
                }

                result.push_str(&lit.show(ctx));
            }
        }

        result
    }
}

#[derive(Default)]
pub struct Solver<'a> {
    context:   Arc<Mutex<Context>>,
    engine:    varisat::Solver<'a>,
    port_vars: BTreeSet<Var>,
}

impl<'a> Solver<'a> {
    pub fn new(ctx: &Arc<Mutex<Context>>) -> Self {
        Self {
            context:   Arc::clone(ctx),
            engine:    Default::default(),
            port_vars: Default::default(),
        }
    }

    fn add_clause(&mut self, clause: &[Lit], info: &str) {
        println!("Add (to solver) {} clause: {:?}.", info, clause);
        self.engine.add_clause(clause);
    }

    pub fn add_formula(&mut self, formula: &Formula) {
        self.engine.add_formula(&formula.cnf);

        let all_vars = formula.get_variables();
        let ctx = self.context.lock().unwrap();
        let port_vars = all_vars.iter().filter(|var| ctx.is_port(var.into_atom_id()));

        self.port_vars.extend(port_vars);
    }

    pub fn inhibit_empty_solution(&mut self) {
        let clause: Vec<_> = self.port_vars.iter().map(|&var| Lit::from_var(var, true)).collect();

        self.add_clause(&clause, "void inhibition");
    }

    pub fn solve(&mut self) -> Result<bool, SolverError> {
        self.engine.solve()
    }

    // fn assume(&mut self, assumptions: &[Lit]) {
    //     self.engine.assume(assumptions);
    // }

    pub fn get_solution(&self) -> Option<Solution> {
        if let Some(model) = self.engine.model() {
            Some(Solution::from_model(&self.context, model))
        } else {
            None
        }
    }
}

#[derive(Default, Debug)]
pub struct Solution {
    model:    Vec<Lit>,
    pre_set:  Vec<Lit>,
    post_set: Vec<Lit>,
}

impl Solution {
    fn from_model<I: IntoIterator<Item = Lit>>(ctx: &Arc<Mutex<Context>>, model: I) -> Self {
        let mut solution = Self::default();
        let ctx = ctx.lock().unwrap();

        for lit in model {
            solution.model.push(lit);

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
