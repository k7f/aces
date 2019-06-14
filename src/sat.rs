use std::{
    collections::BTreeSet,
    sync::{Mutex, Arc},
    convert::TryInto,
    fmt::{self, Write},
};
use varisat::{Var, Lit, CnfFormula, ExtendFormula, solver::SolverError};
use crate::{Context, Polynomial, Face, PortID, LinkID, atom::AtomID};

trait CESVar {
    fn from_atom_id(atom_id: AtomID) -> Self;
    fn into_atom_id(&self) -> AtomID;
    fn show(&self, ctx: &Arc<Mutex<Context>>) -> String;
}

impl CESVar for Var {
    fn from_atom_id(atom_id: AtomID) -> Self {
        Var::from_dimacs(atom_id.get().try_into().unwrap())
    }

    fn into_atom_id(&self) -> AtomID {
        let var = self.to_dimacs();
        unsafe {
            AtomID::new_unchecked(var.try_into().unwrap())
        }
    }

    fn show(&self, ctx: &Arc<Mutex<Context>>) -> String {
        let mut result = String::new();
        let atom_id = self.into_atom_id();

        let ctx = ctx.lock().unwrap();

        if let Some(port) = ctx.get_port(PortID(atom_id)) {
            result.write_fmt(format_args!("{}", port)).unwrap();
        } else if let Some(link) = ctx.get_link(LinkID(atom_id)) {
            result.write_fmt(format_args!("{}", link)).unwrap();
        } else {
            result.push_str("???");
        }

        result
    }
}

trait CESLit {
    fn from_atom_id(atom_id: AtomID, negated: bool) -> Self;
    fn into_atom_id(&self) -> (AtomID, bool);
    fn show(&self, ctx: &Arc<Mutex<Context>>) -> String;
}

impl CESLit for Lit {
    fn from_atom_id(atom_id: AtomID, negated: bool) -> Self {
        Self::from_var(Var::from_atom_id(atom_id), !negated)
    }

    fn into_atom_id(&self) -> (AtomID, bool) {
        let lit = self.to_dimacs();
        unsafe {
            (AtomID::new_unchecked(lit.abs().try_into().unwrap()), lit < 0)
        }
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
    pub(crate) fn from_atom_id(atom_id: AtomID, negated: bool) -> Self {
        Self(Lit::from_atom_id(atom_id, negated))
    }

    #[allow(dead_code)]
    pub(crate) fn into_atom_id(&self) -> (AtomID, bool) {
        self.0.into_atom_id()
    }

    pub fn show(&self, ctx: &Arc<Mutex<Context>>) -> String {
        self.0.show(ctx)
    }
}

pub struct Formula {
    context:   Arc<Mutex<Context>>,
    cnf:       CnfFormula,
    variables: BTreeSet<Var>,
}

impl Formula {
    pub fn new(ctx: &Arc<Mutex<Context>>) -> Self {
        Self {
            context:   Arc::clone(ctx),
            cnf:       Default::default(),
            variables: Default::default(),
        }
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
                .map(|&id| Lit::from_atom_id(id.0, false))
                .chain(other_links.iter().map(|&id| Lit::from_atom_id(id.0, true)));
            let mut clause: Vec<_> = clause.collect();
            clause.push(port_lit.0);

            self.add_clause(&clause, "monomial");
        }
    }

    fn get_variables(&self) -> &BTreeSet<Var> {
        &self.variables
    }
}

impl fmt::Debug for Formula {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Formula {{ cnf: {:?}, variables: {:?} }}", self.cnf, self.variables)
    }
}

impl fmt::Display for Formula {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut first_clause = true;

        for clause in self.cnf.iter() {
            if first_clause {
                first_clause = false;
            } else {
                write!(f, " /^\\ ")?;
            }

            let mut first_lit = true;

            for lit in clause {
                if first_lit {
                    first_lit = false;
                } else {
                    write!(f, " | ")?;
                }

                write!(f, "{}", &lit.show(&self.context))?;
            }
        }

        Ok(())
    }
}

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

pub struct Solution {
    context:  Arc<Mutex<Context>>,
    model:    Vec<Lit>,
    pre_set:  Vec<Lit>,
    post_set: Vec<Lit>,
}

impl Solution {
    fn new(ctx: &Arc<Mutex<Context>>) -> Self {
        Self {
            context:  Arc::clone(ctx),
            model:    Default::default(),
            pre_set:  Default::default(),
            post_set: Default::default(),
        }
    }

    fn from_model<I: IntoIterator<Item = Lit>>(ctx: &Arc<Mutex<Context>>, model: I) -> Self {
        let mut solution = Self::new(ctx);

        for lit in model {
            solution.model.push(lit);

            if lit.is_positive() {
                let (atom_id, _) = lit.into_atom_id();
                let ctx = solution.context.lock().unwrap();

                if let Some(port) = ctx.get_port(PortID(atom_id)) {
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
}

impl fmt::Debug for Solution {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Solution {{ model: {:?}, pre_set: {:?}, post_set: {:?} }}",
            self.model, self.pre_set, self.post_set
        )
    }
}

impl fmt::Display for Solution {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.pre_set.is_empty() {
            write!(f, "{{}} => {{")?;
        } else {
            write!(f, "{{")?;

            for lit in self.pre_set.iter() {
                write!(f, " {}", &lit.show(&self.context))?;
            }

            write!(f, " }} => {{")?;
        }

        if self.post_set.is_empty() {
            write!(f, "}}")?;
        } else {
            for lit in self.post_set.iter() {
                write!(f, " {}", &lit.show(&self.context))?;
            }

            write!(f, " }}")?;
        }

        Ok(())
    }
}
