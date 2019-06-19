use std::{
    collections::BTreeSet,
    convert::TryInto,
    ops,
    fmt::{self, Write},
    error::Error,
};
use varisat::{Var, Lit, CnfFormula, ExtendFormula, solver::SolverError};
use crate::{Context, ContextHandle, Contextual, Polynomial, PortID, LinkID, node, atom::AtomID};

trait CESVar {
    fn from_atom_id(atom_id: AtomID) -> Self;
    fn into_atom_id(self) -> AtomID;
}

impl CESVar for Var {
    fn from_atom_id(atom_id: AtomID) -> Self {
        Var::from_dimacs(atom_id.get().try_into().unwrap())
    }

    fn into_atom_id(self) -> AtomID {
        let var = self.to_dimacs();
        unsafe { AtomID::new_unchecked(var.try_into().unwrap()) }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct Variable(pub(crate) Var);

impl Variable {
    #[allow(dead_code)]
    pub(crate) fn from_atom_id(atom_id: AtomID) -> Self {
        Self(Var::from_atom_id(atom_id))
    }

    pub(crate) fn into_atom_id(self) -> AtomID {
        self.0.into_atom_id()
    }
}

impl Contextual for Variable {
    fn format(&self, ctx: &Context) -> Result<String, Box<dyn Error>> {
        let mut result = String::new();
        let atom_id = self.into_atom_id();

        if let Some(port) = ctx.get_port(PortID(atom_id)) {
            result.write_fmt(format_args!("{}", port.format(ctx)?))?;
        } else if let Some(link) = ctx.get_link(LinkID(atom_id)) {
            result.write_fmt(format_args!("{}", link.format(ctx)?))?;
        } else {
            result.push_str("???");
        }

        Ok(result)
    }
}

trait CESLit {
    fn from_atom_id(atom_id: AtomID, negated: bool) -> Self;
    fn into_atom_id(self) -> (AtomID, bool);
}

impl CESLit for Lit {
    fn from_atom_id(atom_id: AtomID, negated: bool) -> Self {
        Self::from_var(Var::from_atom_id(atom_id), !negated)
    }

    fn into_atom_id(self) -> (AtomID, bool) {
        let lit = self.to_dimacs();
        unsafe { (AtomID::new_unchecked(lit.abs().try_into().unwrap()), lit < 0) }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct Literal(pub(crate) Lit);

impl Literal {
    pub(crate) fn from_atom_id(atom_id: AtomID, negated: bool) -> Self {
        Self(Lit::from_atom_id(atom_id, negated))
    }

    #[allow(dead_code)]
    pub(crate) fn into_atom_id(self) -> (AtomID, bool) {
        self.0.into_atom_id()
    }

    pub fn is_negative(self) -> bool {
        self.0.is_negative()
    }

    pub fn is_positive(self) -> bool {
        self.0.is_positive()
    }
}

impl ops::Not for Literal {
    type Output = Self;

    fn not(self) -> Self {
        Self(self.0.not())
    }
}

impl ops::BitXor<bool> for Literal {
    type Output = Self;

    fn bitxor(self, rhs: bool) -> Self {
        Self(self.0.bitxor(rhs))
    }
}

impl Contextual for Literal {
    fn format(&self, ctx: &Context) -> Result<String, Box<dyn Error>> {
        if self.is_negative() {
            Ok(format!("~{}", Variable(self.0.var()).format(ctx)?))
        } else {
            Variable(self.0.var()).format(ctx)
        }
    }
}

pub struct Formula {
    context:   ContextHandle,
    cnf:       CnfFormula,
    variables: BTreeSet<Var>,
}

impl Formula {
    pub fn new(ctx: ContextHandle) -> Self {
        Self { context: ctx, cnf: Default::default(), variables: Default::default() }
    }

    // FIXME define `Clause` type
    fn add_clause<'a, I, S>(&mut self, clause: I, info: S)
    where
        I: IntoIterator<Item = &'a Literal>,
        S: AsRef<str>,
    {
        let vc: Vec<_> = clause.into_iter().map(|lit| lit.0).collect();

        println!("Add (to formula) {} clause: {:?}.", info.as_ref(), vc);
        self.cnf.add_clause(vc.as_slice());
        self.variables.extend(vc.iter().map(|lit| lit.var()));
    }

    /// Adds an _antiport_ constraint to this `Formula`.
    ///
    /// This clause constrains nodes to a single part of a firing
    /// component, source or sink, so that the induced graph of any
    /// firing component is bipartite.  The `Formula` should contain
    /// one such clause for each internal node of the c-e structure
    /// under analysis.
    pub fn add_antiport(&mut self, port_id: PortID) {
        let (port_lit, antiport_lit) = {
            if let Some(antiport_id) = self.context.lock().unwrap().get_antiport_id(port_id) {
                (
                    Literal::from_atom_id(port_id.into(), true),
                    Literal::from_atom_id(antiport_id.into(), true),
                )
            } else {
                return // this isn't an internal node
            }
        };

        self.add_clause(&[port_lit, antiport_lit], "internal node");
    }

    /// Adds a _link coherence_ constraint to this `Formula`.
    ///
    /// This constraint consists of two clauses which are added in
    /// order to maintain link coherence, so that any firing component
    /// of a c-e structure is itself a proper c-e structure.  The
    /// `Formula` should contain one such constraint for each link of
    /// the c-e structure under analysis.
    ///
    /// Panics if `link_id` doesn't identify any `Link` in the
    /// `Context` of this `Formula`.
    pub fn add_link_coherence(&mut self, link_id: LinkID) {
        let link_lit = Literal::from_atom_id(link_id.into(), true);

        let (tx_port_lit, rx_port_lit) = {
            let ctx = self.context.lock().unwrap();

            if let Some(link) = ctx.get_link(link_id) {
                let tx_port_id = link.get_tx_port_id();
                let rx_port_id = link.get_rx_port_id();

                (
                    Literal::from_atom_id(tx_port_id.into(), false),
                    Literal::from_atom_id(rx_port_id.into(), false),
                )
            } else {
                panic!("There is no link with ID {:?}.", link_id)
            }
        };
        self.add_clause(&[link_lit, tx_port_lit], "link coherence (Tx side)");
        self.add_clause(&[link_lit, rx_port_lit], "link coherence (Rx side)");
    }

    // FIXME
    /// Adds a _polynomial_ constraint to this formula.
    pub fn add_polynomial(&mut self, _port_id: PortID, poly: &Polynomial) {
        // if poly.is_empty() {
        //     return
        // } else if poly.is_single_link() {
        //     // Consequence is a single positive link literal.  The
        //     // constraint is a single clause:  &[port_lit, link_lit]
        // } else if poly.is_monomial() {
        //     // Consequence is a conjunction of at least two positive
        //     // link literals.  The constraint is a sequence of clauses.
        // } else {
        //     // Consequence consists of at least two clauses, each
        //     // being a conjunction of the same number of positive and
        //     // negative (at least one of each) link literals.
        // }

        for clause in poly.as_sat_clauses() {
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

            let ctx = self.context.lock().unwrap();
            let mut first_lit = true;

            for lit in clause {
                let lit = Literal(*lit);

                if first_lit {
                    first_lit = false;
                } else {
                    write!(f, " | ")?;
                }

                write!(f, "{}", ctx.with(&lit))?;
            }
        }

        Ok(())
    }
}

pub struct Solver<'a> {
    context:   ContextHandle,
    engine:    varisat::Solver<'a>,
    port_vars: BTreeSet<Var>,
}

impl<'a> Solver<'a> {
    pub fn new(ctx: ContextHandle) -> Self {
        Self { context: ctx, engine: Default::default(), port_vars: Default::default() }
    }

    fn add_clause<S: AsRef<str>>(&mut self, clause: &[Lit], info: S) {
        println!("Add (to solver) {} clause: {:?}.", info.as_ref(), clause);
        self.engine.add_clause(clause);
    }

    pub fn add_formula(&mut self, formula: &Formula) {
        self.engine.add_formula(&formula.cnf);

        let all_vars = formula.get_variables();
        let ctx = self.context.lock().unwrap();
        let port_vars = all_vars.iter().filter(|var| ctx.is_port(var.into_atom_id()));

        self.port_vars.extend(port_vars);
    }

    /// Block empty solution models by adding a _void inhibition_
    /// clause.
    ///
    /// A model represents an empty solution iff it contains only
    /// negative [`Port`] literals (hence no [`Port`] variable
    /// evaluates to `true`).  Thus, the blocking clause is the
    /// disjunction of all [`Port`] variables known by the solver.
    ///
    /// [`Port`]: crate::Port
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
            Some(Solution::from_model(self.context.clone(), model))
        } else {
            None
        }
    }
}

pub struct Solution {
    context:  ContextHandle,
    model:    Vec<Lit>,
    pre_set:  Vec<Lit>,
    post_set: Vec<Lit>,
}

impl Solution {
    fn new(ctx: ContextHandle) -> Self {
        Self {
            context:  ctx,
            model:    Default::default(),
            pre_set:  Default::default(),
            post_set: Default::default(),
        }
    }

    fn from_model<I: IntoIterator<Item = Lit>>(ctx: ContextHandle, model: I) -> Self {
        let mut solution = Self::new(ctx);

        for lit in model {
            solution.model.push(lit);

            if lit.is_positive() {
                let (atom_id, _) = lit.into_atom_id();
                let ctx = solution.context.lock().unwrap();

                if let Some(port) = ctx.get_port(PortID(atom_id)) {
                    if port.get_face() == node::Face::Tx {
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

            let ctx = self.context.lock().unwrap();

            for lit in self.pre_set.iter() {
                let lit = Literal(*lit);

                write!(f, " {}", ctx.with(&lit))?;
            }

            write!(f, " }} => {{")?;
        }

        if self.post_set.is_empty() {
            write!(f, "}}")?;
        } else {
            let ctx = self.context.lock().unwrap();

            for lit in self.post_set.iter() {
                let lit = Literal(*lit);

                write!(f, " {}", ctx.with(&lit))?;
            }

            write!(f, " }}")?;
        }

        Ok(())
    }
}
