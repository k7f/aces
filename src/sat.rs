use std::{
    collections::BTreeSet,
    convert::TryInto,
    ops,
    fmt::{self, Write},
    error::Error,
};
use log::Level::Debug;
use varisat::{Var, Lit, CnfFormula, ExtendFormula, solver::SolverError};
use crate::{
    Atomic, Context, ContextHandle, Contextual, Polynomial, PortID, LinkID, node, atom::AtomID,
};

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
    fn format(&self, ctx: &Context, dock: Option<node::Face>) -> Result<String, Box<dyn Error>> {
        let mut result = String::new();
        let atom_id = self.into_atom_id();

        if let Some(port) = ctx.get_port(PortID(atom_id)) {
            result.write_fmt(format_args!("{}", port.format(ctx, dock)?))?;
        } else if let Some(link) = ctx.get_link(LinkID(atom_id)) {
            result.write_fmt(format_args!("{}", link.format(ctx, dock)?))?;
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
    pub(crate) fn from_variable(var: Variable, negated: bool) -> Self {
        Literal(Lit::from_var(var.0, !negated))
    }

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

    pub(crate) fn into_variable_if_positive(self) -> Option<Variable> {
        if self.is_positive() {
            Some(Variable(self.0.var()))
        } else {
            None
        }
    }

    pub(crate) fn into_variable_if_negative(self) -> Option<Variable> {
        if self.is_negative() {
            Some(Variable(self.0.var()))
        } else {
            None
        }
    }
}

impl From<Lit> for Literal {
    fn from(lit: Lit) -> Self {
        Literal(lit)
    }
}

impl From<Literal> for Lit {
    fn from(lit: Literal) -> Self {
        lit.0
    }
}

impl From<&Literal> for Lit {
    fn from(lit: &Literal) -> Self {
        lit.0
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
    fn format(&self, ctx: &Context, dock: Option<node::Face>) -> Result<String, Box<dyn Error>> {
        if self.is_negative() {
            Ok(format!("~{}", Variable(self.0.var()).format(ctx, dock)?))
        } else {
            Variable(self.0.var()).format(ctx, dock)
        }
    }
}

pub struct Clause {
    lits: Vec<Lit>,
    info: String,
}

impl Clause {
    pub fn new<I, S>(literals: I, info: S) -> Self
    where
        I: IntoIterator + Clone,
        I::Item: Into<Lit>,
        S: AsRef<str>,
    {
        let lits = literals.into_iter().map(|lit| lit.into()).collect();
        let info = info.as_ref().to_owned();

        Clause { lits, info }
    }

    pub fn from_pair<L1, L2, S>(lit1: L1, lit2: L2, info: S) -> Self
    where
        L1: Into<Lit>,
        L2: Into<Lit>,
        S: AsRef<str>,
    {
        let lits = vec![lit1.into(), lit2.into()];
        let info = info.as_ref().to_owned();

        Clause { lits, info }
    }
}

impl Contextual for Clause {
    fn format(&self, ctx: &Context, dock: Option<node::Face>) -> Result<String, Box<dyn Error>> {
        let mut liter = self.lits.iter();

        if let Some(lit) = liter.next() {
            let mut litxt = Literal(*lit).format(ctx, dock)?;

            for lit in liter {
                litxt.push_str(&format!(", {}", ctx.with(&Literal(*lit))));
            }

            Ok(format!("{{{}}}", litxt))
        } else {
            Ok("{{}}".to_owned())
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

    fn add_clause(&mut self, clause: Clause) {
        if clause.lits.is_empty() {
            debug!("Empty {} clause, not added to formula", clause.info);
        } else {
            if log_enabled!(Debug) {
                let ctx = self.context.lock().unwrap();
                debug!("Add (to formula) {} clause: {}", clause.info, ctx.with(&clause))
            }

            self.cnf.add_clause(clause.lits.as_slice());
            self.variables.extend(clause.lits.iter().map(|lit| lit.var()));
        }
    }

    /// Adds an _antiport_ rule to this `Formula`.
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
                    Lit::from_atom_id(port_id.into(), true),
                    Lit::from_atom_id(antiport_id.into(), true),
                )
            } else {
                return // this isn't an internal node
            }
        };

        let clause = Clause::from_pair(port_lit, antiport_lit, "internal node");
        self.add_clause(clause);
    }

    /// Adds a _link coherence_ rule to this `Formula`.
    ///
    /// This firing rule consists of two clauses which are added in
    /// order to maintain link coherence, so that any firing component
    /// of a c-e structure is itself a proper c-e structure.  The
    /// `Formula` should contain one such rule for each link of the
    /// c-e structure under analysis.
    ///
    /// Panics if `link_id` doesn't identify any `Link` in the
    /// `Context` of this `Formula`.
    pub fn add_link_coherence(&mut self, link_id: LinkID) {
        let link_lit = Lit::from_atom_id(link_id.into(), true);

        let (tx_port_lit, rx_port_lit) = {
            let ctx = self.context.lock().unwrap();

            if let Some(link) = ctx.get_link(link_id) {
                let tx_port_id = link.get_tx_port_id();
                let rx_port_id = link.get_rx_port_id();

                (
                    Lit::from_atom_id(tx_port_id.into(), false),
                    Lit::from_atom_id(rx_port_id.into(), false),
                )
            } else {
                panic!("There is no link with ID {:?}.", link_id)
            }
        };

        let clause = Clause::from_pair(link_lit, tx_port_lit, "link coherence (Tx side)");
        self.add_clause(clause);

        let clause = Clause::from_pair(link_lit, rx_port_lit, "link coherence (Rx side)");
        self.add_clause(clause);
    }

    /// Adds a _polynomial_ rule to this formula.
    pub fn add_polynomial(&mut self, port_id: PortID, poly: &Polynomial<LinkID>) {
        if !poly.is_empty() {
            let port_lit = port_id.into_sat_literal(true);

            for clause in poly.as_sat_clauses(port_lit) {
                self.add_clause(clause);
            }
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
    context:      ContextHandle,
    engine:       varisat::Solver<'a>,
    port_vars:    BTreeSet<Var>,
    is_sat:       Option<bool>,
    last_result:  Option<Result<bool, SolverError>>,
    only_minimal: bool,
    blocked_vars: BTreeSet<Var>,
}

impl<'a> Solver<'a> {
    pub fn new(ctx: ContextHandle) -> Self {
        Self {
            context:      ctx,
            engine:       Default::default(),
            port_vars:    Default::default(),
            is_sat:       None,
            last_result:  None,
            only_minimal: false,
            blocked_vars: Default::default(),
        }
    }

    pub fn reset(&mut self) -> Result<(), SolverError> {
        self.is_sat = None;
        self.last_result = None;
        self.blocked_vars.clear();
        self.engine.close_proof()
    }

    pub fn set_minimal_mode(&mut self, only_minimal: bool) {
        self.only_minimal = only_minimal;
    }

    fn add_clause(&mut self, clause: Clause) {
        if clause.lits.is_empty() {
            debug!("Empty {} clause, not added to solver", clause.info);
        } else {
            if log_enabled!(Debug) {
                let ctx = self.context.lock().unwrap();
                debug!("Add (to solver) {} clause: {}", clause.info, ctx.with(&clause));
            }

            self.engine.add_clause(clause.lits.as_slice());
        }
    }

    fn add_anti_clause(&mut self, model: &[Lit]) {
        let anti_clause: Vec<_> = model.iter().map(|&lit| !lit).collect();

        self.engine.add_clause(anti_clause.as_slice());
    }

    fn add_blocking_clause(&mut self, model: &[Lit]) {
        let mut blocking_clause = Vec::new();

        for &lit in model.iter() {
            if lit.is_positive() && !self.blocked_vars.contains(&lit.var()) {
                self.blocked_vars.insert(lit.var());
            } else {
                blocking_clause.push(!lit);
                self.blocked_vars.remove(&lit.var());
            }
        }

        if !blocking_clause.is_empty() {
            self.engine.add_clause(blocking_clause.as_slice());
        }
    }

    pub fn add_formula(&mut self, formula: &Formula) {
        self.engine.add_formula(&formula.cnf);

        let all_vars = formula.get_variables();
        let ctx = self.context.lock().unwrap();
        let port_vars = all_vars.iter().filter(|var| ctx.is_port(var.into_atom_id()));

        self.port_vars.extend(port_vars);
    }

    /// Blocks empty solution models by adding a _void inhibition_
    /// clause.
    ///
    /// A model represents an empty solution iff it contains only
    /// negative [`Port`] literals (hence no [`Port`] variable
    /// evaluates to `true`).  Thus, the blocking clause is the
    /// disjunction of all [`Port`] variables known by the solver.
    ///
    /// [`Port`]: crate::Port
    pub fn inhibit_empty_solution(&mut self) {
        let lits = self.port_vars.iter().map(|&var| Lit::from_var(var, true));
        let clause = Clause::new(lits, "void inhibition");

        self.add_clause(clause);
    }

    // fn assume(&mut self, assumptions: &[Lit]) {
    //     self.engine.assume(assumptions);
    // }

    pub fn solve(&mut self) -> Option<bool> {
        let result = self.engine.solve();

        let is_sat = result.as_ref().ok().copied();

        if self.is_sat.is_none() {
            self.is_sat = is_sat;
        }
        self.last_result = Some(result);

        is_sat
    }

    pub fn is_sat(&self) -> Option<bool> {
        self.is_sat
    }

    /// Returns `true` if last call to [`solve()`] was interrupted.
    /// Returns `false` if [`solve()`] either failed, or succeeded, or
    /// hasn't been called yet.
    ///
    /// Note, that even if last call to [`solve()`] was indeed
    /// interrupted, a subsequent invocation of [`take_last_result()`]
    /// resets this to return `false` until next [`solve()`].
    ///
    /// [`solve()`]: Solver::solve()
    /// [`take_last_result()`]: Solver::take_last_result()
    pub fn was_interrupted(&self) -> bool {
        if let Some(result) = self.last_result.as_ref() {
            if let Err(err) = result {
                return err.is_recoverable()
            }
        }
        false
    }

    pub fn get_solution(&self) -> Option<Solution> {
        self.engine.model().map(|model| Solution::from_model(self.context.clone(), model))
    }

    /// Returns the result of last call to [`solve()`].
    ///
    /// Note, that this may be invoked successfully only once for
    /// every call to [`solve()`], because, in varisat 0.2,
    /// `varisat::solver::SolverError` can't be cloned.
    ///
    /// [`solve()`]: Solver::solve()
    pub fn take_last_result(&mut self) -> Option<Result<bool, SolverError>> {
        self.last_result.take()
    }
}

impl Iterator for Solver<'_> {
    type Item = Solution;

    fn next(&mut self) -> Option<Self::Item> {
        self.solve().and_then(|is_sat| {
            if is_sat {
                self.engine.model().map(|model| {
                    if self.only_minimal {
                        self.add_blocking_clause(&model);
                    } else {
                        self.add_anti_clause(&model);
                    }

                    Solution::from_model(self.context.clone(), model)
                })
            } else {
                None
            }
        })
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
