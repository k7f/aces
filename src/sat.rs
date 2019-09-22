use std::{
    collections::{BTreeSet, BTreeMap, HashSet},
    iter::FromIterator,
    convert::TryInto,
    mem, ops, fmt,
    sync::Arc,
    error::Error,
};
use log::Level::Debug;
use varisat::{Var, Lit, CnfFormula, ExtendFormula, solver::SolverError};
use crate::{
    Atomic, Context, ContextHandle, Contextual, Polynomial, NodeID, AtomID, PortID, LinkID, ForkID,
    JoinID, Split, atom::Atom, error::AcesError,
};

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Encoding {
    PortLink,
    ForkJoin,
}

trait CEVar {
    fn from_atom_id(atom_id: AtomID) -> Self;
    fn into_atom_id(self) -> AtomID;
}

impl CEVar for Var {
    fn from_atom_id(atom_id: AtomID) -> Self {
        Var::from_dimacs(atom_id.get().try_into().unwrap())
    }

    fn into_atom_id(self) -> AtomID {
        let var = self.to_dimacs();
        unsafe { AtomID::new_unchecked(var.try_into().unwrap()) }
    }
}

impl Contextual for Var {
    fn format(&self, ctx: &Context) -> Result<String, Box<dyn Error>> {
        let atom_id = self.into_atom_id();

        if let Some(atom) = ctx.get_atom(atom_id) {
            match atom {
                Atom::Tx(port) | Atom::Rx(port) => Ok(format!("{}:{}", atom_id, ctx.with(port))),
                Atom::Link(link) => Ok(format!("{}:{}", atom_id, ctx.with(link))),
                Atom::Fork(fork) => Ok(format!("{}:{}", atom_id, ctx.with(fork))),
                Atom::Join(join) => Ok(format!("{}:{}", atom_id, ctx.with(join))),
                Atom::Bottom => Err(Box::new(AcesError::BottomAtomAccess)),
            }
        } else {
            Err(Box::new(AcesError::AtomMissingForID))
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct Variable(pub(crate) Var);

impl Contextual for Variable {
    fn format(&self, ctx: &Context) -> Result<String, Box<dyn Error>> {
        self.0.format(ctx)
    }
}

trait CELit {
    fn from_atom_id(atom_id: AtomID, negated: bool) -> Self;
    fn into_atom_id(self) -> (AtomID, bool);
}

impl CELit for Lit {
    fn from_atom_id(atom_id: AtomID, negated: bool) -> Self {
        Self::from_var(Var::from_atom_id(atom_id), !negated)
    }

    fn into_atom_id(self) -> (AtomID, bool) {
        let lit = self.to_dimacs();
        unsafe { (AtomID::new_unchecked(lit.abs().try_into().unwrap()), lit < 0) }
    }
}

impl Contextual for Lit {
    fn format(&self, ctx: &Context) -> Result<String, Box<dyn Error>> {
        if self.is_negative() {
            Ok(format!("~{}", self.var().format(ctx)?))
        } else {
            self.var().format(ctx)
        }
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
    fn format(&self, ctx: &Context) -> Result<String, Box<dyn Error>> {
        self.0.format(ctx)
    }
}

pub struct Clause {
    lits: Vec<Lit>,
    info: String,
}

impl Clause {
    pub fn from_vec<S: AsRef<str>>(lits: Vec<Lit>, info: S) -> Self {
        let info = info.as_ref().to_owned();

        Clause { lits, info }
    }

    pub fn from_literals<I, S>(literals: I, info: S) -> Self
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

    pub fn from_literals_checked<I, S>(literals: I, info: S) -> Option<Self>
    where
        I: IntoIterator + Clone,
        I::Item: Into<Lit>,
        S: AsRef<str>,
    {
        let positives: HashSet<_> = literals
            .clone()
            .into_iter()
            .filter_map(|lit| {
                let lit = lit.into();

                if lit.is_positive() {
                    Some(lit.var())
                } else {
                    None
                }
            })
            .collect();

        let negatives: HashSet<_> = literals
            .into_iter()
            .filter_map(|lit| {
                let lit = lit.into();

                if lit.is_negative() {
                    Some(lit.var())
                } else {
                    None
                }
            })
            .collect();

        if positives.is_disjoint(&negatives) {
            let lits: Vec<_> = positives
                .into_iter()
                .map(|var| Lit::from_var(var, true))
                .chain(negatives.into_iter().map(|var| Lit::from_var(var, false)))
                .collect();

            let info = info.as_ref().to_owned();

            Some(Clause { lits, info })
        } else {
            trace!("Tautology: +{:?} -{:?}", positives, negatives);
            None
        }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.lits.is_empty()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.lits.len()
    }

    #[inline]
    pub fn get_literals(&self) -> &[Lit] {
        self.lits.as_slice()
    }
}

impl Contextual for Clause {
    fn format(&self, ctx: &Context) -> Result<String, Box<dyn Error>> {
        self.lits.format(ctx)
    }
}

pub struct Formula {
    context:   ContextHandle,
    cnf:       CnfFormula,
    variables: BTreeSet<Var>,
}

impl Formula {
    pub fn new(ctx: &ContextHandle) -> Self {
        Self {
            context:   ctx.clone(),
            cnf:       Default::default(),
            variables: Default::default(),
        }
    }

    /// Only for internal use.
    fn add_clause(&mut self, clause: Clause) -> Result<(), AcesError> {
        if clause.is_empty() {
            Err(AcesError::EmptyClauseRejectedByFormula(clause.info.clone()))
        } else {
            if log_enabled!(Debug) {
                let ctx = self.context.lock().unwrap();
                debug!("Add (to formula) {} clause: {}", clause.info, ctx.with(&clause))
            }

            self.cnf.add_clause(clause.get_literals());
            self.variables.extend(clause.get_literals().iter().map(|lit| lit.var()));

            Ok(())
        }
    }

    /// Adds an _antiport_ rule to this `Formula`.
    ///
    /// This clause constrains nodes to a single part of a firing
    /// component, source or sink, so that the induced graph of any
    /// firing component is bipartite.  The `Formula` should contain
    /// one such clause for each internal node of the c-e structure
    /// under analysis.
    pub fn add_antiport(&mut self, port_id: PortID) -> Result<(), AcesError> {
        let (port_lit, antiport_lit) = {
            if let Some(antiport_id) = self.context.lock().unwrap().get_antiport_id(port_id) {
                (
                    Lit::from_atom_id(port_id.into(), true),
                    Lit::from_atom_id(antiport_id.into(), true),
                )
            } else {
                return Ok(()) // this isn't an internal node
            }
        };

        let clause = Clause::from_pair(port_lit, antiport_lit, "internal node");
        self.add_clause(clause)
    }

    /// Adds a _link coherence_ rule to this `Formula`.
    ///
    /// This firing rule consists of two clauses which are added in
    /// order to maintain link coherence, so that any firing component
    /// of a c-e structure is itself a proper c-e structure.  The
    /// `Formula` should contain one such rule for each link of the
    /// c-e structure under analysis.
    ///
    /// Returns an error if `link_id` doesn't identify any `Link` in
    /// the `Context` of this `Formula`.
    pub fn add_link_coherence(&mut self, link_id: LinkID) -> Result<(), AcesError> {
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
                return Err(AcesError::LinkMissingForID(link_id))
            }
        };

        let clause = Clause::from_pair(link_lit, tx_port_lit, "link coherence (Tx side)");
        self.add_clause(clause)?;

        let clause = Clause::from_pair(link_lit, rx_port_lit, "link coherence (Rx side)");
        self.add_clause(clause)
    }

    /// Adds a _polynomial_ rule to this formula.
    pub fn add_polynomial(
        &mut self,
        port_id: PortID,
        poly: &Polynomial<LinkID>,
    ) -> Result<(), AcesError> {
        if !poly.is_empty() {
            let port_lit = port_id.into_sat_literal(true);

            for clause in poly.as_sat_clauses(port_lit) {
                self.add_clause(clause)?;
            }
        }

        Ok(())
    }

    /// Adds an _antisplit_ rule to this formula.
    ///
    /// This set of clauses constrains nodes to a single part of a
    /// firing component, source or sink, so that the induced graph of
    /// any firing component is bipartite.  The `Formula` should
    /// contain one clause for each fork-join pair of each internal
    /// node of the c-e structure under analysis.
    pub fn add_antisplits(
        &mut self,
        split_ids: &[AtomID],
        antisplit_ids: &[AtomID],
    ) -> Result<(), AcesError> {
        for &split_id in split_ids.iter() {
            let split_lit = Lit::from_atom_id(split_id, true);

            for &antisplit_id in antisplit_ids.iter() {
                let antisplit_lit = Lit::from_atom_id(antisplit_id, true);

                let clause = Clause::from_pair(split_lit, antisplit_lit, "antisplit");
                self.add_clause(clause)?;
            }
        }

        Ok(())
    }

    /// Adds a _sidesplit_ rule to this formula.
    ///
    /// This rule enforces monomiality of firing components.
    pub fn add_sidesplits(&mut self, sidesplit_ids: &[AtomID]) -> Result<(), AcesError> {
        for (pos, &split_id) in sidesplit_ids.iter().enumerate() {
            let split_lit = Lit::from_atom_id(split_id, true);

            for &sidesplit_id in sidesplit_ids[pos + 1..].iter() {
                let sidesplit_lit = Lit::from_atom_id(sidesplit_id, true);

                let clause = Clause::from_pair(split_lit, sidesplit_lit, "sidesplit");
                self.add_clause(clause)?;
            }
        }

        Ok(())
    }

    pub fn add_cosplits(
        &mut self,
        split_id: AtomID,
        cosplit_ids: Vec<Vec<AtomID>>,
    ) -> Result<(), AcesError> {
        let split_lit = Lit::from_atom_id(split_id, true);

        if cosplit_ids.is_empty() {
            Err(AcesError::IncoherencyLeak)
        } else {
            for choice in cosplit_ids.iter() {
                if choice.is_empty() {
                    return Err(AcesError::IncoherencyLeak)
                } else {
                    // Co-split rules are encoded below as plain
                    // disjunctions instead of exclusive choice,
                    // because we rely on reduction to minimal model
                    // when solving.  FIXME reconsider.
                    let lits = Some(split_lit).into_iter().chain(
                        choice.iter().map(|cosplit_id| Lit::from_atom_id(*cosplit_id, false)),
                    );
                    let clause = Clause::from_literals(lits, "cosplit");

                    self.add_clause(clause)?;
                }
            }
            Ok(())
        }
    }

    fn get_variables(&self) -> &BTreeSet<Var> {
        &self.variables
    }
}

impl Eq for Formula {}

impl PartialEq for Formula {
    fn eq(&self, other: &Self) -> bool {
        if Arc::ptr_eq(&self.context, &other.context) {
            self.cnf == other.cnf
        } else {
            let ref ctx = self.context.lock().unwrap();
            let ref other_ctx = other.context.lock().unwrap();

            ctx.partial_cmp(other_ctx).is_some() && self.cnf == other.cnf
        }
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

pub(crate) enum ModelSearchResult {
    Reset,
    Found(Vec<Lit>),
    Done,
    Failed(SolverError),
}

impl ModelSearchResult {
    #[allow(dead_code)]
    pub(crate) fn get_model(&self) -> Option<&[Lit]> {
        match self {
            ModelSearchResult::Found(ref v) => Some(v.as_slice()),
            _ => None,
        }
    }

    #[inline]
    #[allow(dead_code)]
    pub(crate) fn take(&mut self) -> Self {
        mem::replace(self, ModelSearchResult::Reset)
    }

    pub(crate) fn take_error(&mut self) -> Option<SolverError> {
        let old_result = match self {
            ModelSearchResult::Failed(_) => mem::replace(self, ModelSearchResult::Reset),
            _ => return None,
        };

        if let ModelSearchResult::Failed(err) = old_result {
            Some(err)
        } else {
            unreachable!()
        }
    }
}

impl Default for ModelSearchResult {
    fn default() -> Self {
        ModelSearchResult::Reset
    }
}

#[derive(Default, Debug)]
struct Assumptions {
    literals:      Vec<Lit>,
    permanent_len: usize,
}

impl Assumptions {
    fn block_variable(&mut self, var: Var) {
        let lit = Lit::from_var(var, false);

        let pos = if self.permanent_len > 0 {
            match self.literals[..self.permanent_len].binary_search(&lit) {
                Ok(_) => return,
                Err(pos) => pos,
            }
        } else {
            0
        };

        self.literals.insert(pos, lit);
        self.permanent_len += 1;
    }

    fn unblock_variable(&mut self, var: Var) -> bool {
        let lit = Lit::from_var(var, false);

        if self.permanent_len > 0 {
            match self.literals[..self.permanent_len].binary_search(&lit) {
                Ok(pos) => {
                    self.literals.remove(pos);
                    self.permanent_len -= 1;
                    true
                }
                Err(_) => false,
            }
        } else {
            false
        }
    }

    fn unblock_all_variables(&mut self) {
        if self.permanent_len > 0 {
            let new_literals = self.literals.split_off(self.permanent_len);

            self.literals = new_literals;
            self.permanent_len = 0;
        }
    }

    #[inline]
    fn is_empty(&self) -> bool {
        self.literals.len() == self.permanent_len
    }

    #[inline]
    fn reset(&mut self) {
        self.literals.truncate(self.permanent_len);
    }

    #[inline]
    fn add(&mut self, lit: Lit) {
        self.literals.push(lit);
    }

    #[inline]
    fn get_literals(&self) -> &[Lit] {
        assert!(self.literals.len() >= self.permanent_len);

        self.literals.as_slice()
    }
}

impl Contextual for Assumptions {
    fn format(&self, ctx: &Context) -> Result<String, Box<dyn Error>> {
        self.literals.format(ctx)
    }
}

pub struct Solver<'a> {
    context:      ContextHandle,
    engine:       varisat::Solver<'a>,
    all_vars:     BTreeSet<Var>,
    is_sat:       Option<bool>,
    last_model:   ModelSearchResult,
    only_minimal: bool,
    min_residue:  BTreeSet<Var>,
    assumptions:  Assumptions,
}

impl<'a> Solver<'a> {
    pub fn new(ctx: &ContextHandle) -> Self {
        Self {
            context:      ctx.clone(),
            engine:       Default::default(),
            all_vars:     Default::default(),
            is_sat:       None,
            last_model:   Default::default(),
            only_minimal: false,
            min_residue:  Default::default(),
            assumptions:  Default::default(),
        }
    }

    pub fn reset(&mut self) -> Result<(), SolverError> {
        self.is_sat = None;
        self.last_model = ModelSearchResult::Reset;
        self.min_residue.clear();
        self.assumptions.reset();
        self.engine.close_proof()
    }

    pub fn set_minimal_mode(&mut self, only_minimal: bool) {
        self.only_minimal = only_minimal;
    }

    pub fn block_atom_id(&mut self, atom_id: AtomID) {
        let var = Var::from_atom_id(atom_id);

        self.assumptions.block_variable(var);
    }

    pub fn unblock_atom_id(&mut self, atom_id: AtomID) -> bool {
        let var = Var::from_atom_id(atom_id);

        self.assumptions.unblock_variable(var)
    }

    pub fn unblock_all_atoms(&mut self) {
        self.assumptions.unblock_all_variables();
    }

    /// Only for internal use.
    fn add_clause(&mut self, clause: Clause) -> Result<(), AcesError> {
        if clause.is_empty() {
            Err(AcesError::EmptyClauseRejectedBySolver(clause.info.clone()))
        } else {
            if log_enabled!(Debug) {
                let ctx = self.context.lock().unwrap();
                debug!("Add (to solver) {} clause: {}", clause.info, ctx.with(&clause));
            }

            self.engine.add_clause(clause.get_literals());

            Ok(())
        }
    }

    pub fn add_formula(&mut self, formula: &Formula) -> Result<(), AcesError> {
        self.engine.add_formula(&formula.cnf);

        let new_vars = formula.get_variables();
        self.all_vars.extend(new_vars);

        Ok(())
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
    pub fn inhibit_empty_solution(&mut self) -> Result<(), AcesError> {
        let clause = {
            let ctx = self.context.lock().unwrap();
            let mut all_lits: Vec<_> = self
                .all_vars
                .iter()
                .filter_map(|&var| ctx.is_port(var.into_atom_id()).then(Lit::from_var(var, true)))
                .collect();
            let mut fork_lits: Vec<_> = self
                .all_vars
                .iter()
                .filter_map(|&var| ctx.is_fork(var.into_atom_id()).then(Lit::from_var(var, true)))
                .collect();
            let mut join_lits: Vec<_> = self
                .all_vars
                .iter()
                .filter_map(|&var| ctx.is_join(var.into_atom_id()).then(Lit::from_var(var, true)))
                .collect();

            // Include all fork variables or all join variables,
            // depending on in which case the clause grows less.
            if fork_lits.len() > join_lits.len() {
                if join_lits.is_empty() {
                    return Err(AcesError::IncoherencyLeak)
                } else {
                    all_lits.append(&mut join_lits);
                }
            } else if !fork_lits.is_empty() {
                all_lits.append(&mut fork_lits);
            } else if !join_lits.is_empty() {
                return Err(AcesError::IncoherencyLeak)
            }

            Clause::from_vec(all_lits, "void inhibition")
        };

        self.add_clause(clause)
    }

    /// Adds a _model inhibition_ clause which will remove a specific
    /// `model` from solution space.
    ///
    /// The blocking clause is constructed by negating the given
    /// `model`, i.e. by taking the disjunction of all _explicit_
    /// literals and reversing polarity of each.  A literal is
    /// explicit iff its variable has been _registered_ by occurring
    /// in a formula passed to a call to [`add_formula()`].
    ///
    /// [`add_formula()`]: Solver::add_formula()
    pub fn inhibit_model(&mut self, model: &[Lit]) -> Result<(), AcesError> {
        let anti_lits =
            model.iter().filter_map(|&lit| self.all_vars.contains(&lit.var()).then(!lit));
        let clause = Clause::from_literals(anti_lits, "model inhibition");

        self.add_clause(clause)
    }

    pub fn inhibit_last_model(&mut self) -> Result<(), AcesError> {
        if let ModelSearchResult::Found(ref model) = self.last_model {
            let anti_lits =
                model.iter().filter_map(|&lit| self.all_vars.contains(&lit.var()).then(!lit));
            let clause = Clause::from_literals(anti_lits, "model inhibition");

            self.add_clause(clause)
        } else {
            Err(AcesError::NoModelToInhibit)
        }
    }

    pub fn reduce_model(&mut self, model: &[Lit]) -> Result<bool, AcesError> {
        let mut reducing_lits = Vec::new();

        for &lit in model.iter() {
            if !self.min_residue.contains(&lit.var()) {
                if lit.is_positive() {
                    reducing_lits.push(!lit);
                } else {
                    self.assumptions.add(lit);
                    self.min_residue.insert(lit.var());
                }
            }
        }

        if reducing_lits.is_empty() {
            Ok(false)
        } else {
            let clause = Clause::from_literals(reducing_lits.into_iter(), "model reduction");
            self.add_clause(clause)?;

            Ok(true)
        }
    }

    pub fn solve(&mut self) -> Option<bool> {
        if log_enabled!(Debug) && !self.assumptions.is_empty() {
            let ctx = self.context.lock().unwrap();
            debug!("Solving under assumptions: {}", ctx.with(&self.assumptions));
        }

        self.engine.assume(self.assumptions.get_literals());

        let result = self.engine.solve();

        if self.is_sat.is_none() {
            self.is_sat = result.as_ref().ok().copied();
        }

        match result {
            Ok(is_sat) => {
                if is_sat {
                    if let Some(model) = self.engine.model() {
                        self.last_model = ModelSearchResult::Found(model);
                        Some(true)
                    } else {
                        warn!("Solver reported SAT without a model");

                        self.last_model = ModelSearchResult::Done;
                        Some(false)
                    }
                } else {
                    self.last_model = ModelSearchResult::Done;
                    Some(false)
                }
            }
            Err(err) => {
                self.last_model = ModelSearchResult::Failed(err);
                None
            }
        }
    }

    pub fn is_sat(&self) -> Option<bool> {
        self.is_sat
    }

    /// Returns `true` if last call to [`solve()`] was interrupted.
    /// Returns `false` if [`solve()`] either failed, or succeeded, or
    /// hasn't been called yet.
    ///
    /// Note, that even if last call to [`solve()`] was indeed
    /// interrupted, a subsequent invocation of [`take_last_error()`]
    /// resets this to return `false` until next [`solve()`].
    ///
    /// [`solve()`]: Solver::solve()
    /// [`take_last_error()`]: Solver::take_last_error()
    pub fn was_interrupted(&self) -> bool {
        if let ModelSearchResult::Failed(ref err) = self.last_model {
            err.is_recoverable()
        } else {
            false
        }
    }

    pub fn get_solution(&self) -> Option<Solution> {
        self.engine.model().and_then(|model| match Solution::from_model(&self.context, model) {
            Ok(solution) => Some(solution),
            Err(err) => {
                warn!("{} in solver's solution ctor", err);
                None
            }
        })
    }

    /// Returns the error reported by last call to [`solve()`], if
    /// solving has failed; otherwise returns `None`.
    ///
    /// Note, that this may be invoked only once for every
    /// unsuccessful call to [`solve()`], because, in varisat 0.2,
    /// `varisat::solver::SolverError` can't be cloned.
    ///
    /// [`solve()`]: Solver::solve()
    pub fn take_last_error(&mut self) -> Option<SolverError> {
        self.last_model.take_error()
    }
}

impl Iterator for Solver<'_> {
    type Item = Solution;

    fn next(&mut self) -> Option<Self::Item> {
        if self.only_minimal {
            self.assumptions.reset();

            self.solve();

            if let ModelSearchResult::Found(ref top_model) = self.last_model {
                let top_model = top_model.clone();

                trace!("Top model: {:?}", top_model);

                self.min_residue.clear();
                self.assumptions.reset();

                let mut model = top_model.clone();

                loop {
                    match self.reduce_model(&model) {
                        Ok(true) => {}
                        Ok(false) => break,
                        Err(err) => {
                            warn!("{} in solver's iteration", err);
                            return None
                        }
                    }

                    self.solve();

                    if let ModelSearchResult::Found(ref reduced_model) = self.last_model {
                        trace!("Reduced model: {:?}", reduced_model);
                        model = reduced_model.clone();
                    } else {
                        break
                    }
                }

                let min_model = top_model
                    .iter()
                    .map(|lit| Lit::from_var(lit.var(), !self.min_residue.contains(&lit.var())));

                match Solution::from_model(&self.context, min_model) {
                    Ok(solution) => Some(solution),
                    Err(err) => {
                        warn!("{} in solver's iteration", err);
                        None
                    }
                }
            } else {
                None
            }
        } else {
            self.solve();

            if let ModelSearchResult::Found(ref model) = self.last_model {
                match Solution::from_model(&self.context, model.iter().copied()) {
                    Ok(solution) => {
                        if let Err(err) = self.inhibit_last_model() {
                            warn!("{} in solver's iteration", err);

                            None
                        } else {
                            Some(solution)
                        }
                    }
                    Err(err) => {
                        warn!("{} in solver's iteration", err);
                        None
                    }
                }
            } else {
                None
            }
        }
    }
}

pub struct Solution {
    context:  ContextHandle,
    model:    Vec<Lit>,
    pre_set:  Vec<NodeID>,
    post_set: Vec<NodeID>,
    fork_set: Vec<ForkID>,
    join_set: Vec<JoinID>,
}

impl Solution {
    fn new(ctx: &ContextHandle) -> Self {
        Self {
            context:  ctx.clone(),
            model:    Default::default(),
            pre_set:  Default::default(),
            post_set: Default::default(),
            fork_set: Default::default(),
            join_set: Default::default(),
        }
    }

    fn from_model<I: IntoIterator<Item = Lit>>(
        ctx: &ContextHandle,
        model: I,
    ) -> Result<Self, AcesError> {
        let mut solution = Self::new(ctx);

        let mut pre_set: BTreeSet<NodeID> = BTreeSet::new();
        let mut post_set: BTreeSet<NodeID> = BTreeSet::new();
        let mut fork_map: BTreeMap<NodeID, BTreeSet<NodeID>> = BTreeMap::new();
        let mut join_map: BTreeMap<NodeID, BTreeSet<NodeID>> = BTreeMap::new();
        let mut fork_set: BTreeSet<ForkID> = BTreeSet::new();
        let mut join_set: BTreeSet<JoinID> = BTreeSet::new();

        for lit in model {
            solution.model.push(lit);

            if lit.is_positive() {
                let (atom_id, _) = lit.into_atom_id();
                let ctx = solution.context.lock().unwrap();

                if let Some(atom) = ctx.get_atom(atom_id) {
                    match atom {
                        Atom::Tx(port) => {
                            pre_set.insert(port.get_node_id());
                        }
                        Atom::Rx(port) => {
                            post_set.insert(port.get_node_id());
                        }
                        Atom::Link(link) => {
                            let tx_node_id = link.get_tx_node_id();
                            let rx_node_id = link.get_rx_node_id();

                            fork_map
                                .entry(tx_node_id)
                                .or_insert_with(BTreeSet::new)
                                .insert(rx_node_id);
                            join_map
                                .entry(rx_node_id)
                                .or_insert_with(BTreeSet::new)
                                .insert(tx_node_id);
                        }
                        Atom::Fork(fork) => {
                            if let Some(fork_id) = fork.get_fork_id() {
                                pre_set.insert(fork.get_host_id());
                                fork_set.insert(fork_id);
                            } else {
                                return Err(AcesError::SplitMismatch)
                            }
                        }
                        Atom::Join(join) => {
                            if let Some(join_id) = join.get_join_id() {
                                post_set.insert(join.get_host_id());
                                join_set.insert(join_id);
                            } else {
                                return Err(AcesError::SplitMismatch)
                            }
                        }
                        Atom::Bottom => return Err(AcesError::BottomAtomAccess),
                    }
                } else {
                    return Err(AcesError::AtomMissingForID)
                }
            }
        }

        fork_set.extend(fork_map.into_iter().map(|(host, suit)| {
            let suit = Vec::from_iter(suit.into_iter());
            let mut fork = Split::new_fork(host, suit, Default::default());
            solution.context.lock().unwrap().share_fork(&mut fork)
        }));

        join_set.extend(join_map.into_iter().map(|(host, suit)| {
            let suit = Vec::from_iter(suit.into_iter());
            let mut join = Split::new_join(host, suit, Default::default());
            solution.context.lock().unwrap().share_join(&mut join)
        }));

        solution.pre_set.extend(pre_set.into_iter());
        solution.post_set.extend(post_set.into_iter());
        solution.fork_set.extend(fork_set.into_iter());
        solution.join_set.extend(join_set.into_iter());

        Ok(solution)
    }

    pub fn get_context(&self) -> &ContextHandle {
        &self.context
    }

    pub fn get_model(&self) -> &[Lit] {
        self.model.as_slice()
    }

    pub fn get_pre_set(&self) -> &[NodeID] {
        self.pre_set.as_slice()
    }

    pub fn get_post_set(&self) -> &[NodeID] {
        self.post_set.as_slice()
    }

    pub fn get_fork_set(&self) -> &[ForkID] {
        self.fork_set.as_slice()
    }

    pub fn get_join_set(&self) -> &[JoinID] {
        self.join_set.as_slice()
    }
}

impl fmt::Debug for Solution {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ctx = self.context.lock().unwrap();
        write!(
            f,
            "Solution {{ model: {:?}, pre_set: {}, post_set: {}, fork_set: {}, join_set: {} }}",
            self.model,
            ctx.with(&self.pre_set),
            ctx.with(&self.post_set),
            ctx.with(&self.fork_set),
            ctx.with(&self.join_set),
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

            for node_id in self.pre_set.iter() {
                write!(f, " {}", ctx.with(node_id))?;
            }

            write!(f, " }} => {{")?;
        }

        if self.post_set.is_empty() {
            write!(f, "}}")?;
        } else {
            let ctx = self.context.lock().unwrap();

            for node_id in self.post_set.iter() {
                write!(f, " {}", ctx.with(node_id))?;
            }

            write!(f, " }}")?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn new_fork_id(ctx: &ContextHandle, host_name: &str, suit_names: &[&str]) -> ForkID {
        let mut ctx = ctx.lock().unwrap();
        let host_id = ctx.share_node_name(host_name);
        let suit_ids = suit_names.iter().map(|n| ctx.share_node_name(n)).collect();
        let mut fork = Split::new_fork(host_id, suit_ids, Default::default());
        ctx.share_fork(&mut fork)
    }

    fn new_join_id(ctx: &ContextHandle, host_name: &str, suit_names: &[&str]) -> JoinID {
        let mut ctx = ctx.lock().unwrap();
        let host_id = ctx.share_node_name(host_name);
        let suit_ids = suit_names.iter().map(|n| ctx.share_node_name(n)).collect();
        let mut join = Split::new_join(host_id, suit_ids, Default::default());
        ctx.share_join(&mut join)
    }

    #[test]
    fn test_cosplits() {
        let ctx = Context::new_interactive("test_cosplits");
        let a_fork_id = new_fork_id(&ctx, "a", &["z"]);
        let z_join_id = new_join_id(&ctx, "z", &["a"]);

        let mut test_formula = Formula::new(&ctx);
        let mut ref_formula = Formula::new(&ctx);

        test_formula.add_cosplits(a_fork_id.get(), vec![vec![z_join_id.get()]]).unwrap();

        ref_formula
            .add_clause(Clause::from_pair(
                Lit::from_atom_id(a_fork_id.get(), true),
                Lit::from_atom_id(z_join_id.get(), false),
                "",
            ))
            .unwrap();

        assert_eq!(test_formula, ref_formula);
    }
}
