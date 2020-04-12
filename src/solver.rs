use std::{
    collections::{BTreeSet, BTreeMap},
    mem, fmt,
};
use varisat::{Var, Lit, ExtendFormula, solver::SolverError};
use crate::{
    ContextHandle, Contextual, NodeID, AtomID, ForkID, JoinID, Harc, AcesError, AcesErrorKind,
    atom::Atom,
    sat::{CEVar, CELit, Encoding, Search, Clause, Formula},
};

#[derive(Clone, Default, Debug)]
pub(crate) struct Props {
    pub(crate) sat_encoding: Option<Encoding>,
    pub(crate) sat_search:   Option<Search>,
}

impl Props {
    pub(crate) fn clear(&mut self) {
        *self = Default::default();
    }
}

enum ModelSearchResult {
    Reset,
    Found(Vec<Lit>),
    Done,
    Failed(SolverError),
}

impl ModelSearchResult {
    #[allow(dead_code)]
    fn get_model(&self) -> Option<&[Lit]> {
        match self {
            ModelSearchResult::Found(ref v) => Some(v.as_slice()),
            _ => None,
        }
    }

    #[inline]
    #[allow(dead_code)]
    fn take(&mut self) -> Self {
        mem::replace(self, ModelSearchResult::Reset)
    }

    fn take_error(&mut self) -> Option<SolverError> {
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
    fn format(&self, ctx: &ContextHandle) -> Result<String, AcesError> {
        self.literals.format(ctx)
    }
}

pub struct Solver<'a> {
    context:     ContextHandle,
    engine:      varisat::Solver<'a>,
    all_vars:    BTreeSet<Var>,
    is_sat:      Option<bool>,
    last_model:  ModelSearchResult,
    min_residue: BTreeSet<Var>,
    assumptions: Assumptions,
}

impl<'a> Solver<'a> {
    pub fn new(ctx: &ContextHandle) -> Self {
        Self {
            context:     ctx.clone(),
            engine:      Default::default(),
            all_vars:    Default::default(),
            is_sat:      None,
            last_model:  Default::default(),
            min_residue: Default::default(),
            assumptions: Default::default(),
        }
    }

    pub fn reset(&mut self) -> Result<(), SolverError> {
        self.is_sat = None;
        self.last_model = ModelSearchResult::Reset;
        self.min_residue.clear();
        self.assumptions.reset();
        self.engine.close_proof()
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
            Err(AcesErrorKind::EmptyClauseRejectedBySolver(clause.get_info().to_owned())
                .with_context(&self.context))
        } else {
            debug!("Add (to solver) {} clause: {}", clause.get_info(), clause.with(&self.context));

            self.engine.add_clause(clause.get_literals());

            Ok(())
        }
    }

    pub fn add_formula(&mut self, formula: &Formula) -> Result<(), AcesError> {
        self.engine.add_formula(formula.get_cnf());

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
                .filter_map(|&var| {
                    ctx.is_port(var.into_atom_id()).then(|| Lit::from_var(var, true))
                })
                .collect();
            let mut fork_lits: Vec<_> = self
                .all_vars
                .iter()
                .filter_map(|&var| {
                    ctx.is_fork(var.into_atom_id()).then(|| Lit::from_var(var, true))
                })
                .collect();
            let mut join_lits: Vec<_> = self
                .all_vars
                .iter()
                .filter_map(|&var| {
                    ctx.is_join(var.into_atom_id()).then(|| Lit::from_var(var, true))
                })
                .collect();

            // Include all fork variables or all join variables,
            // depending on in which case the clause grows less.
            if fork_lits.len() > join_lits.len() {
                if join_lits.is_empty() {
                    return Err(AcesErrorKind::IncoherencyLeak.with_context(&self.context))
                } else {
                    all_lits.append(&mut join_lits);
                }
            } else if !fork_lits.is_empty() {
                all_lits.append(&mut fork_lits);
            } else if !join_lits.is_empty() {
                return Err(AcesErrorKind::IncoherencyLeak.with_context(&self.context))
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
            model.iter().filter_map(|&lit| self.all_vars.contains(&lit.var()).then(|| !lit));
        let clause = Clause::from_literals(anti_lits, "model inhibition");

        self.add_clause(clause)
    }

    fn inhibit_last_model(&mut self) -> Result<(), AcesError> {
        if let ModelSearchResult::Found(ref model) = self.last_model {
            let anti_lits =
                model.iter().filter_map(|&lit| self.all_vars.contains(&lit.var()).then(|| !lit));
            let clause = Clause::from_literals(anti_lits, "model inhibition");

            self.add_clause(clause)
        } else {
            Err(AcesErrorKind::NoModelToInhibit.with_context(&self.context))
        }
    }

    fn reduce_model(&mut self, model: &[Lit]) -> Result<bool, AcesError> {
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

    fn solve(&mut self) -> Option<bool> {
        if !self.assumptions.is_empty() {
            debug!("Solving under assumptions: {}", self.assumptions.with(&self.context));
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

    pub(crate) fn is_sat(&self) -> Option<bool> {
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

    pub fn last_solution(&self) -> Option<Solution> {
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
    pub(crate) fn take_last_error(&mut self) -> Option<SolverError> {
        self.last_model.take_error()
    }

    fn next_solution(&mut self) -> Option<Solution> {
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

    fn next_minimal_solution(&mut self) -> Option<Solution> {
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
    }
}

impl Iterator for Solver<'_> {
    type Item = Solution;

    fn next(&mut self) -> Option<Self::Item> {
        let search = self.context.lock().unwrap().get_search().unwrap_or(Search::MinSolutions);

        match search {
            Search::MinSolutions => self.next_minimal_solution(),
            Search::AllSolutions => self.next_solution(),
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
                            } else if let Some(join_id) = fork.get_join_id() {
                                return Err(AcesErrorKind::HarcNotAForkMismatch(join_id)
                                    .with_context(&solution.context))
                            } else {
                                unreachable!()
                            }
                        }
                        Atom::Join(join) => {
                            if let Some(join_id) = join.get_join_id() {
                                post_set.insert(join.get_host_id());
                                join_set.insert(join_id);
                            } else if let Some(fork_id) = join.get_fork_id() {
                                return Err(AcesErrorKind::HarcNotAJoinMismatch(fork_id)
                                    .with_context(&solution.context))
                            } else {
                                unreachable!()
                            }
                        }
                        Atom::Bottom => {
                            return Err(
                                AcesErrorKind::BottomAtomAccess.with_context(&solution.context)
                            )
                        }
                    }
                } else {
                    return Err(
                        AcesErrorKind::AtomMissingForID(atom_id).with_context(&solution.context)
                    )
                }
            }
        }

        fork_set.extend(fork_map.into_iter().map(|(host, suit)| {
            let mut fork = Harc::new_fork_unchecked(host, suit);
            solution.context.lock().unwrap().share_fork(&mut fork)
        }));

        join_set.extend(join_map.into_iter().map(|(host, suit)| {
            let mut join = Harc::new_join_unchecked(host, suit);
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
        write!(
            f,
            "Solution {{ model: {:?}, pre_set: {}, post_set: {}, fork_set: {}, join_set: {} }}",
            self.model,
            self.pre_set.with(&self.context),
            self.post_set.with(&self.context),
            self.fork_set.with(&self.context),
            self.join_set.with(&self.context),
        )
    }
}

impl fmt::Display for Solution {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.pre_set.is_empty() {
            write!(f, "{{}} => {{")?;
        } else {
            write!(f, "{{")?;

            for node_id in self.pre_set.iter() {
                write!(f, " {}", node_id.with(&self.context))?;
            }

            write!(f, " }} => {{")?;
        }

        if self.post_set.is_empty() {
            write!(f, "}}")?;
        } else {
            for node_id in self.post_set.iter() {
                write!(f, " {}", node_id.with(&self.context))?;
            }

            write!(f, " }}")?;
        }

        Ok(())
    }
}
