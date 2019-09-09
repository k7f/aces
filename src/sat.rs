use std::{
    collections::{BTreeSet, BTreeMap, HashSet},
    convert::TryInto,
    mem, ops, fmt,
    error::Error,
};
use log::Level::Debug;
use varisat::{Var, Lit, CnfFormula, ExtendFormula, solver::SolverError};
use crate::{
    Atomic, Context, ContextHandle, Contextual, Polynomial, NodeID, AtomID, PortID, LinkID, ForkID,
    JoinID, Split, node, atom::Atom, error::AcesError,
};

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub(crate) enum Encoding {
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
                Atom::Tx(port) | Atom::Rx(port) => Ok(format!("{}", ctx.with(port))),
                Atom::Link(link) => Ok(format!("{}", ctx.with(link))),
                Atom::Fork(fork) => Ok(format!("{}", ctx.with(fork))),
                Atom::Join(join) => Ok(format!("{}", ctx.with(join))),
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

    fn add_clause(&mut self, clause: Clause) {
        if clause.is_empty() {
            panic!("Empty {} clause, not added to formula", clause.info);
        } else {
            if log_enabled!(Debug) {
                let ctx = self.context.lock().unwrap();
                debug!("Add (to formula) {} clause: {}", clause.info, ctx.with(&clause))
            }

            self.cnf.add_clause(clause.get_literals());
            self.variables.extend(clause.get_literals().iter().map(|lit| lit.var()));
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

    /// Adds an _antisplit_ rule to this formula.
    ///
    /// This set of clauses constrains nodes to a single part of a
    /// firing component, source or sink, so that the induced graph of
    /// any firing component is bipartite.  The `Formula` should
    /// contain one clause for each fork-join pair of each internal
    /// node of the c-e structure under analysis.
    pub fn add_antisplits(&mut self, split_ids: &[AtomID], antisplit_ids: &[AtomID]) {
        for &split_id in split_ids.iter() {
            let split_lit = Lit::from_atom_id(split_id, true);

            for &antisplit_id in antisplit_ids.iter() {
                let antisplit_lit = Lit::from_atom_id(antisplit_id, true);

                let clause = Clause::from_pair(split_lit, antisplit_lit, "antisplit");
                self.add_clause(clause);
            }
        }
    }

    /// Adds a _sidesplit_ rule to this formula.
    ///
    /// This rule enforces monomiality of firing components.
    pub fn add_sidesplits(&mut self, sidesplit_ids: &[AtomID]) {
        for (pos, &split_id) in sidesplit_ids.iter().enumerate() {
            let split_lit = Lit::from_atom_id(split_id, true);

            for &sidesplit_id in sidesplit_ids[pos + 1..].iter() {
                let sidesplit_lit = Lit::from_atom_id(sidesplit_id, true);

                let clause = Clause::from_pair(split_lit, sidesplit_lit, "sidesplit");
                self.add_clause(clause);
            }
        }
    }

    pub fn add_cosplits(
        &mut self,
        split_id: AtomID,
        cosplit_ids: Vec<Vec<AtomID>>,
    ) -> Result<(), AcesError> {
        if cosplit_ids.len() < 2 {
            if let Some(coterm) = cosplit_ids.get(0) {
                if coterm.len() < 2 {
                    if let Some(&cosplit_id) = coterm.get(0) {
                        let split_lit = Lit::from_atom_id(split_id, true);
                        let cosplit_lit = Lit::from_atom_id(cosplit_id, false);
                        let clause = Clause::from_pair(split_lit, cosplit_lit, "cosplit");
                        self.add_clause(clause);

                        Ok(())
                    } else {
                        Err(AcesError::IncoherencyLeak)
                    }
                } else {
                    println!("add_cosplits {} -> {:?} (single co-term)", split_id, coterm);

                    Ok(())
                }
            } else {
                Err(AcesError::IncoherencyLeak)
            }
        } else {
            println!("add_cosplits {} -> {:?}", split_id, cosplit_ids);

            Ok(())
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

pub struct Solver<'a> {
    context:      ContextHandle,
    engine:       varisat::Solver<'a>,
    port_vars:    BTreeSet<Var>,
    is_sat:       Option<bool>,
    last_model:   ModelSearchResult,
    only_minimal: bool,
    min_residue:  BTreeSet<Var>,
    assumptions:  Vec<Lit>,
}

impl<'a> Solver<'a> {
    pub fn new(ctx: &ContextHandle) -> Self {
        Self {
            context:      ctx.clone(),
            engine:       Default::default(),
            port_vars:    Default::default(),
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
        self.assumptions.clear();
        self.engine.close_proof()
    }

    pub fn set_minimal_mode(&mut self, only_minimal: bool) {
        self.only_minimal = only_minimal;
    }

    fn add_clause(&mut self, clause: Clause) -> bool {
        if clause.is_empty() {
            panic!("Empty {} clause, not added to solver", clause.info);

            false
        } else {
            if log_enabled!(Debug) {
                let ctx = self.context.lock().unwrap();
                debug!("Add (to solver) {} clause: {}", clause.info, ctx.with(&clause));
            }

            self.engine.add_clause(clause.get_literals());

            true
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
    pub fn inhibit_empty_solution(&mut self) -> bool {
        let lits = self.port_vars.iter().map(|&var| Lit::from_var(var, true));
        let clause = Clause::from_literals(lits, "void inhibition");

        self.add_clause(clause)
    }

    /// Adds a _model inhibition_ clause which will remove a specific
    /// `model` from solution space.
    ///
    /// The blocking clause is constructed by negating the given
    /// `model`, i.e. by taking the disjunction of all literals and
    /// reversing polarity of each.
    pub fn inhibit_model(&mut self, model: &[Lit]) -> bool {
        let anti_lits = model.iter().map(|&lit| !lit);
        let clause = Clause::from_literals(anti_lits, "model inhibition");

        self.add_clause(clause)
    }

    pub fn inhibit_last_model(&mut self) -> bool {
        if let ModelSearchResult::Found(ref model) = self.last_model {
            let anti_lits = model.iter().map(|&lit| !lit);
            let clause = Clause::from_literals(anti_lits, "model inhibition");

            self.add_clause(clause)
        } else {
            false
        }
    }

    pub fn reduce_model(&mut self, model: &[Lit]) -> bool {
        let mut reducing_lits = Vec::new();

        for &lit in model.iter() {
            if !self.min_residue.contains(&lit.var()) {
                if lit.is_positive() {
                    reducing_lits.push(!lit);
                } else {
                    self.assumptions.push(lit);
                    self.min_residue.insert(lit.var());
                }
            }
        }

        if reducing_lits.is_empty() {
            false
        } else {
            let clause = Clause::from_literals(reducing_lits.into_iter(), "model reduction");
            self.add_clause(clause)
        }
    }

    pub fn solve(&mut self) -> Option<bool> {
        if log_enabled!(Debug) && !self.assumptions.is_empty() {
            let ctx = self.context.lock().unwrap();
            debug!("Solving under assumptions: {}", ctx.with(&self.assumptions));
        }

        self.engine.assume(self.assumptions.as_slice());

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
        self.engine.model().map(|model| Solution::from_model(&self.context, model))
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
            self.assumptions.clear();

            self.solve();

            if let ModelSearchResult::Found(ref top_model) = self.last_model {
                let top_model = top_model.clone();

                trace!("Top model: {:?}", top_model);

                self.min_residue.clear();
                self.assumptions.clear();

                let mut model = top_model.clone();

                while self.reduce_model(&model) {
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

                Some(Solution::from_model(&self.context, min_model))
            } else {
                None
            }
        } else {
            self.inhibit_last_model();
            self.solve();

            if let ModelSearchResult::Found(ref model) = self.last_model {
                Some(Solution::from_model(&self.context, model.iter().copied()))
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

    fn from_model<I: IntoIterator<Item = Lit>>(ctx: &ContextHandle, model: I) -> Self {
        let mut solution = Self::new(ctx);
        let mut fork_map = BTreeMap::new();
        let mut join_map = BTreeMap::new();

        for lit in model {
            solution.model.push(lit);

            if lit.is_positive() {
                let (atom_id, _) = lit.into_atom_id();
                let ctx = solution.context.lock().unwrap();

                if let Some(port) = ctx.get_port(PortID(atom_id)) {
                    let node_id = port.get_node_id();

                    if port.get_face() == node::Face::Tx {
                        solution.pre_set.push(node_id);
                    } else {
                        solution.post_set.push(node_id);
                    }
                } else if let Some(link) = ctx.get_link(LinkID(atom_id)) {
                    let tx_node_id = link.get_tx_node_id();
                    let rx_node_id = link.get_rx_node_id();

                    fork_map.entry(tx_node_id).or_insert_with(Vec::new).push(rx_node_id);
                    join_map.entry(rx_node_id).or_insert_with(Vec::new).push(tx_node_id);
                }
            }
        }

        {
            let mut ctx = solution.context.lock().unwrap();

            solution.fork_set = fork_map
                .into_iter()
                .map(|(k, v)| {
                    ctx.share_fork(&mut Split::new_fork(k, v, Default::default()))
                    // FIXME
                })
                .collect();

            solution.join_set = join_map
                .into_iter()
                .map(|(k, v)| {
                    ctx.share_join(&mut Split::new_join(k, v, Default::default()))
                    // FIXME
                })
                .collect();
        }

        solution
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
