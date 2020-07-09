use std::{
    collections::{BTreeSet, HashSet},
    convert::TryInto,
    ops, fmt,
    sync::Arc,
};
use varisat::{Var, Lit, CnfFormula, ExtendFormula};
use crate::{
    Atomic, Context, ContextHandle, Contextual, ExclusivelyContextual, Polynomial, AtomId, PortId,
    LinkId, atom::Atom, FiringSet, AcesError, AcesErrorKind,
};

#[derive(Debug)]
pub(crate) enum Resolution {
    Unsolved,
    Incoherent,
    Deadlock,
    Solved(FiringSet),
}

impl Default for Resolution {
    fn default() -> Self {
        Resolution::Unsolved
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Encoding {
    PortLink,
    ForkJoin,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Search {
    MinSolutions,
    AllSolutions,
}

pub(crate) trait CEVar {
    fn from_atom_id(atom_id: AtomId) -> Self;
    fn into_atom_id(self) -> AtomId;
}

impl CEVar for Var {
    fn from_atom_id(atom_id: AtomId) -> Self {
        Var::from_dimacs(atom_id.get().try_into().unwrap())
    }

    fn into_atom_id(self) -> AtomId {
        let var = self.to_dimacs();
        unsafe { AtomId::new_unchecked(var.try_into().unwrap()) }
    }
}

impl ExclusivelyContextual for Var {
    fn format_locked(&self, ctx: &Context) -> Result<String, AcesError> {
        let atom_id = self.into_atom_id();

        if let Some(atom) = ctx.get_atom(atom_id) {
            match atom {
                Atom::Tx(port) | Atom::Rx(port) => {
                    Ok(format!("{}:{}", atom_id, port.format_locked(ctx)?))
                }
                Atom::Link(link) => Ok(format!("{}:{}", atom_id, link.format_locked(ctx)?)),
                Atom::Fork(fork) => Ok(format!("{}:{}", atom_id, fork.format_locked(ctx)?)),
                Atom::Join(join) => Ok(format!("{}:{}", atom_id, join.format_locked(ctx)?)),
                Atom::Pit(pit) => Ok(format!("{}:{}", atom_id, pit.format_locked(ctx)?)),
                Atom::Fuset(fuset) => Ok(format!("{}:{}", atom_id, fuset.format_locked(ctx)?)),
                Atom::Bottom => Err(AcesError::from(AcesErrorKind::BottomAtomAccess)),
            }
        } else {
            Err(AcesError::from(AcesErrorKind::AtomMissingForId(atom_id)))
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct Variable(pub(crate) Var);

impl Contextual for Variable {
    fn format(&self, ctx: &ContextHandle) -> Result<String, AcesError> {
        self.0.format(ctx)
    }
}

pub(crate) trait CELit {
    fn from_atom_id(atom_id: AtomId, negated: bool) -> Self;
    fn into_atom_id(self) -> (AtomId, bool);
}

impl CELit for Lit {
    fn from_atom_id(atom_id: AtomId, negated: bool) -> Self {
        Self::from_var(Var::from_atom_id(atom_id), !negated)
    }

    fn into_atom_id(self) -> (AtomId, bool) {
        let lit = self.to_dimacs();
        unsafe { (AtomId::new_unchecked(lit.abs().try_into().unwrap()), lit < 0) }
    }
}

impl ExclusivelyContextual for Lit {
    fn format_locked(&self, ctx: &Context) -> Result<String, AcesError> {
        if self.is_negative() {
            Ok(format!("~{}", self.var().format_locked(ctx)?))
        } else {
            self.var().format_locked(ctx)
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct Literal(pub(crate) Lit);

impl Literal {
    pub(crate) fn from_atom_id(atom_id: AtomId, negated: bool) -> Self {
        Self(Lit::from_atom_id(atom_id, negated))
    }

    #[allow(dead_code)]
    pub(crate) fn into_atom_id(self) -> (AtomId, bool) {
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
    #[inline]
    fn from(lit: Lit) -> Self {
        Literal(lit)
    }
}

impl From<Literal> for Lit {
    #[inline]
    fn from(lit: Literal) -> Self {
        lit.0
    }
}

impl From<&Literal> for Lit {
    #[inline]
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
    fn format(&self, ctx: &ContextHandle) -> Result<String, AcesError> {
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

    #[inline]
    pub fn get_info(&self) -> &str {
        self.info.as_str()
    }
}

impl Contextual for Clause {
    fn format(&self, ctx: &ContextHandle) -> Result<String, AcesError> {
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
            Err(AcesErrorKind::EmptyClauseRejectedByFormula(clause.info)
                .with_context(&self.context))
        } else {
            debug!("Add (to formula) {} clause: {}", clause.info, clause.with(&self.context));

            self.cnf.add_clause(clause.get_literals());
            self.variables.extend(clause.get_literals().iter().map(|lit| lit.var()));

            Ok(())
        }
    }

    /// Adds an _anti_port_ rule to this `Formula`.
    ///
    /// This clause constrains dots to a single part of a firing
    /// component, source or sink, so that the induced graph of any
    /// firing component is bipartite.  The `Formula` should contain
    /// one such clause for each internal dot of the c-e structure
    /// under analysis.
    pub fn add_anti_port(&mut self, port_id: PortId) -> Result<(), AcesError> {
        let (port_lit, anti_port_lit) = {
            if let Some(anti_port_id) = self.context.lock().unwrap().get_anti_port_id(port_id) {
                (
                    Lit::from_atom_id(port_id.into(), true),
                    Lit::from_atom_id(anti_port_id.into(), true),
                )
            } else {
                return Ok(()) // this isn't an internal dot
            }
        };

        let clause = Clause::from_pair(port_lit, anti_port_lit, "internal dot");
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
    pub fn add_link_coherence(&mut self, link_id: LinkId) -> Result<(), AcesError> {
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
                return Err(AcesErrorKind::LinkMissingForId(link_id).with_context(&self.context))
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
        port_id: PortId,
        poly: &Polynomial<LinkId>,
    ) -> Result<(), AcesError> {
        if !poly.is_empty() {
            let port_lit = port_id.into_sat_literal(true);

            for clause in poly.as_sat_clauses(port_lit) {
                self.add_clause(clause)?;
            }
        }

        Ok(())
    }

    /// Adds an _anti_wedge_ rule to this formula.
    ///
    /// This set of clauses constrains dots to a single part of a
    /// firing component, source or sink, so that the induced graph of
    /// any firing component is bipartite.  The `Formula` should
    /// contain one clause for each fork-join pair of each internal
    /// dot of the c-e structure under analysis.
    pub fn add_anti_wedges(
        &mut self,
        wedge_ids: &[AtomId],
        anti_wedge_ids: &[AtomId],
    ) -> Result<(), AcesError> {
        for &wedge_id in wedge_ids.iter() {
            let wedge_lit = Lit::from_atom_id(wedge_id, true);

            for &anti_wedge_id in anti_wedge_ids.iter() {
                let anti_wedge_lit = Lit::from_atom_id(anti_wedge_id, true);

                let clause = Clause::from_pair(wedge_lit, anti_wedge_lit, "anti_wedge");
                self.add_clause(clause)?;
            }
        }

        Ok(())
    }

    /// Adds a _branch_wedge_ rule to this formula.
    ///
    /// This rule enforces monomiality of firing components.
    pub fn add_branch_wedges(&mut self, branch_wedge_ids: &[AtomId]) -> Result<(), AcesError> {
        for (pos, &wedge_id) in branch_wedge_ids.iter().enumerate() {
            let wedge_lit = Lit::from_atom_id(wedge_id, true);

            for &branch_wedge_id in branch_wedge_ids[pos + 1..].iter() {
                let branch_wedge_lit = Lit::from_atom_id(branch_wedge_id, true);

                let clause = Clause::from_pair(wedge_lit, branch_wedge_lit, "branch_wedge");
                self.add_clause(clause)?;
            }
        }

        Ok(())
    }

    pub fn add_cowedges(
        &mut self,
        wedge_id: AtomId,
        cowedge_ids: Vec<Vec<AtomId>>,
    ) -> Result<(), AcesError> {
        let wedge_lit = Lit::from_atom_id(wedge_id, true);

        if cowedge_ids.is_empty() {
            Err(AcesErrorKind::IncoherencyLeak.with_context(&self.context))
        } else {
            for choice in cowedge_ids.iter() {
                if choice.is_empty() {
                    return Err(AcesErrorKind::IncoherencyLeak.with_context(&self.context))
                } else {
                    // Co-wedge rules are encoded below as plain
                    // disjunctions instead of exclusive choice,
                    // because we rely on reduction to minimal model
                    // when solving.  FIXME reconsider.
                    let lits = Some(wedge_lit).into_iter().chain(
                        choice.iter().map(|cowedge_id| Lit::from_atom_id(*cowedge_id, false)),
                    );
                    let clause = Clause::from_literals(lits, "cowedge");

                    self.add_clause(clause)?;
                }
            }
            Ok(())
        }
    }

    pub(crate) fn get_cnf(&self) -> &CnfFormula {
        &self.cnf
    }

    pub(crate) fn get_variables(&self) -> &BTreeSet<Var> {
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

            let mut first_lit = true;

            for lit in clause {
                let lit = Literal(*lit);

                if first_lit {
                    first_lit = false;
                } else {
                    write!(f, " | ")?;
                }

                write!(f, "{}", lit.with(&self.context))?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{Context, ForkId, JoinId, domain::Dotset};
    use super::*;

    fn new_fork_id(ctx: &ContextHandle, tip_name: &str, arm_names: &[&str]) -> ForkId {
        let mut ctx = ctx.lock().unwrap();
        let tip_id = ctx.share_dot_name(tip_name);
        let arm_ids = arm_names.iter().map(|n| ctx.share_dot_name(n));
        let pit = Dotset::new(arm_ids);

        ctx.share_fork_from_tip_and_pit(tip_id, pit)
    }

    fn new_join_id(ctx: &ContextHandle, tip_name: &str, arm_names: &[&str]) -> JoinId {
        let mut ctx = ctx.lock().unwrap();
        let tip_id = ctx.share_dot_name(tip_name);
        let arm_ids = arm_names.iter().map(|n| ctx.share_dot_name(n));
        let pit = Dotset::new(arm_ids);

        ctx.share_join_from_tip_and_pit(tip_id, pit)
    }

    #[test]
    fn test_cowedges() {
        let ctx = Context::new_toplevel("test_cowedges");
        let a_fork_id = new_fork_id(&ctx, "a", &["z"]);
        let z_join_id = new_join_id(&ctx, "z", &["a"]);

        let mut test_formula = Formula::new(&ctx);
        let mut ref_formula = Formula::new(&ctx);

        test_formula.add_cowedges(a_fork_id.get(), vec![vec![z_join_id.get()]]).unwrap();

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
