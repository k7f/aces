use std::{slice, iter, cmp, ops, fmt, collections::BTreeSet, error::Error};
use bit_vec::BitVec;
use crate::{
    NodeID, Port, Link, LinkID, Context, ContextHandle, Contextual, InContextMut, Atomic, node,
    monomial, sat, error::AcesError,
};

// FIXME sort rows as a way to fix Eq and, perhaps, to open some
// optimization opportunities.
/// A formal polynomial.
///
/// Internally a `Polynomial` is represented as a vector of _N_
/// [`Atomic`] identifiers, a vector of _M_ [`monomial::Weight`]s, and
/// a boolean matrix with _N_ columns and _M_ rows.  Vector of
/// [`Atomic`] identifiers is sorted in strictly increasing order.  It
/// represents the set of nodes occuring in the polynomial and _N_ is
/// the number of such nodes.
///
/// _M_ is the number of monomials in the canonical representation of
/// the polynomial.  The order in which monomials are listed is
/// arbitrary, but same for rows of the boolean matrix and elements of
/// the weight vector.  An element in row _i_ and column _j_ of the
/// matrix determines if a node in _j_-th position in the vector of
/// identifiers occurs in _i_-th monomial.
///
/// `Polynomial`s may be compared and added using traits from
/// [`std::cmp`] and [`std::ops`] standard modules, with the obvious
/// exception of [`std::cmp::Ord`].  Note however that, in general,
/// preventing [`Context`] or [`Port`] mismatch between polynomials is
/// the responsibility of the caller of an operation.  Implementation
/// detects some, but not all, cases of mismatch and panics if it does
/// so.
#[derive(Clone, Debug)]
pub struct Polynomial<T: Atomic + fmt::Debug> {
    atomics: Vec<T>,
    weights: Vec<monomial::Weight>,
    // FIXME choose a better representation of a boolean matrix.
    terms: Vec<BitVec>,
    dock:  Option<node::Face>,
}

impl Polynomial<LinkID> {
    /// Creates a polynomial from a sequence of vectors of
    /// [`NodeID`]s and in a [`Context`] given by a [`ContextHandle`].
    pub fn from_nodes_in_context<'a, I>(
        ctx: &ContextHandle,
        face: node::Face,
        node_id: NodeID,
        poly_ids: I,
    ) -> Self
    where
        I: IntoIterator + 'a,
        I::Item: IntoIterator<Item = &'a NodeID>,
    {
        let mut result = Self::new_docked(face);
        let mut port = Port::new(face, node_id);
        let pid = ctx.lock().unwrap().share_port(&mut port);
        let host = port.get_node_id();
        let face = port.get_face();

        let mut ctx = ctx.lock().unwrap();

        for mono_ids in poly_ids.into_iter() {
            let mut out_ids = BTreeSet::new();

            for cohost in mono_ids.into_iter().copied() {
                let mut coport = Port::new(!face, cohost);
                let copid = ctx.share_port(&mut coport);

                let mut link = match face {
                    node::Face::Tx => Link::new(pid, host, copid, cohost),
                    node::Face::Rx => Link::new(copid, cohost, pid, host),
                };
                let id = ctx.share_link(&mut link);

                out_ids.insert(id);
            }
            result.add_atomics_sorted(out_ids.into_iter()).unwrap();
        }

        result
    }
}

impl<T: Atomic + fmt::Debug> Polynomial<T> {
    /// Creates an empty polynomial, _&theta;_.
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            atomics: Default::default(),
            weights: Default::default(),
            terms:   Default::default(),
            dock:    None,
        }
    }

    /// Creates an empty polynomial, _&theta;_, docked at a given
    /// [`node::Face`].
    #[allow(clippy::new_without_default)]
    pub fn new_docked(dock: node::Face) -> Self {
        Self {
            atomics: Default::default(),
            weights: Default::default(),
            terms:   Default::default(),
            dock:    Some(dock),
        }
    }

    /// Resets this polynomial into _&theta;_.
    pub fn clear(&mut self) {
        self.atomics.clear();
        self.weights.clear();
        self.terms.clear();
    }

    /// Multiplies this polynomial (all its monomials) by a
    /// single-element monomial.
    pub fn atomic_multiply(&mut self, atomic: T) {
        if self.is_empty() {
            self.atomics.push(atomic);
            self.weights.push(Default::default());

            let mut row = BitVec::new();
            row.push(true);
            self.terms.push(row);
        } else if atomic > *self.atomics.last().unwrap() {
            self.atomics.push(atomic);

            for row in self.terms.iter_mut() {
                row.push(true);
            }
        } else {
            match self.atomics.binary_search(&atomic) {
                Ok(ndx) => {
                    if !self.is_monomial() {
                        for row in self.terms.iter_mut() {
                            row.set(ndx, true);
                        }
                    }
                }
                Err(ndx) => {
                    let old_len = self.atomics.len();

                    self.atomics.insert(ndx, atomic);

                    for row in self.terms.iter_mut() {
                        row.push(false);

                        for i in ndx..old_len {
                            let bit = row.get(i).unwrap();
                            row.set(i + 1, bit);
                        }

                        row.set(ndx, true);
                    }
                }
            }
        }
    }

    /// Adds a sequence of [`Atomic`] identifiers to this polynomial
    /// as another monomial.
    ///
    /// On success, returns `true` if this polynomial changed or
    /// `false` if it didn't, due to idempotency of addition.
    ///
    /// Returns error if `atomics` aren't given in strictly increasing
    /// order, or in case of port mismatch, or if context mismatch was
    /// detected.
    pub fn add_atomics_sorted<I>(&mut self, atomics: I) -> Result<bool, AcesError>
    where
        I: IntoIterator<Item = T>,
    {
        if self.is_empty() {
            let mut prev_thing = None;

            for thing in atomics.into_iter() {
                if let Some(prev) = prev_thing {
                    if thing <= prev {
                        self.atomics.clear();

                        return Err(AcesError::AtomicsNotOrdered)
                    }
                }
                prev_thing = Some(thing);

                self.atomics.push(thing);
            }

            if self.atomics.is_empty() {
                return Ok(false)
            } else {
                self.weights.push(Default::default());

                let row = BitVec::from_elem(self.atomics.len(), true);
                self.terms.push(row);

                return Ok(true)
            }
        }

        let mut new_row = BitVec::with_capacity(2 * self.atomics.len());

        let mut prev_thing = None;
        let mut no_new_things = true;

        // These are used for error recovery; `old_terms` also helps in
        // the implementation of inserting extra bits into rows of the
        // `terms` matrix.
        let mut old_atomics = None;
        let mut old_terms = None;

        let mut ndx = 0;

        for thing in atomics.into_iter() {
            if let Some(prev) = prev_thing {
                if thing <= prev {
                    // Error recovery.

                    if let Some(old_atomics) = old_atomics {
                        self.atomics = old_atomics;
                    }

                    if let Some(old_terms) = old_terms {
                        self.terms = old_terms;
                    }

                    return Err(AcesError::AtomicsNotOrdered)
                }
            }
            prev_thing = Some(thing);

            match self.atomics[ndx..].binary_search(&thing) {
                Ok(i) => {
                    if i > 0 {
                        new_row.grow(i, false);
                    }
                    new_row.push(true);
                    ndx += i + 1;
                }
                Err(i) => {
                    ndx += i;

                    no_new_things = false;

                    if i > 0 {
                        new_row.grow(i, false);
                    }
                    new_row.push(true);

                    self.atomics.insert(ndx, thing);

                    if old_atomics.is_none() {
                        old_atomics = Some(self.atomics.clone());
                    }
                    if old_terms.is_none() {
                        old_terms = Some(self.terms.clone());
                    }

                    let all_rows = old_terms.as_ref().unwrap();

                    for (row, tail_row) in self.terms.iter_mut().zip(all_rows.iter()) {
                        row.truncate(ndx);
                        row.push(false);
                        row.extend(tail_row.iter().skip(ndx));
                    }

                    ndx += 1;
                }
            }
        }

        if no_new_things {
            for row in self.terms.iter() {
                if *row == new_row {
                    return Ok(false)
                }
            }
        }

        self.weights.push(Default::default());
        self.terms.push(new_row);

        Ok(true)
    }

    /// Adds another `Polynomial` to this polynomial.
    ///
    /// On success, returns `true` if this polynomial changed or
    /// `false` if it didn't, due to idempotency of addition.
    ///
    /// Returns error in case of port mismatch, or if context mismatch
    /// was detected.
    pub(crate) fn add_polynomial(&mut self, other: &Self) -> Result<bool, AcesError> {
        if self.dock == other.dock {
            // FIXME optimize.  There are two special cases: when `self`
            // is a superpolynomial, and when it is a subpolynomial of
            // `other`.  First case is a nop; the second: clear followed
            // by clone.

            let mut changed = false;

            for mono in other.get_monomials() {
                if self.add_atomics_sorted(mono)? {
                    changed = true;
                }
            }

            Ok(changed)
        } else {
            Err(AcesError::PortMismatch)
        }
    }

    pub fn is_empty(&self) -> bool {
        self.terms.is_empty()
    }

    pub fn is_atomic(&self) -> bool {
        self.atomics.len() == 1
    }

    pub fn is_monomial(&self) -> bool {
        self.terms.len() == 1
    }

    pub fn num_monomials(&self) -> usize {
        self.terms.len()
    }

    /// Creates a [`Monomials`] iterator.
    pub fn get_monomials(&self) -> Monomials<T> {
        Monomials { poly: self, ndx: 0 }
    }

    pub fn get_atomics(&self) -> slice::Iter<T> {
        self.atomics.iter()
    }

    /// Constructs the firing rule of this polynomial, the logical
    /// constraint imposed on firing components, if the polynomial is
    /// attached to the node and face represented by `port_lit`.  The
    /// rule is transformed into a CNF formula, a conjunction of
    /// disjunctions of [`sat::Literal`]s (a sequence of clauses).
    ///
    /// Returns a sequence of clauses in the form of vector of vectors
    /// of [`sat::Literal`]s.
    pub fn as_sat_clauses(&self, port_lit: sat::Literal) -> Vec<sat::Clause> {
        if self.is_atomic() {
            // Consequence is a single positive link literal.  The
            // rule is a single two-literal port-link clause.

            vec![sat::Clause::from_pair(
                self.atomics[0].into_sat_literal(false),
                port_lit,
                "atomic",
            )]
        } else if self.is_monomial() {
            // Consequence is a conjunction of _N_ >= 2 positive link
            // literals.  The rule is a sequence of _N_ two-literal
            // port-link clauses.

            self.atomics
                .iter()
                .map(|id| sat::Clause::from_pair(id.into_sat_literal(false), port_lit, "monomial"))
                .collect()
        } else {
            // Consequence is a disjunction of _M_ >= 2 statements,
            // each being a conjunction of _N_ >= 2 positive and
            // negative (at least one of each) link literals.  The
            // rule (after being expanded to CNF by distribution) is a
            // sequence of _N_^_M_ port-link clauses, each consisting
            // of the `port_lit` and _M_ link literals.

            let mut clauses = Vec::new();

            let num_monos = self.terms.len();
            let num_nodes = self.atomics.len();

            let clause_table_row = self.atomics.iter().map(|id| id.into_sat_literal(false));

            let mut clause_table: Vec<_> = self
                .terms
                .iter()
                .map(|mono| {
                    mono.iter()
                        .zip(clause_table_row.clone())
                        .map(|(bit, lit)| if bit { lit } else { !lit })
                        .collect()
                })
                .collect();

            clause_table.push(vec![port_lit]);

            let mut cursors = vec![0; num_monos + 1];
            let mut mono_ndx = 0;

            let mut num_tautologies = 0;
            let mut num_repeated_literals = 0;

            'outer: loop {
                let lits = cursors.iter().enumerate().map(|(row, &col)| clause_table[row][col]);

                if let Some(clause) = sat::Clause::from_literals_checked(lits, "polynomial subterm")
                {
                    num_repeated_literals += num_monos + 1 - clause.len();
                    clauses.push(clause);
                } else {
                    num_tautologies += 1;
                }

                'inner: loop {
                    if cursors[mono_ndx] < num_nodes - 1 {
                        cursors[mono_ndx] += 1;

                        for cursor in cursors.iter_mut().take(mono_ndx) {
                            *cursor = 0;
                        }
                        mono_ndx = 0;

                        break 'inner
                    } else if mono_ndx < num_monos - 1 {
                        cursors[mono_ndx] = 0;
                        mono_ndx += 1;
                    } else {
                        break 'outer
                    }
                }
            }

            info!(
                "Pushing {} polynomial subterm clauses (removed {} tautologies and {} repeated \
                 literals)",
                clauses.len(),
                num_tautologies,
                num_repeated_literals,
            );

            clauses
        }
    }
}

impl<T: Atomic + fmt::Debug> PartialEq for Polynomial<T> {
    fn eq(&self, other: &Self) -> bool {
        self.atomics == other.atomics && self.terms == other.terms
    }
}

impl<T: Atomic + fmt::Debug> Eq for Polynomial<T> {}

impl<T: Atomic + fmt::Debug> PartialOrd for Polynomial<T> {
    #[allow(clippy::collapsible_if)]
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        // FIXME optimize, handle errors

        if self.clone().add_polynomial(other).unwrap() {
            if other.clone().add_polynomial(self).unwrap() {
                None
            } else {
                Some(cmp::Ordering::Less)
            }
        } else {
            if other.clone().add_polynomial(self).unwrap() {
                Some(cmp::Ordering::Less)
            } else {
                Some(cmp::Ordering::Equal)
            }
        }
    }
}

impl Contextual for Polynomial<LinkID> {
    fn format(&self, ctx: &Context) -> Result<String, Box<dyn Error>> {
        let mut result = String::new();

        let mut first_mono = true;

        for mono in self.get_monomials() {
            if first_mono {
                first_mono = false;
            } else {
                result.push_str(" + ");
            }

            let mut first_id = true;

            for id in mono {
                if first_id {
                    first_id = false;
                } else {
                    result.push('·');
                }

                let link = ctx.get_link(id).ok_or(AcesError::LinkMissingForID(id))?;

                let s = if let Some(face) = self.dock {
                    match face {
                        node::Face::Tx => link.get_rx_node_id().format(ctx),
                        node::Face::Rx => link.get_tx_node_id().format(ctx),
                    }
                } else {
                    id.format(ctx)
                }?;

                result.push_str(&s);
            }
        }

        Ok(result)
    }
}

impl<'a> InContextMut<'a, Polynomial<LinkID>> {
    pub(crate) fn add_polynomial(&mut self, other: &Self) -> Result<bool, AcesError> {
        if self.get_context() == other.get_context() {
            self.get_thing_mut().add_polynomial(other.get_thing())
        } else {
            Err(AcesError::ContextMismatch)
        }
    }
}

impl<'a> ops::AddAssign<&Self> for InContextMut<'a, Polynomial<LinkID>> {
    fn add_assign(&mut self, other: &Self) {
        self.add_polynomial(other).unwrap();
    }
}

/// An iterator yielding monomials of a [`Polynomial`] as ordered
/// sequences of [`Atomic`] identifiers.
///
/// This is a two-level iterator: the yielded items are themselves
/// iterators.  It borrows the [`Polynomial`] being iterated over and
/// traverses its data in place, without allocating extra space for
/// inner iteration.
pub struct Monomials<'a, T: Atomic + fmt::Debug> {
    poly: &'a Polynomial<T>,
    ndx:  usize,
}

impl<'a, T: Atomic + fmt::Debug> Iterator for Monomials<'a, T> {
    #[allow(clippy::type_complexity)]
    type Item = iter::FilterMap<
        iter::Zip<slice::Iter<'a, T>, bit_vec::Iter<'a>>,
        fn((&T, bool)) -> Option<T>,
    >;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(row) = self.poly.terms.get(self.ndx) {
            fn doit<T: Atomic>((l, b): (&T, bool)) -> Option<T> {
                if b {
                    Some(*l)
                } else {
                    None
                }
            }

            let mono = self
                .poly
                .atomics
                .iter()
                .zip(row.iter())
                .filter_map(doit as fn((&T, bool)) -> Option<T>);

            self.ndx += 1;

            Some(mono)
        } else {
            None
        }
    }
}
