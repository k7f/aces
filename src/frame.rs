use std::{slice, iter, cmp, ops, fmt, collections::BTreeSet};
use bit_vec::BitVec;
use crate::{
    Polarity, DotId, Port, Link, LinkId, ContextHandle, Contextual, ExclusivelyContextual,
    InContextMut, Atomic, AcesError, AcesErrorKind, sat,
};

// FIXME sort rows as a way to fix Eq and, perhaps, to open some
// optimization opportunities.
/// Fuset's frame: a family of dotsets.  This also represents a single
/// equivalence class of formal polynomials.
///
/// Internally a `Frame` is represented as a vector `atomics` of _N_
/// [`Atomic`] identifiers and a boolean matrix with _N_ columns and
/// _M_ rows.  The `atomics` vector, sorted in strictly increasing
/// order, represents the fuset's range (or the set of dots occuring
/// in the polynomial) and _N_ is the number of dots: the frame's
/// width.
///
/// _M_ is the frame size (number of pits or number of monomials in
/// the canonical representation of a polynomial).  The order in which
/// pits (monomials) are listed is arbitrary.  An element in row _i_
/// and column _j_ of the matrix determines if a dot in _j_-th
/// position in the `atomics` vector of identifiers occurs in _i_-th
/// pit (monomial).
///
/// `Frame`s may be compared and added using traits from [`std::cmp`]
/// and [`std::ops`] standard modules, with the obvious exception of
/// [`std::cmp::Ord`].  Note however that, in general, preventing
/// [`Context`] or [`Port`] mismatch between frames is the
/// responsibility of the caller of an operation.  Implementation
/// detects some, but not all, cases of mismatch and panics if it does
/// so.
///
/// [`Context`]: crate::Context
#[derive(Clone, Debug)]
pub struct Frame<T: Atomic + fmt::Debug> {
    atomics:  Vec<T>,
    // FIXME choose a better representation of a boolean matrix.
    terms:    Vec<BitVec>,
    polarity: Option<Polarity>,
}

impl Frame<LinkId> {
    /// Creates a frame from a sequence of sequences of [`DotId`]s and
    /// in a [`Context`] given by a [`ContextHandle`].
    ///
    /// [`Context`]: crate::Context
    pub fn from_dots_in_context<'a, I>(
        ctx: &ContextHandle,
        polarity: Polarity,
        dot_id: DotId,
        pit_ids: I,
    ) -> Self
    where
        I: IntoIterator + 'a,
        I::Item: IntoIterator<Item = &'a DotId>,
    {
        let mut result = Self::new_oriented(polarity);
        let mut port = Port::new(polarity, dot_id);
        let pid = ctx.lock().unwrap().share_port(&mut port);
        let tip = port.get_dot_id();
        let polarity = port.get_polarity();

        let mut ctx = ctx.lock().unwrap();

        for dot_ids in pit_ids.into_iter() {
            let mut out_ids = BTreeSet::new();

            for cotip in dot_ids.into_iter().copied() {
                let mut coport = Port::new(!polarity, cotip);
                let copid = ctx.share_port(&mut coport);

                let mut link = match polarity {
                    Polarity::Tx => Link::new(pid, tip, copid, cotip),
                    Polarity::Rx => Link::new(copid, cotip, pid, tip),
                };
                let id = ctx.share_link(&mut link);

                out_ids.insert(id);
            }
            result.add_atomics_sorted(out_ids.into_iter()).unwrap();
        }

        result
    }
}

impl<T: Atomic + fmt::Debug> Frame<T> {
    /// Creates an empty frame, _&theta;_.
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self { atomics: Default::default(), terms: Default::default(), polarity: None }
    }

    /// Creates an empty frame, _&theta;_, oriented according to a
    /// given [`Polarity`].
    #[allow(clippy::new_without_default)]
    pub fn new_oriented(polarity: Polarity) -> Self {
        Self {
            atomics:  Default::default(),
            terms:    Default::default(),
            polarity: Some(polarity),
        }
    }

    /// Resets this frame into _&theta;_, preserving polarity, if any.
    pub fn clear(&mut self) {
        self.atomics.clear();
        self.terms.clear();
    }

    /// Multiplies this frame (all its pits) by a singleton pit.
    pub fn atomic_multiply(&mut self, atomic: T) {
        if self.is_empty() {
            self.atomics.push(atomic);

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
                    if !self.is_singular() {
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

    /// Adds a sequence of [`Atomic`] identifiers to this frame as
    /// another pit (monomial).
    ///
    /// On success, returns `true` if this frame changed or `false` if
    /// it didn't, due to idempotency of addition.
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
                trace!("Start {:?} with {:?}", self, thing);

                if let Some(prev) = prev_thing {
                    if thing <= prev {
                        self.atomics.clear();

                        return Err(AcesErrorKind::AtomicsNotOrdered.into())
                    }
                }
                prev_thing = Some(thing);

                self.atomics.push(thing);
            }

            if self.atomics.is_empty() {
                return Ok(false)
            } else {
                let row = BitVec::from_elem(self.atomics.len(), true);
                self.terms.push(row);

                return Ok(true)
            }
        }

        let mut new_row = BitVec::with_capacity(2 * self.atomics.len());

        let mut prev_thing = None;
        let mut no_new_things = true;

        // These are used for error recovery.
        let mut old_atomics = None;
        let mut old_terms = None;

        let mut ndx = 0;

        for thing in atomics.into_iter() {
            trace!("Grow {:?} with {:?}", self, thing);

            if let Some(prev) = prev_thing {
                if thing <= prev {
                    // Error recovery.

                    if let Some(old_atomics) = old_atomics {
                        self.atomics = old_atomics;
                    }

                    if let Some(old_terms) = old_terms {
                        self.terms = old_terms;
                    }

                    return Err(AcesErrorKind::AtomicsNotOrdered.into())
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

                    if old_atomics.is_none() {
                        old_atomics = Some(self.atomics.clone());
                    }

                    self.atomics.insert(ndx, thing);

                    let old_rows = self.terms.clone();

                    for (new_row, old_row) in self.terms.iter_mut().zip(old_rows.iter()) {
                        new_row.truncate(ndx);
                        new_row.push(false);
                        new_row.extend(old_row.iter().skip(ndx));
                    }

                    if old_terms.is_none() {
                        old_terms = Some(old_rows);
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

        self.terms.push(new_row);

        Ok(true)
    }

    /// Adds another `Frame` to this frame.
    ///
    /// On success, returns `true` if this frame changed or `false` if
    /// it didn't, due to idempotency of addition.
    ///
    /// Returns error in case of port mismatch, or if context mismatch
    /// was detected.
    pub(crate) fn add_frame(&mut self, other: &Self) -> Result<bool, AcesError> {
        if self.polarity == other.polarity {
            // FIXME optimize.  There are two special cases: when
            // `self` is a superframe, and when it is a subframe of
            // `other`.  First case is a nop; the second: clear
            // followed by clone.

            let mut changed = false;

            for pit in other.iter() {
                if self.add_atomics_sorted(pit)? {
                    changed = true;
                }
            }

            Ok(changed)
        } else {
            Err(AcesErrorKind::FramePolarityMismatch.into())
        }
    }

    pub fn is_empty(&self) -> bool {
        self.terms.is_empty()
    }

    pub fn is_atomic(&self) -> bool {
        self.atomics.len() == 1
    }

    // FIXME is_primitive(): test for injection

    pub fn is_singular(&self) -> bool {
        self.terms.len() == 1
    }

    pub fn width(&self) -> usize {
        self.atomics.len()
    }

    pub fn size(&self) -> usize {
        self.terms.len()
    }

    /// Creates a [`FrameIter`] iterator.
    pub fn iter(&self) -> FrameIter<T> {
        FrameIter { frame: self, ndx: 0 }
    }

    pub fn get_atomics(&self) -> slice::Iter<T> {
        self.atomics.iter()
    }

    /// Constructs the firing rule of this frame, the logical
    /// constraint imposed on firing components, if the frame is
    /// attached to the oriented (with polarity) dot represented by
    /// `port_lit`.  The rule is transformed into a CNF formula, a
    /// conjunction of disjunctions of [`sat::Literal`]s (a sequence
    /// of clauses).
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
        } else if self.is_singular() {
            // Consequence is a conjunction of _N_ >= 2 positive link
            // literals.  The rule is a sequence of _N_ two-literal
            // port-link clauses.

            self.atomics
                .iter()
                .map(|id| sat::Clause::from_pair(id.into_sat_literal(false), port_lit, "singular"))
                .collect()
        } else {
            // Consequence is a disjunction of _M_ >= 2 statements,
            // each being a conjunction of _N_ >= 2 positive and
            // negative (at least one of each) link literals.  The
            // rule (after being expanded to CNF by distribution) is a
            // sequence of _N_^_M_ port-link clauses, each consisting
            // of the `port_lit` and _M_ link literals.

            let mut clauses = Vec::new();

            let num_pits = self.terms.len();
            let num_dots = self.atomics.len();

            let clause_table_row = self.atomics.iter().map(|id| id.into_sat_literal(false));

            let mut clause_table: Vec<_> = self
                .terms
                .iter()
                .map(|pit| {
                    pit.iter()
                        .zip(clause_table_row.clone())
                        .map(|(bit, lit)| if bit { lit } else { !lit })
                        .collect()
                })
                .collect();

            clause_table.push(vec![port_lit]);

            let mut cursors = vec![0; num_pits + 1];
            let mut pit_ndx = 0;

            let mut num_tautologies = 0;
            let mut num_repeated_literals = 0;

            'outer: loop {
                let lits = cursors.iter().enumerate().map(|(row, &col)| clause_table[row][col]);

                if let Some(clause) = sat::Clause::from_literals_checked(lits, "frame subterm") {
                    num_repeated_literals += num_pits + 1 - clause.len();
                    clauses.push(clause);
                } else {
                    num_tautologies += 1;
                }

                'inner: loop {
                    if cursors[pit_ndx] < num_dots - 1 {
                        cursors[pit_ndx] += 1;

                        for cursor in cursors.iter_mut().take(pit_ndx) {
                            *cursor = 0;
                        }
                        pit_ndx = 0;

                        break 'inner
                    } else if pit_ndx < num_pits - 1 {
                        cursors[pit_ndx] = 0;
                        pit_ndx += 1;
                    } else {
                        break 'outer
                    }
                }
            }

            info!(
                "Pushing {} frame subterm clauses (removed {} tautologies and {} repeated \
                 literals)",
                clauses.len(),
                num_tautologies,
                num_repeated_literals,
            );

            clauses
        }
    }
}

impl<T: Atomic + fmt::Debug> PartialEq for Frame<T> {
    fn eq(&self, other: &Self) -> bool {
        self.atomics == other.atomics && self.terms == other.terms
    }
}

impl<T: Atomic + fmt::Debug> Eq for Frame<T> {}

impl<T: Atomic + fmt::Debug> PartialOrd for Frame<T> {
    #[allow(clippy::collapsible_if)]
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        // FIXME optimize, handle errors

        if self.clone().add_frame(other).unwrap() {
            if other.clone().add_frame(self).unwrap() {
                None
            } else {
                Some(cmp::Ordering::Less)
            }
        } else {
            if other.clone().add_frame(self).unwrap() {
                Some(cmp::Ordering::Less)
            } else {
                Some(cmp::Ordering::Equal)
            }
        }
    }
}

impl Contextual for Frame<LinkId> {
    fn format(&self, ctx: &ContextHandle) -> Result<String, AcesError> {
        let mut result = String::new();

        let mut first_pit = true;

        for pit in self.iter() {
            if first_pit {
                first_pit = false;
            } else {
                result.push_str(" + ");
            }

            let mut first_id = true;

            for id in pit {
                if first_id {
                    first_id = false;
                } else {
                    result.push('·');
                }

                let s = if let Some(polarity) = self.polarity {
                    let ctx = ctx.lock().unwrap();
                    let link = ctx
                        .get_link(id)
                        .ok_or_else(|| AcesError::from(AcesErrorKind::LinkMissingForId(id)))?;

                    match polarity {
                        Polarity::Tx => link.get_rx_dot_id().format_locked(&ctx),
                        Polarity::Rx => link.get_tx_dot_id().format_locked(&ctx),
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

impl<'a> InContextMut<'a, Frame<LinkId>> {
    pub(crate) fn add_frame(&mut self, other: &Self) -> Result<bool, AcesError> {
        if self.same_context(other) {
            self.get_thing_mut().add_frame(other.get_thing())
        } else {
            Err(AcesErrorKind::ContextMismatch.with_context(self.get_context()))
        }
    }
}

impl<'a> ops::AddAssign<&Self> for InContextMut<'a, Frame<LinkId>> {
    fn add_assign(&mut self, other: &Self) {
        self.add_frame(other).unwrap();
    }
}

/// An iterator yielding pits (monomials) of a [`Frame`] as ordered
/// sequences of [`Atomic`] identifiers.
///
/// This is a two-level iterator: the yielded items are themselves
/// iterators.  It borrows the [`Frame`] being iterated over and
/// traverses its data in place, without allocating extra space for
/// inner iteration.
pub struct FrameIter<'a, T: Atomic + fmt::Debug> {
    frame: &'a Frame<T>,
    ndx:   usize,
}

impl<'a, T: Atomic + fmt::Debug> Iterator for FrameIter<'a, T> {
    #[allow(clippy::type_complexity)]
    type Item = iter::FilterMap<
        iter::Zip<slice::Iter<'a, T>, bit_vec::Iter<'a>>,
        fn((&T, bool)) -> Option<T>,
    >;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(row) = self.frame.terms.get(self.ndx) {
            fn doit<T: Atomic>((l, b): (&T, bool)) -> Option<T> {
                if b {
                    Some(*l)
                } else {
                    None
                }
            }

            let pit = self
                .frame
                .atomics
                .iter()
                .zip(row.iter())
                .filter_map(doit as fn((&T, bool)) -> Option<T>);

            self.ndx += 1;

            Some(pit)
        } else {
            None
        }
    }
}
