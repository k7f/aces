use std::{
    slice, iter, cmp, ops,
    collections::{BTreeSet, HashSet},
    error::Error,
};
use bit_vec::BitVec;
use crate::{
    NodeID, Port, PortID, Link, LinkID, Context, ContextHandle, Contextual, InContextMut, Atomic,
    node, monomial, sat, error::AcesError,
};

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
/// preventing [`Context`] or [`Port`] mismatch between polynomials
/// is the responsibility of the caller of an operation.
/// Implementation detects some, but not all, cases of mismatch and
/// panics if it does so.
#[derive(Clone, Debug)]
pub struct Polynomial<T: Atomic> {
    product: Vec<T>,
    weights: Vec<monomial::Weight>,
    // FIXME choose a better representation of a boolean matrix.
    sum: Vec<BitVec>,
}

impl Polynomial<LinkID> {
    /// Creates a polynomial from a sequence of vectors of
    /// [`NodeID`]s and in a [`Context`] given by a [`ContextHandle`].
    pub fn from_port_and_ids(ctx: ContextHandle, port: &Port, poly_ids: &[Vec<NodeID>]) -> Self {
        let mut result = Self::new();

        let pid = PortID(port.get_atom_id());
        let host = port.get_node_id();
        let face = port.get_face();

        let mut ctx = ctx.lock().unwrap();

        for mono_ids in poly_ids {
            let mut out_ids = BTreeSet::new();

            for cohost in mono_ids {
                let cohost = *cohost;
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

impl<T: Atomic> Polynomial<T> {
    /// Creates an empty polynomial, _&theta;_.
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            product: Default::default(),
            weights: Default::default(),
            sum:     Default::default(),
        }
    }

    /// Resets this polynomial into _&theta;_.
    pub fn clear(&mut self) {
        self.product.clear();
        self.weights.clear();
        self.sum.clear();
    }

    /// Multiplies this polynomial (all its monomials) by a
    /// single-element monomial.
    pub fn atomic_multiply(&mut self, atomic: T) {
        if self.is_empty() {
            self.product.push(atomic);
            self.weights.push(Default::default());

            let mut row = BitVec::new();
            row.push(true);
            self.sum.push(row);
        } else if atomic > *self.product.last().unwrap() {
            self.product.push(atomic);

            for row in self.sum.iter_mut() {
                row.push(true);
            }
        } else {
            match self.product.binary_search(&atomic) {
                Ok(ndx) => {
                    if !self.is_monomial() {
                        for row in self.sum.iter_mut() {
                            row.set(ndx, true);
                        }
                    }
                }
                Err(ndx) => {
                    let old_len = self.product.len();

                    self.product.insert(ndx, atomic);

                    for row in self.sum.iter_mut() {
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
    /// Returns error if `ids` aren't given in strictly increasing
    /// order, or in case of port mismatch, or if context mismatch
    /// was detected.
    pub fn add_atomics_sorted<I>(&mut self, atomics: I) -> Result<bool, Box<dyn Error>>
    where
        I: IntoIterator<Item = T>,
    {
        if self.is_empty() {
            let mut prev_thing = None;

            for thing in atomics.into_iter() {
                if let Some(prev) = prev_thing {
                    if thing <= prev {
                        self.product.clear();

                        return Err(Box::new(AcesError::AtomicsNotOrdered))
                    }
                }
                prev_thing = Some(thing);

                self.product.push(thing);
            }

            if self.product.is_empty() {
                return Ok(false)
            } else {
                self.weights.push(Default::default());

                let row = BitVec::from_elem(self.product.len(), true);
                self.sum.push(row);

                return Ok(true)
            }
        }

        let mut new_row = BitVec::from_elem(self.product.len(), false);

        let mut prev_thing = None;
        let mut no_new_things = true;

        // These are used for error recovery; `old_sum` also helps in
        // the implementation of inserting extra bits into rows of the
        // sum.
        let mut old_product = None;
        let mut old_sum = None;

        let mut ndx = 0;

        for thing in atomics.into_iter() {
            if let Some(prev) = prev_thing {
                if thing <= prev {
                    // Error recovery.

                    if let Some(old_product) = old_product {
                        self.product = old_product;
                    }

                    if let Some(old_sum) = old_sum {
                        self.sum = old_sum;
                    }

                    return Err(Box::new(AcesError::AtomicsNotOrdered))
                }
            }
            prev_thing = Some(thing);

            match self.product[ndx..].binary_search(&thing) {
                Ok(i) => {
                    new_row.push(true);
                    ndx += i + 1;
                }
                Err(i) => {
                    ndx += i;

                    no_new_things = false;

                    new_row.push(true);
                    new_row.push(false);

                    self.product.insert(ndx, thing);

                    if old_product.is_none() {
                        old_product = Some(self.product.clone());
                    }
                    if old_sum.is_none() {
                        old_sum = Some(self.sum.clone());
                    }

                    let all_rows = old_sum.as_ref().unwrap();

                    for (row, tail_row) in self.sum.iter_mut().zip(all_rows.iter()) {
                        row.truncate(ndx);
                        row.push(false);
                        row.extend(tail_row.iter().skip(ndx));
                    }

                    ndx += 1;
                }
            }
        }

        if no_new_things {
            for row in self.sum.iter() {
                if *row == new_row {
                    return Ok(false)
                }
            }
        }

        self.weights.push(Default::default());
        self.sum.push(new_row);

        Ok(true)
    }

    /// Adds another `Polynomial` to this polynomial.
    ///
    /// On success, returns `true` if this polynomial changed or
    /// `false` if it didn't, due to idempotency of addition.
    ///
    /// Returns error in case of port mismatch, or if context mismatch
    /// was detected.
    pub(crate) fn add_polynomial(&mut self, other: &Self) -> Result<bool, Box<dyn Error>> {
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
    }

    pub fn is_empty(&self) -> bool {
        self.sum.is_empty()
    }

    pub fn is_atomic(&self) -> bool {
        self.product.len() == 1
    }

    pub fn is_monomial(&self) -> bool {
        self.sum.len() == 1
    }

    pub fn num_monomials(&self) -> usize {
        self.sum.len()
    }

    pub fn get_monomials(&self) -> Monomials<T> {
        Monomials { poly: self, ndx: 0 }
    }

    pub fn get_atomics(&self) -> slice::Iter<T> {
        self.product.iter()
    }

    /// Constructs the firing rule of this polynomial, the logical
    /// constraint imposed on firing components, if the polynomial is
    /// attached to the node and face represented by `port_lit`.  The
    /// rule is transformed into a CNF formula, a conjunction of
    /// disjunctions of [`sat::Literal`]s (a sequence of clauses).
    ///
    /// Returns a sequence of clauses in the form of vector of vectors
    /// of [`sat::Literal`]s.
    pub fn as_sat_clauses(&self, port_lit: sat::Literal) -> Vec<Vec<sat::Literal>> {
        if self.is_atomic() {
            // Consequence is a single positive link literal.  The
            // rule is a single two-literal port-link clause.

            vec![vec![self.product[0].into_sat_literal(false), port_lit]]
        } else if self.is_monomial() {
            // Consequence is a conjunction of _N_ >= 2 positive link
            // literals.  The rule is a sequence of _N_ two-literal
            // port-link clauses.

            self.product.iter().map(|id| vec![id.into_sat_literal(false), port_lit]).collect()
        } else {
            // Consequence is a disjunction of _M_ >= 2 statements,
            // each being a conjunction of _N_ >= 2 positive and
            // negative (at least one of each) link literals.  The
            // rule (after being expanded to CNF by distribution) is a
            // sequence of _N_^_M_ port-link clauses, each consisting
            // of the `port_lit` and _M_ link literals.

            let mut clauses = Vec::new();

            let num_monos = self.sum.len();
            let num_nodes = self.product.len();

            let clause_table_row = self.product.iter().map(|id| id.into_sat_literal(false));

            let mut clause_table: Vec<_> = self
                .sum
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
                let clause = cursors.iter().enumerate().map(|(row, &col)| clause_table[row][col]);

                let positives: HashSet<_> =
                    clause.clone().filter_map(|lit| lit.into_variable_if_positive()).collect();
                let negatives: HashSet<_> =
                    clause.filter_map(|lit| lit.into_variable_if_negative()).collect();

                if positives.is_disjoint(&negatives) {
                    let clause: Vec<_> = positives
                        .into_iter()
                        .map(|var| sat::Literal::from_variable(var, false))
                        .chain(
                            negatives.into_iter().map(|var| sat::Literal::from_variable(var, true)),
                        )
                        .collect();

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
                "Pushed {} clauses, removed {} tautologies and {} repeated literals",
                clauses.len(),
                num_tautologies,
                num_repeated_literals,
            );

            clauses
        }
    }
}

impl<T: Atomic> cmp::PartialEq for Polynomial<T> {
    fn eq(&self, other: &Self) -> bool {
        self.product == other.product && self.sum == other.sum
    }
}

impl<T: Atomic> cmp::Eq for Polynomial<T> {}

impl<T: Atomic> cmp::PartialOrd for Polynomial<T> {
    #[allow(clippy::collapsible_if)]
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        // FIXME optimize

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
    fn format(&self, ctx: &Context, dock: Option<node::Face>) -> Result<String, Box<dyn Error>> {
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

                let link = ctx.get_link(id).ok_or(AcesError::LinkMissingForID)?;

                let s = if let Some(face) = dock {
                    match face {
                        node::Face::Tx => link.get_rx_node_id().format(ctx, dock),
                        node::Face::Rx => link.get_tx_node_id().format(ctx, dock),
                    }
                } else {
                    id.format(ctx, dock)
                }?;

                result.push_str(&s);
            }
        }

        Ok(result)
    }
}

impl<'a> InContextMut<'a, Polynomial<LinkID>> {
    pub(crate) fn add_polynomial(&mut self, other: &Self) -> Result<bool, Box<dyn Error>> {
        if self.get_context() == other.get_context() {
            if self.get_dock() == other.get_dock() {
                self.get_thing_mut().add_polynomial(other.get_thing())
            } else {
                Err(Box::new(AcesError::PortMismatch))
            }
        } else {
            Err(Box::new(AcesError::ContextMismatch))
        }
    }
}

impl<'a> ops::AddAssign<&Self> for InContextMut<'a, Polynomial<LinkID>> {
    fn add_assign(&mut self, other: &Self) {
        self.add_polynomial(other).unwrap();
    }
}

/// An iterator yielding monomials of a [`Polynomial`] as sequences of
/// [`Atomic`] identifiers.
///
/// This is a two-level iterator: the yielded items are themselves
/// iterators.  It borrows the [`Polynomial`] being iterated over and
/// traverses its data in place, without allocating extra space for
/// inner iteration.
pub struct Monomials<'a, T: Atomic> {
    poly: &'a Polynomial<T>,
    ndx:  usize,
}

impl<'a, T: Atomic> Iterator for Monomials<'a, T> {
    #[allow(clippy::type_complexity)]
    type Item = iter::FilterMap<
        iter::Zip<slice::Iter<'a, T>, bit_vec::Iter<'a>>,
        fn((&T, bool)) -> Option<T>,
    >;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(row) = self.poly.sum.get(self.ndx) {
            fn doit<T: Atomic>((l, b): (&T, bool)) -> Option<T> {
                if b {
                    Some(*l)
                } else {
                    None
                }
            }

            let mono = self
                .poly
                .product
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
