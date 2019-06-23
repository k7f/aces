use std::{slice, iter, cmp, ops, collections::BTreeSet, error::Error};
use bit_vec::BitVec;
use crate::{
    ID, NodeID, Port, PortID, Link, LinkID, Context, ContextHandle, Contextual, Atomic, node, monomial, sat, error::AcesError,
};

// FIXME remove: to be used only during refactoring.
#[derive(Clone, Debug)]
struct Anchor {
    pid: PortID,
    host: NodeID,
    face: node::Face,
}

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
    anchor: Option<Anchor>,
}

impl Polynomial<LinkID> {
    /// Creates a polynomial from a sequence of vectors of node
    /// [`ID`]s and in a [`Context`] given by a [`ContextHandle`].
    pub fn from_spec(ctx: ContextHandle, port: &Port, spec_poly: &[Vec<ID>]) -> Self {
        let mut result = Self::new();

        let pid = PortID(port.get_atom_id());
        let host = port.get_node_id();
        let face = port.get_face();

        result.anchor = Some(Anchor {
            pid, host, face,
        });

        let mut ctx = ctx.lock().unwrap();

        for spec_mono in spec_poly {
            let mut mono_ids = BTreeSet::new();

            for cohost in spec_mono {
                let cohost = NodeID(*cohost);
                let mut coport = Port::new(!face, cohost);
                let copid = ctx.share_port(&mut coport);

                let mut link = match face {
                    node::Face::Tx => Link::new(pid, host, copid, cohost),
                    node::Face::Rx => Link::new(copid, cohost, pid, host),
                };
                let id = ctx.share_link(&mut link);

                mono_ids.insert(id);
            }
            result.add_links_sorted(mono_ids.into_iter()).unwrap();
        }

        result
    }
}

impl<T: Atomic> Polynomial<T> {
    /// Creates an empty polynomial, _&theta;_.
    pub fn new() -> Self {
        Self {
            product: Default::default(),
            weights: Default::default(),
            sum: Default::default(),
            anchor: Default::default(),
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
    pub fn multiply_by_link(&mut self, id: T) {
        if self.is_empty() {
            self.product.push(id);
            self.weights.push(Default::default());

            let mut row = BitVec::new();
            row.push(true);
            self.sum.push(row);
        } else if id > *self.product.last().unwrap() {
            self.product.push(id);

            for row in self.sum.iter_mut() {
                row.push(true);
            }
        } else {
            match self.product.binary_search(&id) {
                Ok(ndx) => {
                    if !self.is_monomial() {
                        for row in self.sum.iter_mut() {
                            row.set(ndx, true);
                        }
                    }
                }
                Err(ndx) => {
                    let old_len = self.product.len();

                    self.product.insert(ndx, id);

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
    pub fn add_links_sorted<I>(&mut self, ids: I) -> Result<bool, Box<dyn Error>>
    where
        I: IntoIterator<Item = T>,
    {
        if self.is_empty() {
            let mut last_lid = None;

            for lid in ids.into_iter() {
                if let Some(last_id) = last_lid {
                    if lid <= last_id {
                        self.product.clear();

                        return Err(Box::new(AcesError::LinksNotOrdered))
                    }
                }
                last_lid = Some(lid);

                self.product.push(lid);
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

        let mut last_id = None;
        let mut no_new_links = true;

        // These are used for error recovery; `old_sum` also helps in
        // the implementation of inserting extra bits into rows of the
        // sum.
        let mut old_product = None;
        let mut old_sum = None;

        let mut ndx = 0;

        for id in ids.into_iter() {
            if let Some(last_id) = last_id {
                if id <= last_id {
                    // Error recovery.

                    if let Some(old_product) = old_product {
                        self.product = old_product;
                    }

                    if let Some(old_sum) = old_sum {
                        self.sum = old_sum;
                    }

                    return Err(Box::new(AcesError::LinksNotOrdered))
                }
            }
            last_id = Some(id);

            match self.product[ndx..].binary_search(&id) {
                Ok(i) => {
                    new_row.push(true);
                    ndx += i + 1;
                }
                Err(i) => {
                    ndx += i;

                    no_new_links = false;

                    new_row.push(true);
                    new_row.push(false);

                    self.product.insert(ndx, id);

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

        if no_new_links {
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
            if self.add_links_sorted(mono)? {
                changed = true;
            }
        }

        Ok(changed)
    }

    pub fn is_empty(&self) -> bool {
        self.sum.is_empty()
    }

    pub fn is_single_link(&self) -> bool {
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

    pub fn get_links(&self) -> slice::Iter<T> {
        self.product.iter()
    }

    pub fn as_sat_clauses(&self) -> Vec<Vec<sat::Literal>> {
        let mut clauses = Vec::new();

        let mono_template: Vec<_> =
            self.product.iter().map(|id| id.into_sat_literal(true)).collect();

        for row in self.sum.iter() {
            let mut mono = mono_template.clone();

            for (i, lit) in mono.iter_mut().enumerate() {
                if row[i] {
                    *lit = !*lit
                }
            }
            clauses.push(mono)
        }
        clauses
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

impl<T: Atomic> ops::AddAssign<&Self> for Polynomial<T> {
    fn add_assign(&mut self, other: &Self) {
        self.add_polynomial(other).unwrap();
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

                let link = ctx.get_link(id)
                    .ok_or(AcesError::LinkMissingForID)?;

                let s = if let Some(ref anchor) = self.anchor {
                    match anchor.face {
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
