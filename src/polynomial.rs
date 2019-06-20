use std::{slice, collections::BTreeSet};
use bit_vec::BitVec;
use crate::{ID, NodeID, Port, PortID, Link, LinkID, ContextHandle, node, monomial, sat};

/// A formal polynomial.
///
/// Internally a `Polynomial` is represented as a vector of _N_
/// [`LinkID`]s, a vector of _M_ [`monomial::Weight`]s, and a boolean
/// matrix with _N_ columns and _M_ rows.  Vector of [`LinkID`]s is
/// sorted in strictly increasing order.  It represents the set of
/// nodes occuring in the polynomial and _N_ is the number of such
/// nodes.
///
/// _M_ is the number of monomials in the canonical representation of
/// the polynomial.  The order in which monomials are listed is
/// arbitrary, but same for rows of the boolean matrix and elements of
/// the weight vector.  An element in row _i_ and column _j_ of the
/// matrix determines if node in _j_-th position in the links vector
/// occurs in _i_-th monomial.
#[derive(Default, Debug)]
pub struct Polynomial {
    product: Vec<LinkID>,
    weights: Vec<monomial::Weight>,
    // FIXME choose a better representation of a boolean matrix.
    sum: Vec<BitVec>,
}

impl Polynomial {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn from_spec(
        ctx: ContextHandle,
        port: &Port,
        spec_poly: &[Vec<ID>],
    ) -> Self {
        let mut result = Self::new();

        let node_id = port.get_node_id();
        let face = port.get_face();
        let port_id = PortID(port.get_atom_id());

        let mut mono_links = BTreeSet::new();

        let mut ctx = ctx.lock().unwrap();

        for spec_mono in spec_poly {
            mono_links.clear();

            for conode_id in spec_mono {
                let conode_id = NodeID(*conode_id);
                let mut coport = Port::new(!face, conode_id);
                let coport_id = ctx.share_port(&mut coport);

                let mut link = match face {
                    node::Face::Tx => Link::new(port_id, node_id, coport_id, conode_id),
                    node::Face::Rx => Link::new(coport_id, conode_id, port_id, node_id),
                };
                let link_id = ctx.share_link(&mut link);

                mono_links.insert(link_id);
            }
            result.add_links_sorted(&mono_links);
        }

        result
    }

    /// Resets this polynomial into _&theta;_.
    pub fn clear(&mut self) {
        self.product.clear();
        self.weights.clear();
        self.sum.clear();
    }

    /// Multiplies this polynomial (all its monomials) by a
    /// single-link monomial.
    pub fn multiply_by_link(&mut self, link_id: LinkID) {
        if self.is_empty() {
            self.product.push(link_id);
            self.weights.push(Default::default());

            let mut row = BitVec::new();
            row.push(true);
            self.sum.push(row);
        } else if link_id > *self.product.last().unwrap() {
            self.product.push(link_id);

            for row in self.sum.iter_mut() {
                row.push(true);
            }
        } else {
            match self.product.binary_search(&link_id) {
                Ok(ndx) => {
                    if !self.is_monomial() {
                        for row in self.sum.iter_mut() {
                            row.set(ndx, true);
                        }
                    }
                }
                Err(ndx) => {
                    let old_len = self.product.len();

                    self.product.insert(ndx, link_id);

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

    /// Adds a sequence of `links` to this polynomial as another
    /// monomial.
    ///
    /// Panics if `links` aren't given in strictly increasing order.
    pub fn add_links_sorted<'a, I>(&mut self, links: I)
    where
        I: IntoIterator<Item = &'a LinkID>,
    {
        let mut row = BitVec::new();
        let mut last_num = 0;
        let mut ndx = 0;

        for &link_id in links.into_iter() {
            let this_num = link_id.get().get();
            assert!(
                this_num > last_num,
                "Argument `links` have to be given in strictly increasing order."
            );

            match self.product[ndx..].binary_search(&link_id) {
                Ok(i) => {
                    row.push(true);
                    ndx = i + 1;
                }
                Err(i) => {
                    row.push(true);
                    row.push(false);
                    self.product.insert(i, link_id);
                    ndx = i + 1;
                }
            }

            last_num = this_num;
        }

        self.weights.push(Default::default());
        self.sum.push(row);
    }

    /// Adds another `Polynomial` to this polynomial.
    ///
    /// There are two special cases: when `self` is a superpolynomial,
    /// and when it is a subpolynomial of `other`.  First case is a
    /// nop; the second: clear followed by clone.
    pub fn add_polynomial(&mut self, _other: &Self) {
        // FIXME
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

    pub fn get_links(&self) -> slice::Iter<LinkID> {
        self.product.iter()
    }

    pub fn as_sat_clauses(&self) -> Vec<Vec<sat::Literal>> {
        let mut clauses = Vec::new();

        let mono_template: Vec<_> =
            self.product.iter().map(|link_id| link_id.as_sat_literal(true)).collect();

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
