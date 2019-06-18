use std::{slice, collections::BTreeSet};
use crate::atom::LinkID;

#[derive(Default, Debug)]
pub struct Polynomial {
    product: BTreeSet<LinkID>,
    links:   Vec<LinkID>,
    sum:     Vec<(BTreeSet<LinkID>, BTreeSet<LinkID>)>
}

impl Polynomial {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn clear(&mut self) {
        self.product.clear();
        self.links.clear();
        self.sum.clear();
    }

    pub fn multiply_by_link(&mut self, link_id: LinkID) {
        if self.is_empty() {
            self.product.insert(link_id);
            self.links.push(link_id);
            self.sum.push((self.product.clone(), Default::default()));
        } else {
            if self.product.contains(&link_id) {
                if self.is_monomial() {
                    return
                }
            } else {
                self.product.insert(link_id);

                // FIXME
                self.links.clear();
                self.links.extend(self.product.iter());
            }

            for (mono, _) in self.sum.iter_mut() {
                mono.insert(link_id);
            }
        }
    }

    // FIXME
    /// Adds another `Polynomial` to this polynomial.
    ///
    /// Also, updates the cached data in order to reflect the change:
    /// extends the set of links occuring in the polynomial, and
    /// extends all monomial complements.
    ///
    /// There are two special cases: when `self` is a superpolynomial,
    /// and when it is a subpolynomial of `other`.  First case is a
    /// nop; the second: clear followed by clone.
    pub fn add_polynomial(&mut self, other: &Self) {
        if other.is_monomial() {
            for (mono, _) in self.sum.iter() {
                if *mono == other.product {
                    return
                }
            }

            self.product.extend(other.product.iter());

            // FIXME
            self.links.clear();
            self.links.extend(self.product.iter());

            self.sum.push((other.product.clone(), Default::default()));

            for (mono, rest) in self.sum.iter_mut() {
                rest.extend(self.product.difference(&mono).copied());
            }
        } else {
            // FIXME
        }
    }

    pub fn is_empty(&self) -> bool {
        self.sum.is_empty()
    }

    pub fn is_single_link(&self) -> bool {
        self.links.len() == 1
    }

    pub fn is_monomial(&self) -> bool {
        // FIXME
        // self.sum.len() == self.links.len()

        self.sum.len() == 1
    }

    pub fn iter(&self) -> slice::Iter<(BTreeSet<LinkID>, BTreeSet<LinkID>)> {
        self.sum.iter()
    }
}
