use std::{slice, collections::BTreeSet};
use crate::atom::LinkID;

pub type Monomial = BTreeSet<LinkID>;

#[derive(Default, Debug)]
pub struct Polynomial {
    sum:     Vec<(Monomial, Monomial)>,
    product: Monomial,
}

impl Polynomial {
    pub fn new() -> Self {
        Default::default()
    }

    /// Adds a [`Monomial`] to this polynomial, unless it was already
    /// added (respecting idempotency).
    ///
    /// Also, updates the cached data in order to reflect the change:
    /// extends the set of links occuring in the polynomial, and
    /// extends all monomial complements.
    pub fn add_monomial(&mut self, mono: Monomial) {
        for (another, _) in self.sum.iter() {
            if *another == mono {
                return
            }
        }

        self.product.extend(mono.iter());
        self.sum.push((mono, Default::default()));

        for (another, rest) in self.sum.iter_mut() {
            rest.extend(self.product.difference(&another).copied());
        }
    }

    pub fn iter(&self) -> slice::Iter<(Monomial, Monomial)> {
        self.sum.iter()
    }
}
