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

    pub fn add_monomial(&mut self, mono: Monomial) {
        self.product.extend(mono.iter());
        self.sum.push((mono, Default::default()));

        for (mono, rest) in self.sum.iter_mut() {
            rest.extend(self.product.difference(&mono).copied());
        }
    }

    pub fn iter(&self) -> slice::Iter<(Monomial, Monomial)> {
        self.sum.iter()
    }
}
