use crate::nodes::NodeSpace;
use crate::atoms::{AtomSpace, Atom, Source, Sink, Link};

#[derive(Debug)]
pub struct Context {
    pub(crate) nodes: NodeSpace,
    pub(crate) atoms: AtomSpace,
}

impl Context {
    pub fn new() -> Self {
        Self {
            nodes: NodeSpace::new(),
            atoms: AtomSpace::new(),
        }
    }
}
