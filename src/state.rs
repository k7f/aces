use std::collections::BTreeMap;
use crate::NodeID;

#[derive(Default, Debug)]
pub struct State {
    tokens: BTreeMap<NodeID, u64>,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Semantics {
    Sequential,
    Parallel,
}
