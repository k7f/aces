use std::{collections::BTreeMap, iter::FromIterator};
use crate::{ContextHandle, NodeID};

#[derive(Default, Debug)]
pub struct State {
    tokens: BTreeMap<NodeID, u64>,
}

impl State {
    pub fn from_trigger<S: AsRef<str>>(ctx: &ContextHandle, trigger_name: S) -> Self {
        let trigger_id = ctx.lock().unwrap().share_node_name(trigger_name);
        let tokens = BTreeMap::from_iter(Some((trigger_id, 1)));

        State { tokens }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Semantics {
    Sequential,
    Parallel,
}
