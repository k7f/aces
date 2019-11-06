use std::{collections::BTreeMap, iter::FromIterator, error::Error};
use crate::{Context, ContextHandle, Contextual, NodeID};

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

    pub fn num_tokens(&self, node_id: NodeID) -> u64 {
        self.tokens.get(&node_id).copied().unwrap_or(0)
    }
}

impl Contextual for State {
    fn format(&self, ctx: &Context) -> Result<String, Box<dyn Error>> {
        let mut result = String::new();
        let mut at_start = true;

        result.push('{');

        for (node_id, &num_tokens) in self.tokens.iter() {
            if num_tokens > 0 {
                if at_start {
                    at_start = false;
                } else {
                    result.push(',');
                }
                result.push_str(&format!(" {}: {}", node_id.format(ctx)?.as_str(), num_tokens));
            }
        }

        result.push_str(" }");

        Ok(result)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Semantics {
    Sequential,
    Parallel,
}

impl Default for Semantics {
    fn default() -> Self {
        Semantics::Sequential
    }
}
