use std::{
    collections::{btree_map, BTreeMap},
    iter::FromIterator,
    error::Error,
};
use log::Level::{Debug, Trace};
use crate::{ContextHandle, Contextual, NodeID, FiringSet};

#[derive(Clone, Default, Debug)]
pub struct State {
    tokens: BTreeMap<NodeID, u64>,
}

impl State {
    pub fn from_trigger<S: AsRef<str>>(ctx: &ContextHandle, trigger_name: S) -> Self {
        let trigger_id = ctx.lock().unwrap().share_node_name(trigger_name);
        let tokens = BTreeMap::from_iter(Some((trigger_id, 1)));

        State { tokens }
    }

    pub fn clear(&mut self) {
        self.tokens.clear()
    }

    pub fn get(&self, node_id: NodeID) -> u64 {
        self.tokens.get(&node_id).copied().unwrap_or(0)
    }

    pub fn set(&mut self, node_id: NodeID, num_tokens: u64) {
        match self.tokens.entry(node_id) {
            btree_map::Entry::Vacant(entry) => {
                if num_tokens > 0 {
                    entry.insert(num_tokens);
                }
            }
            btree_map::Entry::Occupied(mut entry) => {
                if num_tokens > 0 {
                    *entry.get_mut() = num_tokens;
                } else {
                    entry.remove();
                }
            }
        }
    }

    pub(crate) fn transition_debug<R: rand::RngCore>(
        &mut self,
        ctx: &ContextHandle,
        num_steps: usize,
        fset: &FiringSet,
        rng: &mut R,
    ) -> Option<usize> {
        if log_enabled!(Debug) {
            if num_steps == 0 {
                debug!("Go from {}", self.with(ctx));
            } else if num_steps < 10 {
                debug!("Step {}  {}", num_steps, self.with(ctx));
            } else {
                debug!("Step {} {}", num_steps, self.with(ctx));
            }
        }

        let enabled_fcs = fset.get_enabled(self);

        if let Some(fc_id) = enabled_fcs.get_random(rng) {
            if log_enabled!(Trace) {
                let mut at_start = true;

                for fc in enabled_fcs.iter(fset) {
                    if at_start {
                        trace!("Enabled {}", fc.with(ctx));
                        at_start = false;
                    } else {
                        trace!("        {}", fc.with(ctx));
                    }
                }
            }

            fset.as_slice()[fc_id].fire(self);

            Some(fc_id)
        } else {
            None
        }
    }

    /// Returns ID of the activated firing component taken from the
    /// given [`FiringSet`].
    pub fn transition<R: rand::RngCore>(&mut self, fset: &FiringSet, rng: &mut R) -> Option<usize> {
        let enabled_fcs = fset.get_enabled(self);

        if let Some(fc_id) = enabled_fcs.get_random(rng) {
            fset.as_slice()[fc_id].fire(self);

            Some(fc_id)
        } else {
            None
        }
    }
}

impl Contextual for State {
    fn format(&self, ctx: &ContextHandle) -> Result<String, Box<dyn Error>> {
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
