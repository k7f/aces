use std::{
    collections::{btree_map, BTreeMap},
    iter::FromIterator,
    error::Error,
};
use log::Level::{Debug, Trace};
use crate::{Multiplicity, ContextHandle, Contextual, NodeID, FiringSet, AcesError};

#[derive(Clone, Default, Debug)]
pub struct State {
    tokens: BTreeMap<NodeID, Multiplicity>,
}

impl State {
    pub fn from_trigger<S: AsRef<str>>(ctx: &ContextHandle, trigger_name: S) -> Self {
        let trigger_id = ctx.lock().unwrap().share_node_name(trigger_name);
        let tokens = BTreeMap::from_iter(Some((trigger_id, Multiplicity::one())));

        State { tokens }
    }

    pub fn clear(&mut self) {
        self.tokens.clear()
    }

    pub fn get(&self, node_id: NodeID) -> Multiplicity {
        self.tokens.get(&node_id).copied().unwrap_or_else(Multiplicity::zero)
    }

    pub fn set(&mut self, node_id: NodeID, num_tokens: Multiplicity) {
        match self.tokens.entry(node_id) {
            btree_map::Entry::Vacant(entry) => {
                if num_tokens.is_positive() {
                    entry.insert(num_tokens);
                }
            }
            btree_map::Entry::Occupied(mut entry) => {
                if num_tokens.is_positive() {
                    *entry.get_mut() = num_tokens;
                } else {
                    entry.remove();
                }
            }
        }
    }

    pub fn decrease(&mut self, node_id: NodeID, num_tokens: Multiplicity) -> Result<(), AcesError> {
        if num_tokens.is_positive() {
            if let btree_map::Entry::Occupied(mut entry) = self.tokens.entry(node_id) {
                let tokens_before = *entry.get_mut();

                if tokens_before.is_positive() {
                    if num_tokens.is_finite() {
                        if let Some(tokens_after) = tokens_before.checked_sub(num_tokens) {
                            *entry.get_mut() = tokens_after;
                        } else {
                            return Err(AcesError::StateUnderflow)
                        }
                    } else {
                        return Err(AcesError::LeakedInhibitor)
                    }
                } else if num_tokens.is_finite() {
                    return Err(AcesError::StateUnderflow)
                }
            } else {
                return Err(AcesError::StateUnderflow)
            }
        }

        Ok(())
    }

    pub fn increase(&mut self, node_id: NodeID, num_tokens: Multiplicity) -> Result<(), AcesError> {
        if num_tokens.is_positive() {
            match self.tokens.entry(node_id) {
                btree_map::Entry::Vacant(entry) => {
                    entry.insert(num_tokens);
                }
                btree_map::Entry::Occupied(mut entry) => {
                    let tokens_before = *entry.get_mut();

                    if tokens_before.is_zero() {
                        *entry.get_mut() = num_tokens;
                    } else if tokens_before.is_finite() {
                        if num_tokens.is_omega() {
                            *entry.get_mut() = num_tokens;
                        } else if let Some(tokens_after) = tokens_before.checked_add(num_tokens) {
                            if tokens_after.is_finite() {
                                *entry.get_mut() = tokens_after;
                            } else {
                                return Err(AcesError::StateOverflow)
                            }
                        } else {
                            return Err(AcesError::StateOverflow)
                        }
                    }
                }
            }
        }

        Ok(())
    }

    pub(crate) fn transition_debug<R: rand::RngCore>(
        &mut self,
        ctx: &ContextHandle,
        num_steps: usize,
        fset: &FiringSet,
        rng: &mut R,
    ) -> Result<Option<usize>, AcesError> {
        if log_enabled!(Debug) {
            if num_steps == 0 {
                debug!("Go from {}", self.with(ctx));
            } else if num_steps < 10 {
                debug!("Step {}  {}", num_steps, self.with(ctx));
            } else {
                debug!("Step {} {}", num_steps, self.with(ctx));
            }
        }

        let enabled_fcs = fset.get_enabled(ctx, self);

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

            fset.as_slice()[fc_id].fire(ctx, self)?;

            Ok(Some(fc_id))
        } else {
            Ok(None)
        }
    }

    /// Returns ID of the activated firing component taken from the
    /// given [`FiringSet`].
    pub fn transition<R: rand::RngCore>(
        &mut self,
        ctx: &ContextHandle,
        fset: &FiringSet,
        rng: &mut R,
    ) -> Result<Option<usize>, AcesError> {
        let enabled_fcs = fset.get_enabled(ctx, self);

        if let Some(fc_id) = enabled_fcs.get_random(rng) {
            fset.as_slice()[fc_id].fire(ctx, self)?;

            Ok(Some(fc_id))
        } else {
            Ok(None)
        }
    }
}

impl Contextual for State {
    fn format(&self, ctx: &ContextHandle) -> Result<String, Box<dyn Error>> {
        let mut result = String::new();
        let mut at_start = true;

        result.push('{');

        for (node_id, &num_tokens) in self.tokens.iter() {
            if num_tokens.is_positive() {
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
