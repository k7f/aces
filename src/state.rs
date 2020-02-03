use std::{
    collections::{btree_map, BTreeMap},
    iter::FromIterator,
    fmt,
};
use log::Level::{Debug, Trace};
use crate::{ContextHandle, Contextual, Multiplicity, NodeID, FiringSet, AcesError, AcesErrorKind};

#[derive(Clone, Debug)]
pub struct State {
    context: ContextHandle,
    tokens:  BTreeMap<NodeID, Multiplicity>,
}

impl State {
    pub fn from_triggers_saturated<S, I>(ctx: &ContextHandle, triggers: I) -> Self
    where
        S: AsRef<str>,
        I: IntoIterator<Item = (S, Multiplicity)>,
    {
        let context = ctx.clone();
        let mut ctx = ctx.lock().unwrap();
        let tokens = BTreeMap::from_iter(triggers.into_iter().map(|(name, mul)| {
            let node_id = ctx.share_node_name(name.as_ref());
            let cap = ctx.get_capacity(node_id);

            if mul > cap {
                warn!(
                    "Clamping trigger's {:?} to Capacity({}) of node \"{}\"",
                    mul,
                    cap,
                    name.as_ref()
                );

                (node_id, cap)
            } else {
                (node_id, mul)
            }
        }));

        State { context, tokens }
    }

    pub fn from_triggers_checked<S, I>(ctx: &ContextHandle, triggers: I) -> Result<Self, AcesError>
    where
        S: AsRef<str>,
        I: IntoIterator<Item = (S, Multiplicity)>,
    {
        let mut error = None;
        let tokens = BTreeMap::from_iter(
            triggers
                .into_iter()
                .map(|(name, mul)| {
                    let node_id = ctx.lock().unwrap().share_node_name(name.as_ref());

                    (node_id, mul)
                })
                .take_while(|&(node_id, mul)| {
                    let cap = ctx.lock().unwrap().get_capacity(node_id);

                    if mul > cap {
                        error = Some((node_id, cap, mul));
                        false
                    } else {
                        true
                    }
                }),
        );

        let context = ctx.clone();

        match error {
            Some((n, c, m)) => Err(AcesErrorKind::CapacityOverflow(n, c, m).with_context(&context)),
            None => Ok(State { context, tokens }),
        }
    }

    pub fn clear(&mut self) {
        self.tokens.clear()
    }

    pub fn get(&self, node_id: NodeID) -> Multiplicity {
        self.tokens.get(&node_id).copied().unwrap_or_else(Multiplicity::zero)
    }

    pub fn set_unchecked(&mut self, node_id: NodeID, num_tokens: Multiplicity) {
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

    /// See also [`FiringComponent::fire()`].
    ///
    /// [`FiringComponent::fire()`]: crate::FiringComponent::fire()
    pub(crate) fn decrease(
        &mut self,
        node_id: NodeID,
        num_tokens: Multiplicity,
    ) -> Result<(), AcesError> {
        if num_tokens.is_positive() {
            if let btree_map::Entry::Occupied(mut entry) = self.tokens.entry(node_id) {
                let tokens_before = *entry.get_mut();

                if tokens_before.is_positive() {
                    if num_tokens.is_finite() {
                        if let Some(tokens_after) = tokens_before.checked_sub(num_tokens) {
                            *entry.get_mut() = tokens_after;
                        } else {
                            return Err(AcesErrorKind::StateUnderflow(
                                node_id,
                                tokens_before,
                                num_tokens,
                            )
                            .with_context(&self.context))
                        }
                    } else {
                        return Err(AcesErrorKind::LeakedInhibitor(node_id, tokens_before)
                            .with_context(&self.context))
                    }
                } else if num_tokens.is_finite() {
                    return Err(AcesErrorKind::StateUnderflow(node_id, tokens_before, num_tokens)
                        .with_context(&self.context))
                }
            } else if num_tokens.is_finite() {
                return Err(AcesErrorKind::StateUnderflow(node_id, Multiplicity::zero(), num_tokens)
                    .with_context(&self.context))
            }
        }

        Ok(())
    }

    /// Note: this routine doesn't check for capacity overflow.
    ///
    /// See also [`FiringComponent::fire()`].
    ///
    /// [`FiringComponent::fire()`]: crate::FiringComponent::fire()
    pub(crate) fn increase(
        &mut self,
        node_id: NodeID,
        num_tokens: Multiplicity,
    ) -> Result<(), AcesError> {
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
                                return Err(AcesErrorKind::StateOverflow(
                                    node_id,
                                    tokens_before,
                                    num_tokens,
                                )
                                .with_context(&self.context))
                            }
                        } else {
                            return Err(AcesErrorKind::StateOverflow(
                                node_id,
                                tokens_before,
                                num_tokens,
                            )
                            .with_context(&self.context))
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
                debug!("Go from {}", self);
            } else if num_steps < 10 {
                debug!("Step {}  {}", num_steps, self);
            } else {
                debug!("Step {} {}", num_steps, self);
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

            fset.as_slice()[fc_id].fire(self)?;

            Ok(Some(fc_id))
        } else {
            Ok(None)
        }
    }

    /// Returns ID of the activated firing component taken from the
    /// given [`FiringSet`].
    pub fn transition<R: rand::RngCore>(
        &mut self,
        fset: &FiringSet,
        rng: &mut R,
    ) -> Result<Option<usize>, AcesError> {
        let enabled_fcs = fset.get_enabled(self);

        if let Some(fc_id) = enabled_fcs.get_random(rng) {
            fset.as_slice()[fc_id].fire(self)?;

            Ok(Some(fc_id))
        } else {
            Ok(None)
        }
    }
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut at_start = true;

        '{'.fmt(f)?;

        for (node_id, &num_tokens) in self.tokens.iter() {
            if num_tokens.is_positive() {
                if at_start {
                    at_start = false;
                } else {
                    ','.fmt(f)?;
                }
                write!(f, " {}: {}", node_id.with(&self.context), num_tokens)?;
            }
        }

        " }".fmt(f)
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
