use std::{
    collections::{btree_map, BTreeMap},
    iter::FromIterator,
    fmt,
};
use log::Level::{Debug, Trace};
use crate::{ContextHandle, Contextual, Multiplicity, DotId, FiringSet, AcesError, AcesErrorKind};

#[derive(Clone, Debug)]
pub struct State {
    context: ContextHandle,
    tokens:  BTreeMap<DotId, Multiplicity>,
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
            let dot_id = ctx.share_dot_name(name.as_ref());
            let cap = ctx.get_capacity(dot_id);

            if mul > cap {
                warn!(
                    "Clamping trigger's {:?} to Capacity({}) of dot \"{}\"",
                    mul,
                    cap,
                    name.as_ref()
                );

                (dot_id, cap)
            } else {
                (dot_id, mul)
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
                    let dot_id = ctx.lock().unwrap().share_dot_name(name.as_ref());

                    (dot_id, mul)
                })
                .take_while(|&(dot_id, mul)| {
                    let cap = ctx.lock().unwrap().get_capacity(dot_id);

                    if mul > cap {
                        error = Some((dot_id, cap, mul));
                        false
                    } else {
                        true
                    }
                }),
        );

        match error {
            Some((n, c, m)) => Err(AcesErrorKind::CapacityOverflow(n, c, m).with_context(ctx)),
            None => Ok(State { context: ctx.clone(), tokens }),
        }
    }

    pub fn clear(&mut self) {
        self.tokens.clear()
    }

    pub fn get(&self, dot_id: DotId) -> Multiplicity {
        self.tokens.get(&dot_id).copied().unwrap_or_else(Multiplicity::zero)
    }

    pub fn set_unchecked(&mut self, dot_id: DotId, num_tokens: Multiplicity) {
        match self.tokens.entry(dot_id) {
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
        dot_id: DotId,
        num_tokens: Multiplicity,
    ) -> Result<(), AcesError> {
        if num_tokens.is_positive() {
            if let btree_map::Entry::Occupied(mut entry) = self.tokens.entry(dot_id) {
                let tokens_before = *entry.get_mut();

                if tokens_before.is_positive() {
                    if num_tokens.is_finite() {
                        if let Some(tokens_after) = tokens_before.checked_sub(num_tokens) {
                            *entry.get_mut() = tokens_after;
                        } else {
                            return Err(AcesErrorKind::StateUnderflow(
                                dot_id,
                                tokens_before,
                                num_tokens,
                            )
                            .with_context(&self.context))
                        }
                    } else {
                        return Err(AcesErrorKind::LeakedInhibitor(dot_id, tokens_before)
                            .with_context(&self.context))
                    }
                } else if num_tokens.is_finite() {
                    return Err(AcesErrorKind::StateUnderflow(dot_id, tokens_before, num_tokens)
                        .with_context(&self.context))
                }
            } else if num_tokens.is_finite() {
                return Err(AcesErrorKind::StateUnderflow(dot_id, Multiplicity::zero(), num_tokens)
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
        dot_id: DotId,
        num_tokens: Multiplicity,
    ) -> Result<(), AcesError> {
        if num_tokens.is_positive() {
            match self.tokens.entry(dot_id) {
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
                                    dot_id,
                                    tokens_before,
                                    num_tokens,
                                )
                                .with_context(&self.context))
                            }
                        } else {
                            return Err(AcesErrorKind::StateOverflow(
                                dot_id,
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

    /// Activates and fires a single firing component.
    ///
    /// The firing component is randomly chosen from the enabled
    /// subset of the given [`FiringSet`].
    ///
    /// Returns the position of the activated firing component in the
    /// [`FiringSet`].
    pub fn transition<R: rand::RngCore>(
        &mut self,
        fset: &FiringSet,
        rng: &mut R,
    ) -> Result<Option<usize>, AcesError> {
        let enabled_fcs = fset.get_enabled(self);

        enabled_fcs.fire_single(self, fset, rng)
    }

    /// Activates a random independent set of firing components and
    /// fires them all in a single step.
    ///
    /// The firing components are chosen from the enabled subset of
    /// the given [`FiringSet`].  Note that two firing components are
    /// independent (not adjacent) iff they have disjoint carriers.
    ///
    /// Returns the vector of positions of activated firing components
    /// in the [`FiringSet`].
    pub fn parallel_transition<R: rand::RngCore>(
        &mut self,
        fset: &FiringSet,
        rng: &mut R,
    ) -> Result<Vec<usize>, AcesError> {
        let mut enabled_fcs = fset.get_enabled(self);

        enabled_fcs.fire_parallel(self, fset, rng)
    }

    /// Activates the set of enabled firing components and fires them
    /// all in a single step.
    ///
    /// Returns the vector of positions of activated firing components
    /// in the [`FiringSet`].
    pub fn maximal_transition(&mut self, fset: &FiringSet) -> Result<Vec<usize>, AcesError> {
        let enabled_fcs = fset.get_enabled(self);

        enabled_fcs.fire_maximal(self, fset)?;

        Ok(enabled_fcs.into())
    }
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut at_start = true;

        '{'.fmt(f)?;

        for (dot_id, &num_tokens) in self.tokens.iter() {
            if num_tokens.is_positive() {
                if at_start {
                    at_start = false;
                } else {
                    ','.fmt(f)?;
                }
                write!(f, " {}: {}", dot_id.with(&self.context), num_tokens)?;
            }
        }

        " }".fmt(f)
    }
}

#[derive(Debug)]
pub struct Goal {
    targets: BTreeMap<DotId, Multiplicity>,
}

impl Goal {
    pub fn from_targets_checked<S, I>(ctx: &ContextHandle, targets: I) -> Result<Self, AcesError>
    where
        S: AsRef<str>,
        I: IntoIterator<Item = (S, Multiplicity)>,
    {
        let mut error = None;
        let targets = BTreeMap::from_iter(
            targets
                .into_iter()
                .map(|(name, mul)| {
                    let dot_id = ctx.lock().unwrap().share_dot_name(name.as_ref());

                    (dot_id, mul)
                })
                .take_while(|&(dot_id, mul)| {
                    let cap = ctx.lock().unwrap().get_capacity(dot_id);

                    if mul > cap {
                        error = Some((dot_id, cap, mul));
                        false
                    } else {
                        true
                    }
                }),
        );

        match error {
            Some((n, c, m)) => Err(AcesErrorKind::CapacityOverflow(n, c, m).with_context(ctx)),
            None => Ok(Goal { targets }),
        }
    }

    pub fn is_reached(&self, state: &State) -> Option<DotId> {
        for (&dot_id, &target_tokens) in self.targets.iter() {
            let tokens = state.get(dot_id);

            if target_tokens.is_omega() {
                if tokens.is_omega() {
                    return Some(dot_id)
                }
            } else if tokens >= target_tokens {
                return Some(dot_id)
            }
        }

        None
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Semantics {
    Sequential,
    Parallel,
    Maximal,
}

impl Default for Semantics {
    fn default() -> Self {
        Semantics::Sequential
    }
}
