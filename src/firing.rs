use std::{slice, collections::BTreeMap, convert::TryFrom};
use rand::{RngCore, Rng};
use crate::{
    ContextHandle, Contextual, Polarity, DotId, Capacity, Weight, Solution, State, AcesError,
    AcesErrorKind, domain::Dotset,
};

#[derive(Clone, Default, Debug)]
pub struct FiringComponent {
    pre_set:  BTreeMap<DotId, Weight>,
    post_set: BTreeMap<DotId, (Weight, Capacity)>,
}

impl FiringComponent {
    pub fn is_enabled(&self, state: &State) -> bool {
        for (&dot_id, &weight) in self.pre_set.iter() {
            let tokens = state.get(dot_id);

            if tokens.is_zero() {
                if weight.is_finite() {
                    return false
                }
            } else if tokens < weight {
                return false
            }
        }

        for (&dot_id, &(weight, capacity)) in self.post_set.iter() {
            if let Some(tokens_after) = state.get(dot_id).checked_add(weight) {
                if tokens_after > capacity {
                    return false
                }
            } else {
                warn!("Overflow of state at {:?} when checking enablement", dot_id);

                if capacity.is_finite() {
                    return false
                }
            }
        }

        true
    }

    /// Note: this routine doesn't check for enablement.  Instead, it
    /// assumes that the firing component is enabled in the given
    /// state.  Therefore, prior to a call to [`fire()`],
    /// [`is_enabled()`] should return `true` (it may be called
    /// directly, or e.g. via [`FiringSet::get_enabled()`]).
    ///
    /// [`is_enabled()`]: FiringComponent::is_enabled()
    /// [`fire()`]: FiringComponent::fire()
    pub fn fire(&self, state: &mut State) -> Result<(), AcesError> {
        for (&dot_id, &weight) in self.pre_set.iter() {
            state.decrease(dot_id, weight)?;
        }

        for (&dot_id, &(weight, _)) in self.post_set.iter() {
            state.increase(dot_id, weight)?;
        }

        Ok(())
    }
}

impl TryFrom<Solution> for FiringComponent {
    type Error = AcesError;

    #[allow(clippy::map_entry)]
    fn try_from(sol: Solution) -> Result<Self, Self::Error> {
        let mut ctx = sol.get_context().lock().unwrap();
        let pre_dots = sol.get_pre_set();
        let post_dots = sol.get_post_set();
        let mut pre_set = BTreeMap::new();
        let mut post_set = BTreeMap::new();
        let mut forks = BTreeMap::new();
        let mut joins = BTreeMap::new();

        'outer_forks: for &fork_id in sol.get_fork_set() {
            let fork =
                ctx.get_fork(fork_id).ok_or_else(|| AcesErrorKind::ForkMissingForId(fork_id))?;
            let tx_dot_id = fork.get_tip_id();

            for &dot_id in pre_dots {
                if dot_id == tx_dot_id {
                    if pre_set.contains_key(&dot_id) {
                        return Err(AcesErrorKind::FiringDotDuplicated(Polarity::Tx, dot_id).into())
                    } else {
                        pre_set.insert(dot_id, Weight::one());
                        forks.insert(dot_id, fork_id);
                        continue 'outer_forks
                    }
                }
            }

            return Err(AcesErrorKind::FiringDotMissing(Polarity::Tx, tx_dot_id).into())
        }

        'outer_joins: for &join_id in sol.get_join_set() {
            let join = ctx
                .get_join(join_id)
                .ok_or_else(|| AcesError::from(AcesErrorKind::JoinMissingForId(join_id)))?;
            let rx_dot_id = join.get_tip_id();

            for &dot_id in post_dots {
                if dot_id == rx_dot_id {
                    if post_set.contains_key(&dot_id) {
                        return Err(AcesErrorKind::FiringDotDuplicated(Polarity::Rx, dot_id).into())
                    } else {
                        let capacity = ctx.get_capacity(dot_id);
                        post_set.insert(dot_id, (Weight::one(), capacity));
                        joins.insert(dot_id, join_id);
                        continue 'outer_joins
                    }
                }
            }

            return Err(AcesErrorKind::FiringDotMissing(Polarity::Rx, rx_dot_id).into())
        }

        let mut dotset = Dotset::new_unchecked(pre_set.keys().copied());
        let _pre_set_id = ctx.share_dotset(&mut dotset);
        let mut dotset = Dotset::new_unchecked(post_set.keys().copied());
        let _post_set_id = ctx.share_dotset(&mut dotset);

        for (dot_id, fork_id) in forks {
            let weight = ctx.get_weight(fork_id.get())?;
            pre_set.entry(dot_id).and_modify(|v| *v = weight);
        }

        for (dot_id, join_id) in joins {
            let weight = ctx.get_weight(join_id.get())?;
            post_set.entry(dot_id).and_modify(|v| v.0 = weight);
        }

        Ok(FiringComponent { pre_set, post_set })
    }
}

impl Contextual for FiringComponent {
    // FIXME add definition of
    //fn format_styled(&self, ctx: &ContextHandle) -> Result<String, AcesError> {

    fn format(&self, ctx: &ContextHandle) -> Result<String, AcesError> {
        let mut result = String::new();

        if self.pre_set.is_empty() {
            result.push_str("{} => {");
        } else {
            result.push('{');

            for (dot_id, weight) in self.pre_set.iter() {
                result.push(' ');

                if weight.is_omega() {
                    result.push('(');
                    result.push_str(dot_id.format(ctx)?.as_str());
                    result.push(')');
                } else if weight.is_zero() {
                    result.push_str(format!("[={}]", dot_id.format(ctx)?.as_str()).as_str());
                } else if weight.is_multiple() {
                    result.push_str(
                        format!("[{}:{}]", dot_id.format(ctx)?.as_str(), weight).as_str(),
                    );
                } else {
                    result.push_str(dot_id.format(ctx)?.as_str());
                }
            }

            result.push_str(" } => {");
        }

        if self.post_set.is_empty() {
            result.push('}');
        } else {
            for (dot_id, (weight, capacity)) in self.post_set.iter() {
                result.push(' ');

                if weight.is_omega() {
                    result.push('(');
                    result.push_str(dot_id.format(ctx)?.as_str());
                    if capacity.is_multiple() {
                        result.push_str(format!("#{})", capacity).as_str());
                    } else {
                        result.push(')');
                    }
                } else if weight.is_multiple() {
                    if capacity.is_multiple() {
                        result.push_str(
                            format!("[{}:{}#{}]", weight, dot_id.format(ctx)?.as_str(), capacity)
                                .as_str(),
                        );
                    } else {
                        result.push_str(
                            format!("[{}:{}]", weight, dot_id.format(ctx)?.as_str()).as_str(),
                        );
                    }
                } else if weight.is_zero() {
                    if capacity.is_multiple() {
                        result.push_str(
                            format!("[={}#{}]", dot_id.format(ctx)?.as_str(), capacity).as_str(),
                        );
                    } else {
                        result.push_str(format!("[={}]", dot_id.format(ctx)?.as_str()).as_str());
                    }
                } else if capacity.is_multiple() {
                    result.push_str(
                        format!("[{}#{}]", dot_id.format(ctx)?.as_str(), capacity).as_str(),
                    );
                } else {
                    result.push_str(dot_id.format(ctx)?.as_str());
                }
            }

            result.push_str(" }");
        }

        Ok(result)
    }
}

#[derive(Clone, Default, Debug)]
pub struct FiringSet {
    fcs: Vec<FiringComponent>,
}

impl FiringSet {
    #[inline]
    pub fn as_slice(&self) -> &[FiringComponent] {
        self.fcs.as_slice()
    }

    pub fn get_enabled(&self, state: &State) -> FiringSequence {
        let mut enabled_fcs = FiringSequence::new();

        for (pos, fc) in self.fcs.as_slice().iter().enumerate() {
            if fc.is_enabled(state) {
                enabled_fcs.push(pos)
            }
        }

        enabled_fcs
    }
}

impl From<Vec<FiringComponent>> for FiringSet {
    #[inline]
    fn from(fcs: Vec<FiringComponent>) -> Self {
        FiringSet { fcs }
    }
}

// FIXME grouping
#[derive(Default, Debug)]
pub struct FiringSequence {
    fcs: Vec<(usize, bool)>,
}

impl FiringSequence {
    pub fn new() -> Self {
        Default::default()
    }

    #[inline]
    pub fn clear(&mut self) {
        self.fcs.clear()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.fcs.is_empty()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.fcs.len()
    }

    #[inline]
    pub fn push(&mut self, fc_id: usize) {
        self.fcs.push((fc_id, true))
    }

    #[inline]
    pub fn first(&self) -> Option<usize> {
        self.fcs.first().map(|r| r.0)
    }

    #[inline]
    pub fn get(&self, pos: usize) -> Option<usize> {
        self.fcs.get(pos).map(|r| r.0)
    }

    pub fn get_random<R: RngCore>(&self, rng: &mut R) -> Option<usize> {
        match self.fcs.len() {
            0 => None,
            1 => Some(self.fcs[0].0),
            n => Some(self.fcs[rng.gen_range(0, n)].0),
        }
    }

    pub fn iter<'b, 'a: 'b>(&'a self, firing_set: &'b FiringSet) -> FiringIter<'b> {
        FiringIter { iter: self.fcs.iter(), fset: firing_set }
    }

    pub fn par_iter<'b, 'a: 'b>(&'a self, firing_set: &'b FiringSet) -> FiringParIter<'b> {
        FiringParIter { iter: self.fcs.iter(), fset: firing_set }
    }
}

pub struct FiringIter<'a> {
    iter: slice::Iter<'a, (usize, bool)>,
    fset: &'a FiringSet,
}

impl<'a> Iterator for FiringIter<'a> {
    type Item = &'a FiringComponent;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((pos, _)) = self.iter.next() {
            let fc = self.fset.fcs.get(*pos).expect("Broken firing sequence.");
            Some(fc)
        } else {
            None
        }
    }
}

pub struct FiringParIter<'a> {
    iter: slice::Iter<'a, (usize, bool)>,
    fset: &'a FiringSet,
}

impl<'a> Iterator for FiringParIter<'a> {
    type Item = Vec<&'a FiringComponent>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut fcs = Vec::new();

        while let Some((pos, fin)) = self.iter.next() {
            let fc = self.fset.fcs.get(*pos).expect("Broken firing sequence.");

            fcs.push(fc);

            if *fin {
                return Some(fcs)
            }
        }

        None
    }
}
