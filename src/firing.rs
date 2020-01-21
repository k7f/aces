use std::{slice, collections::BTreeMap, convert::TryFrom, error::Error};
use rand::{RngCore, Rng};
use crate::{
    ContextHandle, Contextual, NodeID, ForkID, JoinID, node, Solution, State, AcesError,
    AcesErrorKind,
};

#[derive(Default, Debug)]
pub struct FiringComponent {
    pre_set:  BTreeMap<NodeID, ForkID>,
    post_set: BTreeMap<NodeID, JoinID>,
}

impl FiringComponent {
    pub fn is_enabled(&self, ctx: &ContextHandle, state: &State) -> bool {
        let ctx = ctx.lock().unwrap();

        for (&node_id, fork_id) in self.pre_set.iter() {
            let weight = ctx.get_weight(fork_id.get());
            let tokens = state.get(node_id);

            if weight.is_omega() {
                if tokens.is_positive() {
                    return false
                }
            } else if tokens < weight {
                return false
            }
        }

        for (&node_id, join_id) in self.post_set.iter() {
            let capacity = ctx.get_capacity(node_id);
            let weight = ctx.get_weight(join_id.get());

            if let Some(tokens_after) = state.get(node_id).checked_add(weight) {
                if tokens_after > capacity {
                    return false
                }
            } else {
                warn!("Overflow of state when checking enablement");

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
    pub fn fire(&self, ctx: &ContextHandle, state: &mut State) -> Result<(), AcesError> {
        let ctx = ctx.lock().unwrap();

        for (&node_id, fork_id) in self.pre_set.iter() {
            let weight = ctx.get_weight(fork_id.get());

            state.decrease(node_id, weight)?;
        }

        for (&node_id, join_id) in self.post_set.iter() {
            let weight = ctx.get_weight(join_id.get());

            state.increase(node_id, weight)?;
        }

        Ok(())
    }
}

impl TryFrom<Solution> for FiringComponent {
    type Error = AcesError;

    #[allow(clippy::map_entry)]
    fn try_from(sol: Solution) -> Result<Self, Self::Error> {
        let ctx = sol.get_context().lock().unwrap();
        let mut pre_set = BTreeMap::new();
        let mut post_set = BTreeMap::new();

        'outer_forks: for &fork_id in sol.get_fork_set() {
            let fork = ctx
                .get_fork(fork_id)
                .ok_or_else(|| AcesError::from(AcesErrorKind::ForkMissingForID(fork_id)))?;
            let tx_node_id = fork.get_host_id();

            for &node_id in sol.get_pre_set().iter() {
                if node_id == tx_node_id {
                    if pre_set.contains_key(&node_id) {
                        return Err(AcesError::from(AcesErrorKind::FiringNodeDuplicated(
                            node::Face::Tx,
                        )))
                    } else {
                        pre_set.insert(node_id, fork_id);
                        continue 'outer_forks
                    }
                }
            }

            return Err(AcesError::from(AcesErrorKind::FiringNodeMissing(node::Face::Tx)))
        }

        'outer_joins: for &join_id in sol.get_join_set() {
            let join = ctx
                .get_join(join_id)
                .ok_or_else(|| AcesError::from(AcesErrorKind::JoinMissingForID(join_id)))?;
            let rx_node_id = join.get_host_id();

            for &node_id in sol.get_post_set().iter() {
                if node_id == rx_node_id {
                    if post_set.contains_key(&node_id) {
                        return Err(AcesError::from(AcesErrorKind::FiringNodeDuplicated(
                            node::Face::Rx,
                        )))
                    } else {
                        post_set.insert(node_id, join_id);
                        continue 'outer_joins
                    }
                }
            }

            return Err(AcesError::from(AcesErrorKind::FiringNodeMissing(node::Face::Rx)))
        }

        Ok(FiringComponent { pre_set, post_set })
    }
}

impl Contextual for FiringComponent {
    fn format(&self, ctx: &ContextHandle) -> Result<String, Box<dyn Error>> {
        let mut result = String::new();

        if self.pre_set.is_empty() {
            result.push_str("{} => {");
        } else {
            result.push('{');

            for node_id in self.pre_set.keys() {
                result.push(' ');
                result.push_str(node_id.format(ctx)?.as_str());
            }

            result.push_str(" } => {");
        }

        if self.post_set.is_empty() {
            result.push('}');
        } else {
            for node_id in self.post_set.keys() {
                result.push(' ');
                result.push_str(node_id.format(ctx)?.as_str());
            }

            result.push_str(" }");
        }

        Ok(result)
    }
}

#[derive(Default, Debug)]
pub struct FiringSet {
    fcs: Vec<FiringComponent>,
}

impl FiringSet {
    #[inline]
    pub fn as_slice(&self) -> &[FiringComponent] {
        self.fcs.as_slice()
    }

    pub fn get_enabled(&self, ctx: &ContextHandle, state: &State) -> FiringSequence {
        let mut enabled_fcs = FiringSequence::new();

        for (pos, fc) in self.fcs.as_slice().iter().enumerate() {
            if fc.is_enabled(ctx, state) {
                enabled_fcs.push(pos)
            }
        }

        enabled_fcs
    }
}

impl From<Vec<FiringComponent>> for FiringSet {
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
