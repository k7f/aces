use std::{slice, collections::BTreeMap, convert::TryFrom, error::Error};
use crate::{Context, Contextual, NodeID, ForkID, JoinID, node, Solution, State, AcesError};

#[derive(Default, Debug)]
pub struct FiringComponent {
    pre_set:  BTreeMap<NodeID, ForkID>,
    post_set: BTreeMap<NodeID, JoinID>,
}

impl FiringComponent {
    pub fn is_enabled(&self, state: &State) -> bool {
        for &node_id in self.pre_set.keys() {
            if state.num_tokens(node_id) == 0 {
                return false
            }
        }

        for &node_id in self.post_set.keys() {
            if state.num_tokens(node_id) > 0 {
                return false
            }
        }

        true
    }
}

impl TryFrom<Solution> for FiringComponent {
    type Error = AcesError;

    #[allow(clippy::map_entry)]
    fn try_from(sol: Solution) -> Result<Self, Self::Error> {
        let ctx = sol.get_context().lock().unwrap();
        let mut pre_set = BTreeMap::new();
        let mut post_set = BTreeMap::new();

        'outer_forks: for &fid in sol.get_fork_set() {
            let fork = ctx.get_fork(fid).ok_or(AcesError::ForkMissingForID)?;
            let tx_node_id = fork.get_host_id();

            for &node_id in sol.get_pre_set().iter() {
                if node_id == tx_node_id {
                    if pre_set.contains_key(&node_id) {
                        return Err(AcesError::FiringNodeDuplicated(node::Face::Tx))
                    } else {
                        pre_set.insert(node_id, fid);
                        continue 'outer_forks
                    }
                }
            }

            return Err(AcesError::FiringNodeMissing(node::Face::Tx))
        }

        'outer_joins: for &jid in sol.get_join_set() {
            let join = ctx.get_join(jid).ok_or(AcesError::JoinMissingForID)?;
            let rx_node_id = join.get_host_id();

            for &node_id in sol.get_post_set().iter() {
                if node_id == rx_node_id {
                    if post_set.contains_key(&node_id) {
                        return Err(AcesError::FiringNodeDuplicated(node::Face::Rx))
                    } else {
                        post_set.insert(node_id, jid);
                        continue 'outer_joins
                    }
                }
            }

            return Err(AcesError::FiringNodeMissing(node::Face::Rx))
        }

        Ok(FiringComponent { pre_set, post_set })
    }
}

impl Contextual for FiringComponent {
    fn format(&self, ctx: &Context) -> Result<String, Box<dyn Error>> {
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
    fn from(fcs: Vec<FiringComponent>) -> Self {
        FiringSet { fcs }
    }
}

#[derive(Default, Debug)]
pub struct FiringSequence {
    computation: Vec<(usize, bool)>,
}

impl FiringSequence {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn push(&mut self, fc_id: usize) {
        self.computation.push((fc_id, true))
    }

    pub fn iter<'b, 'a: 'b>(&'a self, firing_set: &'b FiringSet) -> FiringIter<'b> {
        FiringIter { iter: self.computation.iter(), fset: firing_set }
    }

    pub fn par_iter<'b, 'a: 'b>(&'a self, firing_set: &'b FiringSet) -> FiringParIter<'b> {
        FiringParIter { iter: self.computation.iter(), fset: firing_set }
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
