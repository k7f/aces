use std::{collections::BTreeMap};
use crate::{NodeID, ForkID, JoinID, monomial::Weight};

#[derive(Default, Debug)]
pub struct FiringComponent {
    forks:    BTreeMap<ForkID, Weight>,
    joins:    BTreeMap<JoinID, Weight>,
    pre_set:  Vec<NodeID>,
    post_set: Vec<NodeID>,
}
