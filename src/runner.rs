use crate::{State, Semantics, FiringSequence};

#[derive(Clone, Default, Debug)]
pub(crate) struct Options {
    pub(crate) semantics: Option<Semantics>,
    pub(crate) max_steps: Option<u64>,
}

pub struct Runner {
    initial_state:   State,
    firing_sequence: FiringSequence,
}

impl Runner {
    pub fn run(&mut self) {}
}
