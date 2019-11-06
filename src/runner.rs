use crate::{ContextHandle, CEStructure, State, Semantics, FiringSequence};

#[derive(Clone, Default, Debug)]
pub(crate) struct Options {
    pub(crate) semantics: Option<Semantics>,
    pub(crate) max_steps: Option<u64>,
}

#[derive(Debug)]
pub struct Runner {
    initial_state:   State,
    firing_sequence: FiringSequence,
}

impl Runner {
    pub fn new<S: AsRef<str>>(ctx: &ContextHandle, trigger_name: S) -> Self {
        let initial_state = State::from_trigger(ctx, trigger_name);
        let firing_sequence = FiringSequence::new();

        Runner { initial_state, firing_sequence }
    }

    pub fn go(&mut self, ces: &CEStructure) {
        let ctx = ces.get_context().lock().unwrap();

        let ref state = self.initial_state;

        println!("Go from {}", ctx.with(state));

        if let Some(fset) = ces.get_firing_set() {
            let fcs = fset.get_enabled(state);
            let mut at_start = true;

            for fc in fcs.iter(fset) {
                if at_start {
                    println!("Enabled: {}", ctx.with(fc));
                    at_start = false;
                } else {
                    println!("         {}", ctx.with(fc));
                }
            }
        }
    }
}
