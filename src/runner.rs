use log::Level::Debug;
use crate::{ContextHandle, State, Semantics, FiringSet, FiringSequence};

#[derive(Clone, Default, Debug)]
pub(crate) struct Options {
    pub(crate) semantics: Option<Semantics>,
    pub(crate) max_steps: Option<usize>,
}

#[derive(Debug)]
pub struct Runner {
    context:         ContextHandle,
    initial_state:   State,
    current_state:   State,
    max_steps:       usize,
    firing_sequence: FiringSequence,
}

impl Runner {
    pub fn new<S: AsRef<str>>(ctx: &ContextHandle, trigger_name: S) -> Self {
        let context = ctx.clone();
        let initial_state = State::from_trigger(&context, trigger_name);
        let current_state = initial_state.clone();
        let max_steps = context.lock().unwrap().get_max_steps().unwrap_or(1);
        let firing_sequence = FiringSequence::new();

        Runner { context, initial_state, current_state, max_steps, firing_sequence }
    }

    pub fn go(&mut self, fset: &FiringSet) {
        let mut rng = rand::thread_rng();

        self.max_steps = self.context.lock().unwrap().get_max_steps().unwrap_or(1);

        for num_steps in 0..self.max_steps {
            let fc_id = if log_enabled!(Debug) {
                self.current_state.transition_debug(&self.context, num_steps, fset, &mut rng)
            } else {
                self.current_state.transition(fset, &mut rng)
            };

            if let Some(fc_id) = fc_id {
                self.firing_sequence.push(fc_id);
            } else {
                debug!("Deadlock after {} steps", num_steps);

                return
            }
        }

        if log_enabled!(Debug) {
            debug!("Stop at {}", self.context.lock().unwrap().with(&self.current_state));
            debug!("Done after {} steps", self.max_steps);
        }
    }

    #[inline]
    pub fn get_initial_state(&self) -> &State {
        &self.initial_state
    }

    #[inline]
    pub fn get_current_state(&self) -> &State {
        &self.current_state
    }

    #[inline]
    pub fn get_max_steps(&self) -> usize {
        self.max_steps
    }

    #[inline]
    pub fn get_firing_sequence(&self) -> &FiringSequence {
        &self.firing_sequence
    }
}
