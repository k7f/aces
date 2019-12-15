use log::Level::Debug;
use crate::{ContextHandle, Contextual, State, Semantics, FiringSet, FiringSequence, AcesError};

#[derive(Clone, Default, Debug)]
pub(crate) struct Props {
    pub(crate) semantics: Option<Semantics>,
    pub(crate) max_steps: Option<usize>,
}

impl Props {
    pub(crate) fn clear(&mut self) {
        *self = Default::default();
    }
}

#[derive(Debug)]
pub struct Runner {
    context:         ContextHandle,
    initial_state:   State,
    current_state:   State,
    semantics:       Semantics,
    max_steps:       usize,
    firing_sequence: FiringSequence,
}

impl Runner {
    fn update_props(&mut self) {
        let ctx = self.context.lock().unwrap();

        if let Some(v) = ctx.get_semantics() {
            self.semantics = v;
        }

        if let Some(v) = ctx.get_max_steps() {
            self.max_steps = v;
        }
    }

    pub fn new<S: AsRef<str>>(ctx: &ContextHandle, trigger_name: S) -> Self {
        let context = ctx.clone();
        let initial_state = State::from_trigger(&context, trigger_name);
        let current_state = initial_state.clone();
        let semantics = Semantics::default();
        let max_steps = 1;
        let firing_sequence = FiringSequence::new();

        let mut runner =
            Runner { context, initial_state, current_state, semantics, max_steps, firing_sequence };
        runner.update_props();

        runner
    }

    pub fn go(&mut self, fset: &FiringSet) -> Result<(), AcesError> {
        let mut rng = rand::thread_rng();

        self.update_props();

        if self.semantics == Semantics::Parallel {
            println!("Firing under parallel semantics isn't implemented yet.");
            return Ok(())
        }

        for num_steps in 0..self.max_steps {
            let fc_id = if log_enabled!(Debug) {
                self.current_state.transition_debug(&self.context, num_steps, fset, &mut rng)?
            } else {
                self.current_state.transition(&self.context, fset, &mut rng)?
            };

            if let Some(fc_id) = fc_id {
                self.firing_sequence.push(fc_id);
            } else {
                debug!("Deadlock after {} steps", num_steps);

                return Ok(())
            }
        }

        if log_enabled!(Debug) {
            debug!("Stop at {}", self.current_state.with(&self.context));
            debug!("Done after {} steps", self.max_steps);
        }

        Ok(())
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
