use log::Level::Debug;
use crate::{
    ContextHandle, DotId, Multiplicity, State, Goal, Semantics, FiringSet, FiringSequence,
    AcesError,
};

#[derive(Clone, Default, Debug)]
pub(crate) struct Props {
    pub(crate) semantics:  Option<Semantics>,
    pub(crate) max_steps:  Option<usize>,
    pub(crate) num_passes: Option<usize>,
}

impl Props {
    pub(crate) fn clear(&mut self) {
        *self = Default::default();
    }
}

#[derive(Debug)]
pub enum StopCondition {
    GoalReached(DotId, usize),
    Stalemate(usize),
    Pause(usize),
    UnimplementedFeature(String),
}

#[derive(Debug)]
pub struct Runner {
    context:         ContextHandle,
    rng:             Option<rand::rngs::ThreadRng>,
    initial_state:   State,
    current_state:   State,
    goal:            Option<Goal>,
    semantics:       Semantics,
    max_steps:       usize,
    num_passes:      Option<usize>,
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

        self.num_passes = ctx.get_num_passes();
    }

    pub fn new<S, I>(ctx: &ContextHandle, triggers: I) -> Self
    where
        S: AsRef<str>,
        I: IntoIterator<Item = (S, Multiplicity)>,
    {
        let context = ctx.clone();
        let rng = None;
        let initial_state = State::from_triggers_saturated(&context, triggers);
        let current_state = initial_state.clone();
        let goal = None;
        let semantics = Semantics::default();
        let max_steps = 1;
        let num_passes = None;
        let firing_sequence = FiringSequence::new();

        let mut runner = Runner {
            context,
            rng,
            initial_state,
            current_state,
            goal,
            semantics,
            max_steps,
            num_passes,
            firing_sequence,
        };
        runner.update_props();

        runner
    }

    pub fn with_goal<S, I>(mut self, targets: I) -> Result<Self, AcesError>
    where
        S: AsRef<str>,
        I: IntoIterator<Item = (S, Multiplicity)>,
    {
        self.goal = Some(Goal::from_targets_checked(&self.context, targets)?);

        Ok(self)
    }

    pub fn restart(&mut self) {
        self.current_state = self.initial_state.clone();
        self.firing_sequence.clear();
    }

    pub fn go(&mut self, fset: &FiringSet) -> Result<StopCondition, AcesError> {
        self.rng = Some(rand::thread_rng());
        self.update_props();
        self.resume(fset)
    }

    pub fn resume(&mut self, fset: &FiringSet) -> Result<StopCondition, AcesError> {
        match self.semantics {
            Semantics::Sequential => {
                let mut rng = self.rng.unwrap_or_else(rand::thread_rng);
                self.rng = Some(rng);

                for num_steps in 0..self.max_steps {
                    if let Some(dot_id) = self.goal_is_reached() {
                        debug!("Goal reached at {:?} after {} steps", dot_id, num_steps);

                        return Ok(StopCondition::GoalReached(dot_id, num_steps))
                    }

                    let fc_id = if log_enabled!(Debug) {
                        self.current_state.transition_debug(
                            &self.context,
                            num_steps,
                            fset,
                            &mut rng,
                        )?
                    } else {
                        self.current_state.transition(fset, &mut rng)?
                    };

                    if let Some(fc_id) = fc_id {
                        self.firing_sequence.push(fc_id);
                    } else {
                        debug!("Stuck after {} steps", num_steps + 1);

                        return Ok(StopCondition::Stalemate(num_steps))
                    }
                }
            }

            Semantics::Parallel => {
                let mut rng = self.rng.unwrap_or_else(rand::thread_rng);
                self.rng = Some(rng);

                for num_steps in 0..self.max_steps {
                    if let Some(dot_id) = self.goal_is_reached() {
                        debug!("Goal reached at {:?} after {} steps", dot_id, num_steps);

                        return Ok(StopCondition::GoalReached(dot_id, num_steps))
                    }

                    let fc_ids = self.current_state.parallel_transition(fset, &mut rng)?;

                    if let Some((last_id, other_ids)) = fc_ids.split_last() {
                        self.firing_sequence.push_multi(*last_id, other_ids);
                    } else {
                        debug!("Stuck after {} steps", num_steps + 1);

                        return Ok(StopCondition::Stalemate(num_steps))
                    }
                }
            }

            Semantics::Maximal => {
                warn!("This is only a mockup, maximal semantics isn't supported yet");

                for num_steps in 0..self.max_steps {
                    if let Some(dot_id) = self.goal_is_reached() {
                        debug!("Goal reached at {:?} after {} steps", dot_id, num_steps);

                        return Ok(StopCondition::GoalReached(dot_id, num_steps))
                    }

                    let fc_ids = self.current_state.maximal_transition(fset)?;

                    if let Some((last_id, other_ids)) = fc_ids.split_last() {
                        self.firing_sequence.push_multi(*last_id, other_ids);
                    } else {
                        debug!("Stuck after {} steps", num_steps + 1);

                        return Ok(StopCondition::Stalemate(num_steps))
                    }
                }
            }
        }

        if let Some(dot_id) = self.goal_is_reached() {
            debug!("Goal reached at {:?} after {} steps", dot_id, self.max_steps);

            return Ok(StopCondition::GoalReached(dot_id, self.max_steps))
        }

        if log_enabled!(Debug) {
            debug!("Stop at {}", self.current_state);
            debug!("Done after {} steps", self.max_steps);
        }

        Ok(StopCondition::Pause(self.max_steps))
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
    pub fn get_goal(&self) -> Option<&Goal> {
        self.goal.as_ref()
    }

    #[inline]
    pub fn get_max_steps(&self) -> usize {
        self.max_steps
    }

    #[inline]
    pub fn get_num_passes(&self) -> Option<usize> {
        self.num_passes
    }

    #[inline]
    pub fn get_firing_sequence(&self) -> &FiringSequence {
        &self.firing_sequence
    }

    pub fn goal_is_reached(&self) -> Option<DotId> {
        self.goal.as_ref().and_then(|goal| goal.is_reached(&self.current_state))
    }
}
