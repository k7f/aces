use std::error::Error;
use crate::Semantics;
use super::{App, Command, Solve};

pub struct Go {
    solve:               Solve,
    trigger_name:        Option<String>,
    requested_semantics: Option<Semantics>,
    requested_max_steps: Option<u64>,
}

impl Go {
    pub(crate) fn new(app: &mut App) -> Self {
        let solve = Solve::new(app);

        let trigger_name = app.value_of("TRIGGER").map(String::from);

        let requested_semantics = app.value_of("SEMANTICS").map(|v| match v {
            "seq" => Semantics::Sequential,
            "par" => Semantics::Parallel,
            _ => unreachable!(),
        });

        let requested_max_steps = app.value_of("MAX_STEPS").map(|v| match v.parse::<u64>() {
            Ok(val) => val,
            Err(_) => panic!("The argument '{}' isn't a valid value of MAX_STEPS", v),
        });

        app.accept_selectors(&["SEMANTICS", "MAX_STEPS"]);

        Self { solve, trigger_name, requested_semantics, requested_max_steps }
    }

    /// Creates a [`Go`] instance and returns it as a [`Command`]
    /// trait object.
    ///
    /// The [`App`] argument is modified, because [`Go`] is a
    /// top-level [`Command`] which accepts a set of CLI selectors
    /// (see [`App::accept_selectors()`]) and specifies an application
    /// mode.
    pub fn new_command(app: &mut App) -> Box<dyn Command> {
        app.set_mode("Go");
        Box::new(Self::new(app))
    }
}

impl Command for Go {
    fn name_of_log_file(&self) -> String {
        self.solve.name_of_log_file()
    }

    fn console_level(&self) -> Option<log::LevelFilter> {
        self.solve.console_level()
    }

    fn run(&self) -> Result<(), Box<dyn Error>> {
        self.solve.run()?;

        Ok(())
    }
}
