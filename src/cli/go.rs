use std::error::Error;
use crate::{Semantics, Runner};
use super::{App, Command, Solve};

pub struct Go {
    solve:        Solve,
    trigger_name: Option<String>,
}

impl Go {
    pub(crate) fn new(app: &mut App) -> Self {
        let solve = Solve::new(app);

        let trigger_name = app.value_of("TRIGGER").map(String::from);

        if let Ok(mut ctx) = solve.get_context().lock() {
            if let Some(v) = app.value_of("SEMANTICS") {
                match v {
                    "seq" => ctx.set_semantics(Semantics::Sequential),
                    "par" => ctx.set_semantics(Semantics::Parallel),
                    _ => unreachable!(),
                }
            }

            if let Some(v) = app.value_of("MAX_STEPS") {
                match v.parse::<usize>() {
                    Ok(val) => ctx.set_max_steps(val),
                    Err(err) => {
                        panic!("The argument '{}' isn't a valid value of MAX_STEPS ({})", v, err)
                    }
                }
            }
        } else {
            // FIXME
            panic!()
        }

        app.accept_selectors(&["SEMANTICS", "MAX_STEPS"]);

        Self { solve, trigger_name }
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

    fn run(&mut self) -> Result<(), Box<dyn Error>> {
        self.solve.run()?;

        let ces = self.solve.get_ces();

        if let Some(fset) = ces.get_firing_set() {
            let mut runner = Runner::new(
                ces.get_context(),
                self.trigger_name.as_ref().map(|s| s.as_str()).unwrap_or("Start"),
            );

            runner.go(fset);

            let fcs = runner.get_firing_sequence();
            let num_steps = fcs.len();
            let mut state = runner.get_initial_state().clone();
            let ctx = ces.get_context().lock().unwrap();

            for (i, fc) in fcs.iter(fset).enumerate() {
                if i == 0 {
                    println!("Go from {}", ctx.with(&state));
                } else {
                    println!("     => {}", ctx.with(&state));
                }

                println!("{}. {}", i + 1, ctx.with(fc));

                fc.fire(&mut state);
            }

            if num_steps < runner.get_max_steps() {
                println!("     => {}", ctx.with(&state));
                println!("Deadlock after {} steps.", num_steps);
            } else {
                println!("Stop at {}", ctx.with(&state));
                println!("Done after {} steps.", num_steps);
            }
        } else {
            println!("Structural deadlock.");
        }

        Ok(())
    }
}
