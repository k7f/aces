use std::error::Error;
use crate::Semantics;
use super::{App, Command, Solve};

pub struct Go {
    solve:               Box<dyn Command>,
    requested_semantics: Option<Semantics>,
    requested_max_steps: Option<u64>,
}

impl Go {
    pub fn new_command(app: &App) -> Box<dyn Command> {
        let solve = Solve::new_command(app);

        let initial_state = app.value_of("TRIGGER").map(|trigger| {
            println!("start node: {}", trigger);
        });

        let requested_semantics = app.value_of("SEMANTICS").map(|v| match v {
            "seq" => Semantics::Sequential,
            "par" => Semantics::Parallel,
            _ => unreachable!(),
        });

        let requested_max_steps = app.value_of("MAX_STEPS").map(|v| match v.parse::<u64>() {
            Ok(val) => val,
            Err(_) => panic!("The argument '{}' isn't a valid value of MAX_STEPS", v),
        });

        Box::new(Self { solve, requested_semantics, requested_max_steps })
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
