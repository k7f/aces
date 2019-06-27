use std::{str::FromStr, error::Error};
use crate::{Context, CES, sat, error::AcesError};
use super::{App, Command};

pub struct Describe {
    main_path: String,
    verbosity: u64,
}

impl Describe {
    pub fn new_command(app: &App) -> Box<dyn Command> {
        let main_path = app.value_of("MAIN_PATH").unwrap_or_else(|| unreachable!()).to_owned();
        let verbosity = app.occurrences_of("verbose").max(app.occurrences_of("log"));

        Box::new(Self { main_path, verbosity })
    }
}

impl Command for Describe {
    fn name_of_log_file(&self) -> String {
        if let Ok(mut path) = std::path::PathBuf::from_str(&self.main_path) {
            if path.set_extension("log") {
                if let Some(file_name) = path.file_name() {
                    return file_name.to_str().unwrap().to_owned()
                } else {
                }
            } else {
            }
        } else {
        }

        "aces.log".to_owned()
    }

    fn run(&self) -> Result<(), Box<dyn Error>> {
        let ctx = Context::new_as_handle();

        let ces = CES::from_file(ctx.clone(), &self.main_path)?;

        if self.verbosity >= 3 {
            trace!("{:?}", ctx.lock().unwrap());
        } else {
            debug!("{:?}", ctx.lock().unwrap().nodes);
        }

        // FIXME display not debug
        info!("{:?}", ces);

        if !ces.is_coherent() {
            Err(Box::new(AcesError::CESIsIncoherent(
                ces.get_name().unwrap_or("anonymous").to_owned(),
            )))
        } else {
            let formula = ces.get_formula();

            debug!("Raw {:?}", formula);
            info!("Formula: {}", formula);

            let mut solver = sat::Solver::new(ctx.clone());
            solver.add_formula(&formula);
            solver.inhibit_empty_solution();

            match solver.solve() {
                Ok(true) => {
                    if let Some(solution) = solver.get_solution() {
                        debug!("Raw {:?}", solution);
                        println!("Solution: {}", solution);
                    }
                }
                Ok(false) => {
                    println!("\nStructural deadlock (found no solutions).");
                }
                _ => {} // FIXME
            }

            Ok(())
        }
    }
}
