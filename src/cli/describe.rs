use std::{str::FromStr, error::Error};
use crate::{Context, ContentOrigin, CES, sat, error::AcesError};
use super::{App, Command};

pub struct Describe {
    main_path:    String,
    minimal_mode: bool,
    verbosity:    u64,
}

impl Describe {
    pub fn new_command(app: &App) -> Box<dyn Command> {
        let main_path = app.value_of("MAIN_PATH").unwrap_or_else(|| unreachable!()).to_owned();
        let minimal_mode = !app.is_present("all");
        let verbosity = app.occurrences_of("verbose").max(app.occurrences_of("log"));

        Box::new(Self { main_path, minimal_mode, verbosity })
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

    fn console_level(&self) -> Option<log::LevelFilter> {
        Some(match self.verbosity {
            0 => log::LevelFilter::Warn,
            1 => log::LevelFilter::Info,
            2 => log::LevelFilter::Debug,
            _ => log::LevelFilter::Trace,
        })
    }

    fn run(&self) -> Result<(), Box<dyn Error>> {
        let ctx = Context::new_toplevel("describe", ContentOrigin::cex_script(&self.main_path));

        let ces = CES::from_file(ctx.clone(), &self.main_path)?;

        trace!("{:?}", ctx.lock().unwrap());
        trace!("{:?}", ces);
        // FIXME impl Display
        // info!("{}", ces);

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

            info!("Start of {}-solution search", if self.minimal_mode { "min" } else { "all" });
            solver.set_minimal_mode(self.minimal_mode);

            if let Some(first_solution) = solver.next() {
                debug!("1. Raw {:?}", first_solution);
                println!("1. Solution: {}", first_solution);

                for (count, solution) in solver.enumerate() {
                    debug!("{}. Raw {:?}", count + 2, solution);
                    println!("{}. Solution: {}", count + 2, solution);
                }
            } else if solver.is_sat().is_some() {
                println!("\nStructural deadlock (found no solutions).");
            } else if solver.was_interrupted() {
                warn!("Solving was interrupted");
            } else if let Some(Err(err)) = solver.take_last_result() {
                error!("Solving failed: {}", err);
            } else {
                unreachable!()
            }

            Ok(())
        }
    }
}
