use std::error::Error;
use crate::{Context, CES, sat, error::AcesError};
use super::{App, Command};

pub struct Describe;

impl Command for Describe {
    fn run(app: &App) -> Result<(), Box<dyn Error>> {
        let main_path = app.value_of("MAIN_PATH").unwrap_or_else(|| unreachable!());

        let verbosity = app.occurrences_of("verbose");

        let ctx = Context::new_as_handle();

        let ces = CES::from_file(ctx.clone(), main_path)?;

        if verbosity >= 3 {
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
