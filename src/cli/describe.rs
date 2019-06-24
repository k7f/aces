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

        if verbosity >= 2 {
            if verbosity >= 3 {
                println!("{:?}", ctx.lock().unwrap());
            } else {
                println!("{:?}", ctx.lock().unwrap().nodes);
            }
        }

        if verbosity >= 1 {
            // FIXME display not debug
            println!("{:?}", ces);
        }

        if !ces.is_coherent() {
            Err(Box::new(AcesError::CESIsIncoherent(
                ces.get_name().unwrap_or("anonymous").to_owned(),
            )))
        } else {
            let formula = ces.get_formula();

            if verbosity >= 1 {
                println!("\nRaw {:?}", formula);

                if verbosity >= 2 {
                    println!("Formula: {}", formula);
                }
            }

            let mut solver = sat::Solver::new(ctx.clone());
            solver.add_formula(&formula);
            solver.inhibit_empty_solution();

            match solver.solve() {
                Ok(true) => {
                    if let Some(solution) = solver.get_solution() {
                        if verbosity >= 1 {
                            println!("\nRaw {:?}", solution);
                        }

                        println!("Solution: {}", solution);
                    }
                }
                Ok(false) => {
                    println!("\nFound no solutions...");
                }
                _ => {} // FIXME
            }

            Ok(())
        }
    }
}
