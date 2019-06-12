use std::{
    sync::{Mutex, Arc},
    error::Error,
};
use crate::{
    Context, CES,
    sat::{Solver, Solution},
    error::AcesError,
};
use super::{App, Command};

pub struct Describe;

impl Command for Describe {
    fn run(app: &App) -> Result<(), Box<dyn Error>> {
        let main_path = app.value_of("MAIN_PATH").unwrap_or_else(|| unreachable!());

        let verbosity = app.occurrences_of("verbose");

        let ref ctx = Arc::new(Mutex::new(Context::new()));
        let ces = CES::from_file(ctx, main_path)?;

        if verbosity >= 1 {
            if verbosity >= 2 {
                println!("{:?}", ctx.lock().unwrap());
            } else {
                println!("{:?}", ctx.lock().unwrap().nodes);
            }
        }

        // FIXME display not debug
        println!("{:?}", ces);

        if !ces.is_coherent() {
            Err(Box::new(AcesError::CESIsIncoherent(
                ces.get_name().unwrap_or("anonymous").to_owned(),
            )))
        } else {
            let formula = ces.get_formula();
            println!("\nCNF: {:?}", formula);

            println!("Formula: {}", formula.show(ctx));

            let mut solver = Solver::new();
            solver.add_formula(&formula);
            solver.inhibit_empty_solution();

            match solver.solve() {
                Ok(true) => {
                    if let Some(model) = solver.get_model() {
                        println!("\nModel: {:?}", model);

                        let solution = Solution::from_model(ctx, model);
                        println!("Solution: {}", &solution.show(ctx));
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
