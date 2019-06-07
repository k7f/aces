use std::{sync::{Mutex, Arc}, error::Error};
use crate::{Context, CES, sat::{self, IntoAtomID}, error::AcesError};
use super::{App, Command};

pub struct Describe;

impl Command for Describe {
    fn run(app: &App) -> Result<(), Box<dyn Error>> {
        let main_path = app.value_of("MAIN_PATH")
            .unwrap_or_else(|| unreachable!());

        let verbosity = app.occurrences_of("verbose");

        let ctx = Arc::new(Mutex::new(Context::new()));
        let ces = CES::from_file(Arc::clone(&ctx), main_path)?;

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
            Err(Box::new(AcesError::CESIsIncoherent(ces.get_name().to_owned())))
        } else {
            let formula = ces.get_formula(ctx.clone());
            println!("\nCNF: {:?}", formula);

            let mut solver = sat::Solver::new();
            solver.add_formula(&formula);
            match solver.solve() {
                Ok(true) => {
                    if let Some(model) = solver.model() {
                        println!("\nModel: {:?}", model);

                        let mut solution = (Vec::new(), Vec::new());
                        let ctx = ctx.lock().unwrap();
                        for lit in model {
                            let (atom_id, is_true) = lit.into_atom_id();
                            if ctx.get_link(atom_id).is_none() {
                                if let Some(atom) = ctx.get_atom(atom_id) {
                                    if is_true {
                                        solution.0.push(atom);
                                    } else {
                                        solution.1.push(atom);
                                    }
                                }
                            }
                        }

                        print!("Solution: {{");
                        for atom in solution.0 {
                            print!(" {}", atom);
                        }
                        print!(" }} => {{");
                        for atom in solution.1 {
                            print!(" {}", atom);
                        }
                        println!(" }}");
                    }
                }
                Ok(false) => {
                    println!("\nFound no solutions...");
                }
                _ => {}  // FIXME
            }

            Ok(())
        }
    }
}
