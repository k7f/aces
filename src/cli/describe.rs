use std::{sync::{Mutex, Arc}, error::Error};
use crate::{Context, CES, sat::{self, IntoAtomID}, error::AcesError};
use super::{App, Command};

pub struct Describe;

impl Command for Describe {
    fn run(app: &App) -> Result<(), Box<dyn Error>> {
        let main_path = app.value_of("MAIN_PATH")
            .unwrap_or_else(|| unreachable!());

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
            Err(Box::new(AcesError::CESIsIncoherent(ces.get_name().to_owned())))
        } else {
            let formula = ces.get_formula(ctx);
            println!("\nCNF: {:?}", formula);

            let mut solver = sat::Solver::new();
            solver.add_formula(&formula);
            match solver.solve() {
                Ok(true) => {
                    if let Some(model) = solver.model() {
                        println!("\nModel: {:?}", model);

                        print!("Solution: ");

                        //solution = model.as_solution(ctx);

                        let mut solution = (Vec::new(), Vec::new());
                        let ctx = ctx.lock().unwrap();
                        for lit in model {
                            if lit.is_positive() {
                                let (atom_id, _) = lit.into_atom_id();
                                if let Some(source) = ctx.get_source(atom_id) {
                                    solution.0.push(source);
                                } else if let Some(sink) = ctx.get_sink(atom_id) {
                                    solution.1.push(sink);
                                }
                            }
                        }

                        if solution.0.is_empty() {
                            print!("{{}} => {{");
                        } else {
                            print!("{{");
                            for atom in solution.0 {
                                print!(" {}", atom);
                            }
                            print!(" }} => {{");
                        }
                        if solution.1.is_empty() {
                            println!("}}");
                        } else {
                            for atom in solution.1 {
                                print!(" {}", atom);
                            }
                            println!(" }}");
                        }
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
