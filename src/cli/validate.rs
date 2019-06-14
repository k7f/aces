use std::{
    sync::{Mutex, Arc},
    error::Error,
};
use crate::{Context, CES, error::AcesError};
use super::{App, Command};

pub struct Validate;

impl Command for Validate {
    fn run(app: &App) -> Result<(), Box<dyn Error>> {
        let glob_path = app.value_of("GLOB_PATH").unwrap_or_else(|| unreachable!());

        let do_abort = app.is_present("abort");
        let syntax_only = app.is_present("syntax");
        let recursive = app.is_present("recursive");
        let verbosity = app.occurrences_of("verbose");

        // FIXME
        let ref glob_path = format!("{}/*.ces", glob_path);

        let mut num_bad_files = 0;
        let ref ctx = Arc::new(Mutex::new(Context::new()));

        if recursive {
            // FIXME
        }

        match glob::glob(glob_path) {
            Ok(path_list) => {
                for entry in path_list {
                    match entry {
                        Ok(ref path) => {
                            if verbosity >= 1 {
                                println!("> {}", path.display());
                            }
                            let result = CES::from_file(ctx, path);
                            match result {
                                Ok(ces) => {
                                    if verbosity >= 2 {
                                        println!("+++ {:?}", ces);
                                    }

                                    if !syntax_only && !ces.is_coherent() {
                                        if do_abort {
                                            println!("... Aborting on structural error.");
                                        }

                                        eprintln!(
                                            "!!! Structural error in file '{}'...",
                                            path.display()
                                        );

                                        let err = AcesError::CESIsIncoherent(
                                            ces.get_name().unwrap_or("anonymous").to_owned(),
                                        );

                                        if do_abort {
                                            return Err(Box::new(err))
                                        } else {
                                            eprintln!("[Error] {}.", err);
                                            num_bad_files += 1;
                                        }
                                    }
                                }
                                Err(err) => {
                                    if do_abort {
                                        println!("... Aborting on syntax error.");
                                    }

                                    eprintln!("!!! Syntax error in file '{}'...", path.display());

                                    if do_abort {
                                        return Err(err)
                                    } else {
                                        eprintln!("[Error] {}.", err);
                                        num_bad_files += 1;
                                    }
                                }
                            }
                        }
                        Err(err) => {
                            eprintln!("??? Bad entry in path list: {}", err);
                        }
                    }
                }

                print!("... Done ");
                if num_bad_files > 0 {
                    println!(
                        "({} bad file{}).",
                        num_bad_files,
                        if num_bad_files == 1 { "" } else { "s" },
                    );
                } else {
                    println!("(no bad files).");
                }

                if verbosity >= 3 {
                    if verbosity >= 4 {
                        println!("{:?}", ctx.lock().unwrap());
                    } else {
                        println!("{:?}", ctx.lock().unwrap().nodes);
                    }
                }

                Ok(())
            }
            Err(err) => panic!("Invalid glob pattern: {}", err),
        }
    }
}
