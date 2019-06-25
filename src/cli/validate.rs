use std::error::Error;
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

        let ctx = Context::new_as_handle();

        if recursive {
            // FIXME
        }

        match glob::glob(glob_path) {
            Ok(path_list) => {
                for entry in path_list {
                    match entry {
                        Ok(ref path) => {
                            if verbosity >= 1 {
                                info!("> {}", path.display());
                            }
                            let result = CES::from_file(ctx.clone(), path);
                            match result {
                                Ok(ces) => {
                                    if verbosity >= 2 {
                                        debug!("{:?}", ces);
                                    }

                                    if !syntax_only && !ces.is_coherent() {
                                        let err = AcesError::CESIsIncoherent(
                                            ces.get_name().unwrap_or("anonymous").to_owned(),
                                        );

                                        if do_abort {
                                            warn!("Aborting on structural error");
                                            return Err(Box::new(err))
                                        } else {
                                            error!(
                                                "Structural error in file '{}'...\n\t{}",
                                                path.display(),
                                                err
                                            );
                                            num_bad_files += 1;
                                        }
                                    }
                                }
                                Err(err) => {
                                    if do_abort {
                                        warn!("Aborting on syntax error");
                                        return Err(err)
                                    } else {
                                        error!(
                                            "Syntax error in file '{}'...\n\t{}",
                                            path.display(),
                                            err
                                        );
                                        num_bad_files += 1;
                                    }
                                }
                            }
                        }
                        Err(err) => {
                            error!("Bad entry in path list: {}", err);
                        }
                    }
                }

                if num_bad_files > 0 {
                    println!(
                        "... Done ({} bad file{}).",
                        num_bad_files,
                        if num_bad_files == 1 { "" } else { "s" },
                    );
                } else {
                    println!("... Done (no bad files).");
                }

                if verbosity >= 3 {
                    if verbosity >= 4 {
                        trace!("{:?}", ctx.lock().unwrap());
                    } else {
                        trace!("{:?}", ctx.lock().unwrap().nodes);
                    }
                }

                Ok(())
            }
            Err(err) => panic!("Invalid glob pattern: {}", err),
        }
    }
}
