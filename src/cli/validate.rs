use std::error::Error;
use crate::{Context, CES, error::AcesError};
use super::{App, Command};

pub struct Validate {
    glob_path:    String,
    do_abort:     bool,
    syntax_only:  bool,
    is_recursive: bool,
    verbosity:    u64,
}

impl Validate {
    pub fn new_command(app: &App) -> Box<dyn Command> {
        let glob_path = app.value_of("GLOB_PATH").unwrap_or_else(|| unreachable!()).to_owned();
        let do_abort = app.is_present("abort");
        let syntax_only = app.is_present("syntax");
        let is_recursive = app.is_present("recursive");
        let verbosity = app.occurrences_of("verbose").max(app.occurrences_of("log"));

        Box::new(Self { glob_path, do_abort, syntax_only, is_recursive, verbosity })
    }
}

impl Command for Validate {
    fn name_of_log_file(&self) -> String {
        "aces-validation.log".to_owned()
    }

    fn run(&self) -> Result<(), Box<dyn Error>> {
        // FIXME
        let ref glob_path = format!("{}/*.ces", self.glob_path);

        let mut num_bad_files = 0;

        let ctx = Context::new_as_handle();

        if self.is_recursive {
            // FIXME
        }

        match glob::glob(glob_path) {
            Ok(path_list) => {
                for entry in path_list {
                    match entry {
                        Ok(ref path) => {
                            if self.verbosity >= 1 {
                                info!("> {}", path.display());
                            }
                            let result = CES::from_file(ctx.clone(), path);
                            match result {
                                Ok(ces) => {
                                    if self.verbosity >= 2 {
                                        debug!("{:?}", ces);
                                    }

                                    if !self.syntax_only && !ces.is_coherent() {
                                        let err = AcesError::CESIsIncoherent(
                                            ces.get_name().unwrap_or("anonymous").to_owned(),
                                        );

                                        if self.do_abort {
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
                                    if self.do_abort {
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

                if self.verbosity >= 3 {
                    if self.verbosity >= 4 {
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
