use std::{sync::{Mutex, Arc}, error::Error};
use aces::{NodeSpace, CES};

fn validate(clap_matches: &clap::ArgMatches, glob_path: &str) -> Result<(), Box<dyn Error>> {
    let do_abort = clap_matches.is_present("abort");
    let syntax_only = clap_matches.is_present("syntax");
    let recursive = clap_matches.is_present("recursive");
    let verbosity = clap_matches.occurrences_of("verbose");

    // FIXME
    let ref glob_path = format!("{}/*.ces", glob_path);

    let mut num_bad_files = 0;
    let nodes = Arc::new(Mutex::new(NodeSpace::new()));

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
                        let result = CES::from_file(path, Arc::clone(&nodes));
                        match result {
                            Ok(ces) => {
                                if verbosity >= 2 {
                                    println!("+++ {:?}", ces);
                                }
                            }
                            Err(err) => {
                                if do_abort {
                                    println!("... Aborting on error.");
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

                        if !syntax_only {
                            // FIXME
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
                println!("{:?}", nodes.lock().unwrap());
            }

            Ok(())
        }
        Err(err) => {
            panic!("Invalid glob pattern: {}", err)
        }
    }
}

fn doit(clap_matches: &clap::ArgMatches, main_path: &str) -> Result<(), Box<dyn Error>> {
    let verbosity = clap_matches.occurrences_of("verbose");

    let nodes = Arc::new(Mutex::new(NodeSpace::new()));
    let ces = CES::from_file(main_path, Arc::clone(&nodes))?;

    if verbosity >= 1 {
        println!("{:?}", nodes.lock().unwrap());
    }
    println!("{:?}", ces);

    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    let clap_spec = clap::YamlLoader::load_from_str(include_str!("aces.cli"))?;
    let clap_matches = clap::App::from_yaml(&clap_spec[0])
        .name(env!("CARGO_PKG_NAME"))
        .version(env!("CARGO_PKG_VERSION"))
        .author(env!("CARGO_PKG_AUTHORS"))
        .about(env!("CARGO_PKG_DESCRIPTION"))
        .get_matches();

    let mut result;

    if let Some(matches) = clap_matches.subcommand_matches("validate") {
        if let Some(glob_path) = matches.value_of("GLOB_PATH") {
            result = validate(matches, glob_path)

        } else {
            unreachable!()
        }
    } else if let Some(main_path) = clap_matches.value_of("MAIN_PATH") {
        result = doit(&clap_matches, main_path)

    } else {
        unreachable!()
    }

    if let Err(err) = result {
        eprintln!("[Error] {}.", err);
        std::process::exit(-1)
    } else {
        std::process::exit(0)
    }
}
