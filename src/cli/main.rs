#![allow(clippy::toplevel_ref_arg)]

use std::{path::PathBuf, error::Error};
use aces::cli::{App, Command, Describe, Validate};

fn setup_logger(verbosity: u64) {
    if verbosity > 0 {
        let mut dispatcher = fern::Dispatch::new()
            .format(|out, message, record| {
                out.finish(format_args!(
                    "[{}][{}] {}.",
                    record.target(),
                    record.level(),
                    message,
                ))
            })
            .level(log::LevelFilter::Info)
            .chain(std::io::stdout());

        let mut path = PathBuf::from("log");

        if path.exists() {
            path.set_file_name("aces.log");

            match fern::log_file(path) {
                Ok(log_file) => dispatcher = dispatcher.chain(log_file),
                Err(err) => eprintln!("[ERROR] {}.", err),
            }
        } else {
            eprintln!("[WARN] Logging to file disabled, because directory \"log\" doesn't exist.");
        }

        dispatcher.apply().unwrap_or_else(|err| eprintln!("[ERROR] {}.", err));
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let ref cli_spec_str = include_str!("aces.cli");

    let cli_spec = clap::YamlLoader::load_from_str(cli_spec_str)?;
    let cli_matches = clap::App::from_yaml(&cli_spec[0]);
    let app = App::from_clap(cli_matches);

    let verbosity = app.occurrences_of("verbose");
    setup_logger(verbosity);

    let result = match app.subcommand_name().unwrap_or("describe") {
        "validate" => Validate::run(&app),
        "describe" => Describe::run(&app),
        _ => unreachable!(),
    };

    if let Err(err) = result {
        eprintln!("[ERROR] {}.", err);
        std::process::exit(-1)
    } else {
        std::process::exit(0)
    }
}
