#![allow(clippy::toplevel_ref_arg)]

#[macro_use]
extern crate log;

use std::error::Error;
use aces::Logger;
use aces::cli::{App, Solve, Validate};

fn main() -> Result<(), Box<dyn Error>> {
    let ref cli_spec_str = include_str!("aces.cli");

    let cli_spec = clap::YamlLoader::load_from_str(cli_spec_str)?;
    let cli_matches = clap::App::from_yaml(&cli_spec[0]);
    let app = App::from_clap(cli_matches);

    let command = match app.subcommand_name().unwrap_or("solve") {
        "solve" => Solve::new_command(&app),
        "validate" => Validate::new_command(&app),
        _ => unreachable!(),
    };

    let console_level = command.console_level().unwrap_or(match app.occurrences_of("verbose") {
        0 => log::LevelFilter::Warn,
        1 => log::LevelFilter::Info,
        2 => log::LevelFilter::Debug,
        _ => log::LevelFilter::Trace,
    });

    let mut logger = Logger::new(app.get_name()).with_console(console_level);

    if let Some(dirname) = app.value_of("LOG_DIR") {
        logger = logger.with_explicit_directory(dirname);
    }

    if app.is_present("log") || logger.get_directory().is_some() {
        let mut file_level = match app.occurrences_of("log") {
            0 | 1 => log::LevelFilter::Info,
            2 => log::LevelFilter::Debug,
            _ => log::LevelFilter::Trace,
        };
        if file_level < console_level {
            file_level = console_level;
        }

        logger = logger.with_file(command.name_of_log_file(), file_level);
    }

    logger.apply();

    if let Err(err) = command.run() {
        error!("{}", err);
        std::process::exit(-1)
    } else {
        std::process::exit(0)
    }
}
