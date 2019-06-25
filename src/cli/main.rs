#![allow(clippy::toplevel_ref_arg)]

#[macro_use]
extern crate log;

use std::{path::PathBuf, error::Error};
use aces::cli::{App, Command, Describe, Validate};

fn setup_logger(with_file: Option<&str>, with_dir: Option<&str>, verbosity: u64) {
    use fern::{
        Dispatch,
        colors::{Color, ColoredLevelConfig},
    };

    // FIXME these should be configurable by the user
    let colors = ColoredLevelConfig::new()
        .trace(Color::Blue)
        .debug(Color::Yellow)
        .info(Color::Green)
        .warn(Color::Magenta)
        .error(Color::Red);

    let error_prefix =
        format!("[\x1B[{color}mERROR\x1B[0m]\x1B[{color}m", color = Color::Red.to_fg_str());
    let error_suffix = "\x1B[0m";

    let console_level = match verbosity {
        0 => log::LevelFilter::Warn,
        1 => log::LevelFilter::Info,
        2 => log::LevelFilter::Debug,
        _ => log::LevelFilter::Trace,
    };

    let mut main_logger = Dispatch::new();

    main_logger = main_logger.chain(
        Dispatch::new()
            .format(move |out, message, record| match record.level() {
                log::Level::Info => out.finish(format_args!("{}.", message)),
                log::Level::Warn | log::Level::Debug => {
                    out.finish(format_args!("[{}]\t{}.", colors.color(record.level()), message))
                }
                _ => out.finish(format_args!(
                    "[{}]\t\x1B[{}m{}.\x1B[0m",
                    colors.color(record.level()),
                    colors.get_color(&record.level()).to_fg_str(),
                    message
                )),
            })
            .level(console_level)
            .chain(std::io::stdout()),
    );

    let dir_path = match with_dir {
        Some(dirname) => {
            let path = PathBuf::from(dirname);

            if path.is_dir() {
                Some(path)

            } else if path.exists() {
                eprintln!(
                    "{}\tCan't use \"{}\" as a logging directory, because it exists and isn't a directory.{}",
                    error_prefix, path.display(), error_suffix
                );
                None

            } else if let Err(err) = std::fs::create_dir(&path) {
                eprintln!(
                    "{}\tCan't create \"{}\" directory: {}.{}",
                    error_prefix, path.display(), err, error_suffix
                );
                None

            } else {
                Some(path)
            }
        }
        None => {
            if with_file.is_some() {

                let path = PathBuf::from("log");

                if path.is_dir() {
                    Some(path)

                } else {
                    eprintln!(
                        "{}\tLogging to file is disabled, because directory \"log\" doesn't \
                         exist...\n\tCreate this directory or run '{} --log-dir <LOG_DIR> ...'.{}",
                        error_prefix, env!("CARGO_PKG_NAME"), error_suffix
                    );

                    None
                }
            } else {
                None
            }
        }
    };

    if let Some(filename) = with_file {
        if let Some(path) = dir_path {

            let path = path.join(filename);

            let log_file =
                std::fs::OpenOptions::new().write(true).create(true).append(false).open(path);

            match log_file {
                Ok(log_file) => {
                    main_logger = main_logger.chain(
                        Dispatch::new()
                            .format(move |out, message, record| {
                                out.finish(format_args!(
                                    "[{}][{}] {}.",
                                    record.target(),
                                    record.level(),
                                    message,
                                ))
                            })
                            .level(log::LevelFilter::Info)
                            .chain(log_file),
                    );
                }
                Err(err) => {
                    eprintln!("{}\t{}.{}", err, error_prefix, error_suffix);
                }
            }
        }
    }

    main_logger
        .apply()
        .unwrap_or_else(|err| eprintln!("{}\t{}.{}", err, error_prefix, error_suffix));
}

fn main() -> Result<(), Box<dyn Error>> {
    let ref cli_spec_str = include_str!("aces.cli");

    let cli_spec = clap::YamlLoader::load_from_str(cli_spec_str)?;
    let cli_matches = clap::App::from_yaml(&cli_spec[0]);
    let app = App::from_clap(cli_matches);

    let verbosity = app.occurrences_of("verbose");
    let dirname_to_log = app.value_of("LOG_DIR");
    let filename_to_log = if dirname_to_log.is_some() || app.is_present("log") { Some("aces.log") } else { None };

    setup_logger(filename_to_log, dirname_to_log, verbosity);

    let result = match app.subcommand_name().unwrap_or("describe") {
        "validate" => Validate::run(&app),
        "describe" => Describe::run(&app),
        _ => unreachable!(),
    };

    if let Err(err) = result {
        error!("{}", err);
        std::process::exit(-1)
    } else {
        std::process::exit(0)
    }
}
