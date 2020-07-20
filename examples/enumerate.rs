#[macro_use]
extern crate log;

use std::{fmt, error::Error};
use aces::{Permutation, Logger};

#[derive(Debug)]
struct EnError(String);

impl fmt::Display for EnError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Error for EnError {}

#[derive(Debug)]
struct App {
    name:      String,
    size:      usize,
    log_dir:   Option<String>,
    verbosity: u32,
}

impl App {
    fn new<S: AsRef<str>>(name: S) -> Result<Self, Box<dyn Error>> {
        const DEFAULT_SIZE: usize = 9;

        let name = name.as_ref().into();
        let mut log_dir = None;
        let mut verbosity = 0;
        let mut size = DEFAULT_SIZE;

        for (prev_arg, next_arg) in std::env::args().zip(std::env::args().skip(1)) {
            match next_arg.as_str() {
                "-v" => verbosity += 1,
                "-vv" => verbosity += 2,
                "-vvv" => verbosity += 3,
                "--log-dir" => {}
                "--size" => {}
                arg => {
                    if arg.starts_with('-') {
                        panic!("ERROR: Invalid CLI option \"{}\"", arg)
                    } else {
                        match prev_arg.as_str() {
                            "--log-dir" => log_dir = Some(arg.into()),
                            "--size" => size = arg.parse()?,
                            _ => {}
                        }
                    }
                }
            }
        }

        Ok(App { name, size, log_dir, verbosity })
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let app = App::new("Enumerate")?;

    let log_level = match app.verbosity {
        0 => log::LevelFilter::Info,
        1 => log::LevelFilter::Debug,
        _ => log::LevelFilter::Trace,
    };

    let mut logger = Logger::new(app.name).with_console(log_level);

    if let Some(log_dir) = app.log_dir {
        logger = logger.with_explicit_directory(log_dir);
    }

    if logger.get_directory().is_some() {
        logger = logger.with_file("enumerate.log", log_level);
    }

    logger.apply();

    let mut perm = Permutation::new(0..app.size).unwrap();
    let mut stack = vec![0; app.size];
    let mut count = 1_u64;

    while perm.heap_step(stack.as_mut_slice()) {
        count += 1;
    }

    assert_eq!(count, (2..app.size as u64 + 1).product());
    info!("Total {}, last {:?} (rank {})", count, perm, perm.rank());
    info!("Last unranked {:?}", Permutation::from_rank(perm.rank(), app.size));

    Ok(())
}
