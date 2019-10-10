use std::{str::FromStr, error::Error};
use crate::{Context, ContentOrigin, CEStructure, sat};
use super::{App, Command};

pub struct Solve {
    verbosity:          u64,
    main_path:          String,
    requested_encoding: Option<sat::Encoding>,
    requested_search:   Option<sat::Search>,
}

impl Solve {
    pub fn new_command(app: &App) -> Box<dyn Command> {
        let verbosity = app.occurrences_of("verbose").max(app.occurrences_of("log"));
        let main_path = app.value_of("MAIN_PATH").unwrap_or_else(|| unreachable!()).to_owned();

        let requested_encoding = app.value_of("SAT_ENCODING").map(|v| match v {
            "PL" | "port-link" => sat::Encoding::PortLink,
            "FJ" | "fork-join" => sat::Encoding::ForkJoin,
            _ => unreachable!(),
        });

        let requested_search = app.value_of("SAT_SEARCH").map(|v| match v {
            "min" => sat::Search::MinSolutions,
            "all" => sat::Search::AllSolutions,
            _ => unreachable!(),
        });

        Box::new(Self { verbosity, main_path, requested_encoding, requested_search })
    }
}

impl Command for Solve {
    fn name_of_log_file(&self) -> String {
        if let Ok(mut path) = std::path::PathBuf::from_str(&self.main_path) {
            if path.set_extension("log") {
                if let Some(file_name) = path.file_name() {
                    return file_name.to_str().unwrap().to_owned()
                } else {
                }
            } else {
            }
        } else {
        }

        "aces.log".to_owned()
    }

    fn console_level(&self) -> Option<log::LevelFilter> {
        Some(match self.verbosity {
            0 => log::LevelFilter::Warn,
            1 => log::LevelFilter::Info,
            2 => log::LevelFilter::Debug,
            _ => log::LevelFilter::Trace,
        })
    }

    fn run(&self) -> Result<(), Box<dyn Error>> {
        let ctx = Context::new_toplevel("aces-solve", ContentOrigin::cex_script(&self.main_path));

        if let Some(encoding) = self.requested_encoding {
            ctx.lock().unwrap().set_encoding(encoding);
        }

        if let Some(search) = self.requested_search {
            ctx.lock().unwrap().set_search(search);
        }

        let mut ces = CEStructure::from_file(&ctx, &self.main_path)?;

        trace!("{:?}", ctx.lock().unwrap());
        trace!("{:?}", ces);
        // FIXME impl Display
        // info!("{}", ces);

        ces.solve()?;

        if let Some(fs) = ces.get_firing_set() {
            println!("Firing components:");

            let ctx = ctx.lock().unwrap();

            for (i, fc) in fs.as_slice().iter().enumerate() {
                println!("{}. {}", i + 1, ctx.with(fc));
            }
        }

        Ok(())
    }
}
