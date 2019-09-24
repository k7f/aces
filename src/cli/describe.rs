use std::{str::FromStr, error::Error};
use crate::{Context, ContentOrigin, CEStructure, sat};
use super::{App, Command};

pub struct Describe {
    verbosity:           u64,
    delayed_warnings:    Vec<String>, // Delayed until after logger's setup.
    main_path:           String,
    requested_encoding:  Option<sat::Encoding>,
    requested_reduction: Option<bool>,
}

impl Describe {
    pub fn new_command(app: &App) -> Box<dyn Command> {
        let verbosity = app.occurrences_of("verbose").max(app.occurrences_of("log"));
        let mut delayed_warnings = Vec::new();
        let main_path = app.value_of("MAIN_PATH").unwrap_or_else(|| unreachable!()).to_owned();

        let requested_encoding = {
            let short_encoding = app.value_of("ENCODING").map(|v| match v {
                "PL" => sat::Encoding::PortLink,
                "FJ" => sat::Encoding::ForkJoin,
                _ => unreachable!(),
            });
            let long_encoding = if app.is_present("PORT_LINK") {
                if app.is_present("FORK_JOIN") {
                    delayed_warnings.push("Conflicting encoding requests, none applied".to_owned());
                    None
                } else {
                    Some(sat::Encoding::PortLink)
                }
            } else if app.is_present("FORK_JOIN") {
                Some(sat::Encoding::ForkJoin)
            } else {
                None
            };

            if short_encoding.is_none() {
                long_encoding
            } else if long_encoding.is_none() || long_encoding == short_encoding {
                short_encoding
            } else {
                delayed_warnings.push("Conflicting encoding requests, none applied".to_owned());
                None
            }
        };

        let requested_reduction = {
            if app.is_present("all") {
                Some(true)
            } else {
                None
            }
        };

        Box::new(Self {
            verbosity,
            delayed_warnings,
            main_path,
            requested_encoding,
            requested_reduction,
        })
    }
}

impl Command for Describe {
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
        for warning in self.delayed_warnings.iter() {
            warn!("{}", warning);
        }

        let ctx = Context::new_toplevel("describe", ContentOrigin::cex_script(&self.main_path));

        if let Some(encoding) = self.requested_encoding {
            ctx.lock().unwrap().set_encoding(encoding);
        }

        if let Some(reduction) = self.requested_reduction {
            ctx.lock().unwrap().set_reduction(reduction);
        }

        let mut ces = CEStructure::from_file(&ctx, &self.main_path)?;

        trace!("{:?}", ctx.lock().unwrap());
        trace!("{:?}", ces);
        // FIXME impl Display
        // info!("{}", ces);

        ces.solve()?;

        if let Some(fcs) = ces.get_firing_components() {
            println!("Firing components:");

            let ctx = ctx.lock().unwrap();

            for (i, fc) in fcs.iter().enumerate() {
                println!("{}. {}", i + 1, ctx.with(fc));
            }
        }

        Ok(())
    }
}
