use std::{str::FromStr, error::Error};
use crate::{Context, ContextHandle, ContentOrigin, CEStructure, sat};
use super::{App, Command};

pub struct Solve {
    verbosity:          u64,
    main_path:          String,
    more_paths:         Vec<String>,
    requested_encoding: Option<sat::Encoding>,
    requested_search:   Option<sat::Search>,
    context:            ContextHandle,
}

impl Solve {
    pub(crate) fn new(app: &mut App) -> Self {
        let verbosity = app.occurrences_of("verbose").max(app.occurrences_of("log"));

        let mut path_values = app.values_of("MAIN_PATH").unwrap_or_else(|| unreachable!());
        let main_path = path_values.next().unwrap_or_else(|| unreachable!()).to_owned();
        let more_paths: Vec<_> = path_values.map(|p| p.to_owned()).collect();

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

        let context_name =
            format!("aces-{}", app.get_mode().expect("unexpected anonymous mode").to_lowercase());
        let context = Context::new_toplevel(context_name, ContentOrigin::cex_script(&main_path));

        app.accept_selectors(&["SAT_ENCODING", "SAT_SEARCH"]);

        Self { verbosity, main_path, more_paths, requested_encoding, requested_search, context }
    }

    /// Creates a [`Solve`] instance and returns it as a [`Command`]
    /// trait object.
    ///
    /// The [`App`] argument is modified, because [`Solve`] is a
    /// top-level [`Command`] which accepts a set of CLI selectors
    /// (see [`App::accept_selectors()`]) and specifies an application
    /// mode.
    pub fn new_command(app: &mut App) -> Box<dyn Command> {
        app.set_mode("Solve");
        Box::new(Self::new(app))
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
        if let Some(encoding) = self.requested_encoding {
            self.context.lock().unwrap().set_encoding(encoding);
        }

        if let Some(search) = self.requested_search {
            self.context.lock().unwrap().set_search(search);
        }

        let mut ces = CEStructure::from_file(&self.context, &self.main_path)?;

        for path in self.more_paths.iter() {
            ces = ces.with_file(path)?;
        }

        trace!("{:?}", self.context.lock().unwrap());
        trace!("{:?}", ces);
        // FIXME impl Display
        // info!("{}", ces);

        ces.solve()?;

        if let Some(fs) = ces.get_firing_set() {
            println!("Firing components:");

            let ctx = self.context.lock().unwrap();

            for (i, fc) in fs.as_slice().iter().enumerate() {
                println!("{}. {}", i + 1, ctx.with(fc));
            }
        }

        Ok(())
    }
}
