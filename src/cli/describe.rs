use std::{sync::{Mutex, Arc}, error::Error};
use crate::{Context, CES};
use super::{App, Command};

pub struct Describe;

impl Command for Describe {
    fn run(app: &App) -> Result<(), Box<dyn Error>> {
        let main_path = app.value_of("MAIN_PATH")
            .unwrap_or_else(|| unreachable!());

        let verbosity = app.occurrences_of("verbose");

        let ctx = Arc::new(Mutex::new(Context::new()));
        let ces = CES::from_file(Arc::clone(&ctx), main_path)?;

        if verbosity >= 1 {
            println!("{:?}", ctx.lock().unwrap());
        }
        println!("{:?}", ces);

        Ok(())
    }
}
