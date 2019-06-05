use std::{sync::{Mutex, Arc}, error::Error};
use crate::{NodeSpace, CES};
use super::{App, Command};

pub struct Describe;

impl Command for Describe {
    fn run(app: &App) -> Result<(), Box<dyn Error>> {
        let main_path = app.value_of("MAIN_PATH")
            .unwrap_or_else(|| unreachable!());

        let verbosity = app.occurrences_of("verbose");

        let nodes = Arc::new(Mutex::new(NodeSpace::new()));
        let ces = CES::from_file(main_path, Arc::clone(&nodes))?;

        if verbosity >= 1 {
            println!("{:?}", nodes.lock().unwrap());
        }
        println!("{:?}", ces);

        Ok(())
    }
}
