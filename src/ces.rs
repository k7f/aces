use std::{io::Read, fs::File, path::Path, sync::{Mutex, Arc}, error::Error};
use crate::{NodeSpace, spec::CESSpec};

#[derive(Debug)]
pub struct CES {
    spec: CESSpec,
}

impl CES {
    pub fn from_str(spec: &str, nodes: Arc<Mutex<NodeSpace>>) -> Result<Self, Box<dyn Error>> {
        let spec = CESSpec::from_str(spec, nodes)?;

        Ok(Self { spec })
    }

    pub fn from_file<P: AsRef<Path>>(path: P, nodes: Arc<Mutex<NodeSpace>>) -> Result<Self, Box<dyn Error>> {
        let mut fp = File::open(path)?;
        let mut spec = String::new();
        fp.read_to_string(&mut spec)?;

        Self::from_str(&spec, nodes)
    }
}
