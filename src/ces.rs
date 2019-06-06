use std::{collections::BTreeMap, io::Read, fs::File, path::Path, sync::{Mutex, Arc}, error::Error};
use crate::{NodeSpace, spec::{CESSpec, spec_from_str}};

type SourceID = usize;
type SinkID = usize;

#[derive(Debug)]
struct Polynomial;

#[derive(Debug)]
pub struct CES {
    spec:    Box<dyn CESSpec>,
    causes:  BTreeMap<SourceID, Polynomial>,
    effects: BTreeMap<SinkID, Polynomial>,
}

impl CES {
    pub fn from_str(spec_str: &str, nodes: Arc<Mutex<NodeSpace>>) -> Result<Self, Box<dyn Error>> {
        let spec = spec_from_str(spec_str, nodes)?;

        Ok(Self {
            spec,
            causes: Default::default(),
            effects: Default::default(),
        })
    }

    pub fn from_file<P: AsRef<Path>>(path: P, nodes: Arc<Mutex<NodeSpace>>) -> Result<Self, Box<dyn Error>> {
        let mut fp = File::open(path)?;
        let mut spec = String::new();
        fp.read_to_string(&mut spec)?;

        Self::from_str(&spec, nodes)
    }
}
