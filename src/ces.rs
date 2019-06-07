use std::{
    collections::BTreeMap,
    io::Read,
    fs::File,
    path::Path,
    sync::{Mutex, Arc},
    error::Error,
};

use crate::{
    Context,
    Source,
    Sink,
    Link,
    spec::{CESSpec, spec_from_str},
    error::AcesError,
};

type SourceID = usize;
type SinkID = usize;
type LinkID = usize;

type Monomial = Vec<usize>;
type Polynomial = Vec<Monomial>;

#[derive(Debug)]
pub struct CES {
    spec:        Box<dyn CESSpec>,
    causes:      BTreeMap<SourceID, Polynomial>,
    effects:     BTreeMap<SinkID, Polynomial>,
    links:       BTreeMap<LinkID, Option<bool>>,
    is_coherent: bool,
}

impl CES {
    pub fn from_str(ctx: Arc<Mutex<Context>>, spec_str: &str) -> Result<Self, Box<dyn Error>> {

        let spec = spec_from_str(ctx.clone(), spec_str)?;

        let mut causes: BTreeMap<_, _> = Default::default();
        let mut effects: BTreeMap<_, _> = Default::default();
        let mut links: BTreeMap<_, _> = Default::default();
        let mut is_coherent = true;

        let mut ctx = ctx.lock().unwrap();

        for node_id in spec.get_source_ids() {

            if let Some(spec_poly) = spec.get_effects_by_id(node_id) {

                let node_name = ctx.nodes.get_name(node_id)
                    .ok_or(AcesError::NodeMissingForSource)?
                    .to_owned();

                let atom_id = ctx.atoms.take_source(Source::new(node_name.clone(), node_id));

                let mut poly = Polynomial::new();

                for spec_mono in spec_poly {

                    let mut mono = Monomial::new();

                    for &conode_id in spec_mono {

                        let conode_name = ctx.nodes.get_name(conode_id)
                            .ok_or(AcesError::NodeMissingForSink)?
                            .to_owned();

                        let coatom_id = ctx.atoms.take_sink(Sink::new(conode_name.clone(), conode_id));
                        let link_id = ctx.atoms.take_link(
                            Link::new(
                                atom_id,
                                node_name.clone(),
                                node_id,
                                coatom_id,
                                conode_name,
                                conode_id,
                            )
                        );

                        mono.push(link_id);

                        // link's occurence in causes is missing
                        links.insert(link_id, Some(false));
                    }
                    poly.push(mono);
                }

                effects.insert(atom_id, poly);
            }
        }

        // FIXME dry
        for node_id in spec.get_sink_ids() {

            if let Some(spec_poly) = spec.get_causes_by_id(node_id) {

                let node_name = ctx.nodes.get_name(node_id)
                    .ok_or(AcesError::NodeMissingForSink)?
                    .to_owned();

                let atom_id = ctx.atoms.take_sink(Sink::new(node_name.clone(), node_id));

                let mut poly = Polynomial::new();

                for spec_mono in spec_poly {

                    let mut mono = Monomial::new();

                    for &conode_id in spec_mono {

                        let conode_name = ctx.nodes.get_name(conode_id)
                            .ok_or(AcesError::NodeMissingForSource)?
                            .to_owned();

                        let coatom_id = ctx.atoms.take_source(Source::new(conode_name.clone(), conode_id));
                        let link_id = ctx.atoms.take_link(
                            Link::new(
                                coatom_id,
                                conode_name,
                                conode_id,
                                atom_id,
                                node_name.clone(),
                                node_id,
                            )
                        );

                        mono.push(link_id);

                        if let Some(what_missing) = links.get_mut(&link_id) {
                            // link occurs in causes
                            if *what_missing == Some(false) {
                                // and it occurs in effects
                                *what_missing = None;
                            }
                        } else {
                            // link's occurence in effects is missing
                            links.insert(link_id, Some(true));
                            is_coherent = false;
                        }
                    }
                    poly.push(mono);
                }

                causes.insert(atom_id, poly);
            }
        }

        if is_coherent {
            for what_missing in links.values() {
                if what_missing.is_some() {
                    is_coherent = false;
                    break
                }
            }
        }

        Ok(Self { spec, causes, effects, links, is_coherent })
    }

    pub fn from_file<P: AsRef<Path>>(ctx: Arc<Mutex<Context>>, path: P) -> Result<Self, Box<dyn Error>> {
        let mut fp = File::open(path)?;
        let mut spec = String::new();
        fp.read_to_string(&mut spec)?;

        Self::from_str(ctx, &spec)
    }

    pub fn get_name(&self) -> &str {
        self.spec.get_name()
    }

    pub fn is_coherent(&self) -> bool {
        self.is_coherent
    }
}
