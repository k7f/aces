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
    Port,
    Link,
    spec::{CESSpec, spec_from_str},
    sat::{self, ExtendFormula, FromAtomID, AddPolynomial},
    error::AcesError,
};

type SourceID = usize;
type SinkID = usize;
type LinkID = usize;

type Monomial = Vec<LinkID>;
type Polynomial = Vec<Monomial>;

#[derive(Debug)]
pub struct CES {
    context:     Arc<Mutex<Context>>,
    spec:        Box<dyn CESSpec>,
    causes:      BTreeMap<SourceID, Polynomial>,
    effects:     BTreeMap<SinkID, Polynomial>,
    links:       BTreeMap<LinkID, Option<bool>>,
    is_coherent: bool,
}

impl CES {
    pub fn from_str(ctx: &Arc<Mutex<Context>>, spec_str: &str) -> Result<Self, Box<dyn Error>> {

        let context = Arc::clone(ctx);

        let spec = spec_from_str(ctx, spec_str)?;

        let mut causes: BTreeMap<_, _> = Default::default();
        let mut effects: BTreeMap<_, _> = Default::default();
        let mut links: BTreeMap<_, _> = Default::default();
        let mut is_coherent = true;

        let mut ctx = ctx.lock().unwrap();

        for node_id in spec.get_source_ids() {

            if let Some(spec_poly) = spec.get_effects_by_id(node_id) {

                let node_name = ctx.get_node_name(node_id)
                    .ok_or(AcesError::NodeMissingForSource)?
                    .to_owned();

                let atom_id = ctx.atoms.take_port(Port::new_tx(node_name.clone(), node_id));

                let mut poly = Polynomial::new();

                for spec_mono in spec_poly {

                    let mut mono = Monomial::new();

                    for &conode_id in spec_mono {

                        let conode_name = ctx.get_node_name(conode_id)
                            .ok_or(AcesError::NodeMissingForSink)?
                            .to_owned();

                        let coatom_id = ctx.atoms.take_port(Port::new_rx(conode_name.clone(), conode_id));
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

                let node_name = ctx.get_node_name(node_id)
                    .ok_or(AcesError::NodeMissingForSink)?
                    .to_owned();

                let atom_id = ctx.atoms.take_port(Port::new_rx(node_name.clone(), node_id));

                let mut poly = Polynomial::new();

                for spec_mono in spec_poly {

                    let mut mono = Monomial::new();

                    for &conode_id in spec_mono {

                        let conode_name = ctx.get_node_name(conode_id)
                            .ok_or(AcesError::NodeMissingForSource)?
                            .to_owned();

                        let coatom_id = ctx.atoms.take_port(Port::new_tx(conode_name.clone(), conode_id));
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

        Ok(Self { context, spec, causes, effects, links, is_coherent })
    }

    pub fn from_file<P: AsRef<Path>>(ctx: &Arc<Mutex<Context>>, path: P) -> Result<Self, Box<dyn Error>> {
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

    pub fn get_formula(&self, ctx: &Arc<Mutex<Context>>) -> sat::CnfFormula {
        let mut formula = sat::CnfFormula::new();

        for (&port_id, poly) in self.causes.iter() {
            let ctx = ctx.lock().unwrap();

            if let Some(port) = ctx.get_port(port_id) {

                let port_lit = sat::Lit::from_atom_id(port_id, true);
                let node_id = port.get_node_id();

                // FIXME use a caching method get_antiport()
                for &antiport_id in self.effects.keys() {
                    if let Some(antiport) = ctx.get_port(antiport_id) {
                        if antiport.get_node_id() == node_id {
                            let antiport_lit = sat::Lit::from_atom_id(antiport_id, true);
                            let clause = &[port_lit, antiport_lit];

                            println!("add antiport clause {:?}", clause);
                            formula.add_clause(clause);

                            break
                        }
                    }
                }

                formula.add_polynomial(port_lit, poly);
            }
        }

        for (&port_id, poly) in self.effects.iter() {
            let port_lit = sat::Lit::from_atom_id(port_id, true);

            formula.add_polynomial(port_lit, poly);
        }

        formula
    }
}
