use std::{
    collections::BTreeMap,
    io::Read,
    fs::File,
    path::Path,
    sync::{Mutex, Arc},
    error::Error,
};

use crate::{
    Context, Port, Face, Link, Monomial, Polynomial,
    spec::{CESSpec, spec_from_str},
    atom::{TxID, RxID, LinkID},
    sat::{self, ExtendFormula, FromAtomID, AddPolynomial},
    error::AcesError,
};

#[derive(Debug)]
pub struct CES {
    context: Arc<Mutex<Context>>,
    spec:    Option<Box<dyn CESSpec>>,
    causes:  BTreeMap<TxID, Polynomial>,
    effects: BTreeMap<RxID, Polynomial>,
    links:   BTreeMap<LinkID, Option<Face>>,
    num_broken_links: u32,
}

impl CES {
    fn new(ctx: &Arc<Mutex<Context>>) -> Self {
        Self {
            context: Arc::clone(ctx),
            spec:    Default::default(),
            causes:  Default::default(),
            effects: Default::default(),
            links:   Default::default(),
            num_broken_links: 0,
        }
    }

    fn add_polynomial(
        &mut self,
        node_id:   usize,
        face:      Face,
        spec_poly: &Vec<Vec<usize>>,
    ) -> Result<(), Box<dyn Error>>
    {
        let mut ctx = self.context.lock().unwrap();

        let node_name = ctx.get_node_name(node_id)
            .ok_or(AcesError::NodeMissingForPort(face))?
            .to_owned();

        let atom_id = ctx.atoms.take_port(Port::new(face, node_name.clone(), node_id));

        let mut poly = Polynomial::new();

        for spec_mono in spec_poly {

            let mut mono = Monomial::new();

            for &conode_id in spec_mono {

                let conode_name = ctx.get_node_name(conode_id)
                    .ok_or(AcesError::NodeMissingForPort(!face))?
                    .to_owned();

                let coatom_id = ctx.atoms.take_port(Port::new(!face, conode_name.clone(), conode_id));

                let link_id = match face {
                    Face::Tx => ctx.atoms.take_link(
                        Link::new(
                            atom_id,
                            node_name.clone(),
                            node_id,
                            coatom_id,
                            conode_name,
                            conode_id,
                        )
                    ),
                    Face::Rx => ctx.atoms.take_link(
                        Link::new(
                            coatom_id,
                            conode_name,
                            conode_id,
                            atom_id,
                            node_name.clone(),
                            node_id,
                        )
                    ),
                };

                match face {
                    Face::Tx => {
                        if let Some(what_missing) = self.links.get_mut(&link_id) {
                            match *what_missing {
                                Some(Face::Tx) => {
                                    // Link occurs in causes and effects:
                                    // nothing misses, link not broken.
                                    *what_missing = None;
                                    self.num_broken_links -= 1;
                                }
                                _ => {
                                    // Link reoccurrence in effects.
                                }
                            }
                        } else {
                            // Link occurs in effects, but its occurence
                            // in causes is missing: link is broken.
                            self.links.insert(link_id, Some(Face::Rx));
                            self.num_broken_links += 1;
                        }
                    }

                    Face::Rx => {
                        if let Some(what_missing) = self.links.get_mut(&link_id) {
                            match *what_missing {
                                Some(Face::Rx) => {
                                    // Link occurs in causes and effects:
                                    // nothing misses, link not broken.
                                    *what_missing = None;
                                    self.num_broken_links -= 1;
                                }
                                _ => {
                                    // Link reoccurrence in causes.
                                }
                            }
                        } else {
                            // Link occurs in causes, but its occurence
                            // in effects is missing: link is broken.
                            self.links.insert(link_id, Some(Face::Tx));
                            self.num_broken_links += 1;
                        }
                    }
                }

                mono.insert(link_id);
            }
            poly.add_monomial(mono);
        }

        if face == Face::Tx {
            self.effects.insert(atom_id, poly);
        } else {
            self.causes.insert(atom_id, poly);
        }

        Ok(())
    }

    pub fn from_str(ctx: &Arc<Mutex<Context>>, raw_spec: &str) -> Result<Self, Box<dyn Error>> {
        let mut ces = CES::new(ctx);

        let spec = spec_from_str(ctx, raw_spec)?;

        for node_id in spec.get_carrier_ids() {
            if let Some(ref spec_poly) = spec.get_causes_by_id(node_id) {
                ces.add_polynomial(node_id, Face::Rx, spec_poly)?;
            }
            if let Some(ref spec_poly) = spec.get_effects_by_id(node_id) {
                ces.add_polynomial(node_id, Face::Tx, spec_poly)?;
            }
        }

        ces.spec = Some(spec);
        Ok(ces)
    }

    pub fn from_file<P: AsRef<Path>>(ctx: &Arc<Mutex<Context>>, path: P) -> Result<Self, Box<dyn Error>> {
        let mut fp = File::open(path)?;
        let mut raw_spec = String::new();
        fp.read_to_string(&mut raw_spec)?;

        Self::from_str(ctx, &raw_spec)
    }

    pub fn get_name(&self) -> Option<&str> {
        if let Some(ref spec) = self.spec {
            spec.get_name()
        } else {
            None
        }
    }

    pub fn is_coherent(&self) -> bool {
        self.num_broken_links == 0
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
