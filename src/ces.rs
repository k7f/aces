use std::{
    collections::BTreeMap,
    io::Read,
    fs::File,
    path::Path,
    sync::{Arc, Mutex},
    error::Error,
};

use crate::{
    Context, ID, Port, Face, Link, NodeID, PortID, LinkID, Monomial, Polynomial,
    spec::{CESSpec, spec_from_str},
    sat,
};

#[derive(Debug)]
pub struct CES {
    context:          Arc<Mutex<Context>>,
    spec:             Option<Box<dyn CESSpec>>,
    causes:           BTreeMap<PortID, Polynomial>,
    effects:          BTreeMap<PortID, Polynomial>,
    links:            BTreeMap<LinkID, Option<Face>>,
    num_broken_links: u32,
}

impl CES {
    fn new(ctx: &Arc<Mutex<Context>>) -> Self {
        Self {
            context:          Arc::clone(ctx),
            spec:             Default::default(),
            causes:           Default::default(),
            effects:          Default::default(),
            links:            Default::default(),
            num_broken_links: 0,
        }
    }

    fn add_polynomial(
        &mut self,
        node_id: NodeID,
        face: Face,
        spec_poly: &[Vec<ID>],
    ) -> Result<(), Box<dyn Error>> {
        let mut ctx = self.context.lock().unwrap();
        let atom_id = ctx.take_port(Port::new(face, node_id));

        let mut poly = Polynomial::new();

        for spec_mono in spec_poly {
            let mut mono = Monomial::new();

            for conode_id in spec_mono {
                let conode_id = NodeID(*conode_id);
                let coatom_id = ctx.take_port(Port::new(!face, conode_id));

                let link_id = match face {
                    Face::Tx => ctx.take_link(Link::new(atom_id, node_id, coatom_id, conode_id)),
                    Face::Rx => ctx.take_link(Link::new(coatom_id, conode_id, atom_id, node_id)),
                };

                match face {
                    Face::Tx => {
                        if let Some(what_missing) = self.links.get_mut(&link_id) {
                            if *what_missing == Some(Face::Tx) {
                                // Link occurs in causes and effects:
                                // nothing misses, link not broken.
                                *what_missing = None;
                                self.num_broken_links -= 1;
                            } else {
                                // Link reoccurrence in effects.
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
                            if *what_missing == Some(Face::Rx) {
                                // Link occurs in causes and effects:
                                // nothing misses, link not broken.
                                *what_missing = None;
                                self.num_broken_links -= 1;
                            } else {
                                // Link reoccurrence in causes.
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

    pub fn from_str<S: AsRef<str>>(
        ctx: &Arc<Mutex<Context>>,
        raw_spec: S,
    ) -> Result<Self, Box<dyn Error>> {
        let mut ces = CES::new(ctx);

        let spec = spec_from_str(ctx, raw_spec)?;

        for id in spec.get_carrier_ids() {
            let node_id = NodeID(id);

            if let Some(ref spec_poly) = spec.get_causes_by_id(id) {
                ces.add_polynomial(node_id, Face::Rx, spec_poly)?;
            }
            if let Some(ref spec_poly) = spec.get_effects_by_id(id) {
                ces.add_polynomial(node_id, Face::Tx, spec_poly)?;
            }
        }

        ces.spec = Some(spec);
        Ok(ces)
    }

    pub fn from_file<P: AsRef<Path>>(
        ctx: &Arc<Mutex<Context>>,
        path: P,
    ) -> Result<Self, Box<dyn Error>> {
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

    pub fn get_formula(&self) -> sat::Formula {
        let mut formula = sat::Formula::new(&self.context);

        for (&port_id, poly) in self.causes.iter() {
            let port_lit = sat::Literal::from_atom_id(port_id.into(), true);

            formula.add_polynomial(port_lit, poly);

            let ctx = self.context.lock().unwrap();

            if let Some(antiport_id) = ctx.get_antiport_id(port_id) {
                let antiport_lit = sat::Literal::from_atom_id(antiport_id.into(), true);

                formula.add_internal_node(port_lit, antiport_lit);
            }
        }

        for (&port_id, poly) in self.effects.iter() {
            let port_lit = sat::Literal::from_atom_id(port_id.into(), true);

            formula.add_polynomial(port_lit, poly);
        }

        formula
    }
}
