use std::{collections::BTreeMap, io::Read, fs::File, path::Path, error::Error};

use crate::{
    ContextHandle, ID, Port, Face, Link, NodeID, PortID, LinkID, Polynomial,
    spec::{CESSpec, spec_from_str},
    node, sat,
};

/// A single c-e structure.
///
/// Internally, instances of this type own structural information (the
/// cause and effect polynomials), semantic properties (node
/// capacities for the carrier), the specification from which a c-e
/// structure originated (optionally), and some auxiliary recomputable
/// data.  Other properties are available indirectly: `CES` instance
/// owns a [`ContextHandle`] which resolves to a shared [`Context`]
/// object.
///
/// [`Context`]: crate::Context
#[derive(Debug)]
pub struct CES {
    context:          ContextHandle,
    spec:             Option<Box<dyn CESSpec>>,
    causes:           BTreeMap<PortID, Polynomial>,
    effects:          BTreeMap<PortID, Polynomial>,
    links:            BTreeMap<LinkID, Option<Face>>,
    carrier:          BTreeMap<NodeID, node::Capacity>,
    num_broken_links: u32,
}

impl CES {
    /// Creates an empty c-e structure from a given [`ContextHandle`].
    pub fn new(ctx: ContextHandle) -> Self {
        Self {
            context:          ctx,
            spec:             Default::default(),
            causes:           Default::default(),
            effects:          Default::default(),
            links:            Default::default(),
            carrier:          Default::default(),
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

        let mut mono = Polynomial::new();
        let mut poly = Polynomial::new();

        for spec_mono in spec_poly {
            mono.clear();

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

                mono.multiply_by_link(link_id);
            }
            poly.add_polynomial(&mono);
        }

        if face == Face::Tx {
            self.effects.insert(atom_id, poly);
        } else {
            self.causes.insert(atom_id, poly);
        }

        if !self.carrier.contains_key(&node_id) {
            self.carrier.insert(node_id, Default::default());
        }

        Ok(())
    }

    /// Creates a new c-e structure from a given [`ContextHandle`] and
    /// a specification string.
    pub fn from_str<S: AsRef<str>>(
        ctx: ContextHandle,
        raw_spec: S,
    ) -> Result<Self, Box<dyn Error>> {
        let spec = spec_from_str(&ctx, raw_spec)?;

        let mut ces = CES::new(ctx);

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

    /// Creates a new c-e structure from a given [`ContextHandle`] and
    /// a specification file to be found along the `path`.
    pub fn from_file<P: AsRef<Path>>(ctx: ContextHandle, path: P) -> Result<Self, Box<dyn Error>> {
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

    /// Returns link coherence status indicating whether this object
    /// represents a proper c-e structure.
    ///
    /// C-e structure is coherent iff it has no broken links, where a
    /// link is broken iff it occurs either in causes or in effects,
    /// but not in both.  Internally, there is a broken links counter
    /// associated with each `CES` object.  This counter is updated
    /// whenever a polynomial is added to the structure.
    pub fn is_coherent(&self) -> bool {
        self.num_broken_links == 0
    }

    pub fn get_formula(&self) -> sat::Formula {
        let mut formula = sat::Formula::new(self.context.clone());

        for (&port_id, poly) in self.causes.iter() {
            formula.add_polynomial(port_id, poly);
            formula.add_antiport(port_id);
        }

        for (&port_id, poly) in self.effects.iter() {
            formula.add_polynomial(port_id, poly);
        }

        for (&link_id, _) in self.links.iter() {
            formula.add_link_coherence(link_id);
        }

        formula
    }
}
