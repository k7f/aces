use std::{collections::BTreeMap, io::Read, fs::File, path::Path, error::Error};

use crate::{
    ContextHandle, ID, Port, NodeID, PortID, LinkID, Polynomial,
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
    links:            BTreeMap<LinkID, Option<node::Face>>,
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

    // pub fn add_polynomial(
    //     &mut self,
    //     node_id: NodeID,
    //     face: node::Face,
    //     poly: Polynomial,
    // ) -> Result<(), Box<dyn Error>> {
    //     Ok(())
    // }

    fn register_effect_link(&mut self, link_id: LinkID) {
        if let Some(what_missing) = self.links.get_mut(&link_id) {
            if *what_missing == Some(node::Face::Tx) {
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
            self.links.insert(link_id, Some(node::Face::Rx));
            self.num_broken_links += 1;
        }
    }

    fn register_cause_link(&mut self, link_id: LinkID) {
        if let Some(what_missing) = self.links.get_mut(&link_id) {
            if *what_missing == Some(node::Face::Rx) {
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
            self.links.insert(link_id, Some(node::Face::Tx));
            self.num_broken_links += 1;
        }
    }

    fn add_polynomial_from_spec(
        &mut self,
        node_id: NodeID,
        face: node::Face,
        spec_poly: &[Vec<ID>],
    ) -> Result<(), Box<dyn Error>> {
        let mut port = Port::new(face, node_id);
        let port_id = self.context.lock().unwrap().share_port(&mut port);

        let poly = Polynomial::from_spec(self.context.clone(), &port, spec_poly);

        for link_id in poly.get_links() {
            match face {
                node::Face::Tx => self.register_effect_link(*link_id),
                node::Face::Rx => self.register_cause_link(*link_id),
            }
        }

        if face == node::Face::Tx {
            self.effects.insert(port_id, poly);
        } else {
            self.causes.insert(port_id, poly);
        }

        self.carrier.entry(node_id).or_insert_with(Default::default);

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
                ces.add_polynomial_from_spec(node_id, node::Face::Rx, spec_poly)?;
            }
            if let Some(ref spec_poly) = spec.get_effects_by_id(id) {
                ces.add_polynomial_from_spec(node_id, node::Face::Tx, spec_poly)?;
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
