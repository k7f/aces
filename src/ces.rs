use std::{collections::BTreeMap, io::Read, fs::File, path::Path, error::Error};

use crate::{
    ContextHandle, Port, NodeID, PortID, LinkID, Polynomial,
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
    /// Creates an empty c-e structure within a [`Context`] given by a
    /// [`ContextHandle`].
    ///
    /// [`Context`]: crate::Context
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

    fn add_effect_polynomial(&mut self, port_id: PortID, poly: Polynomial) {
        for &link_id in poly.get_links() {
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

        // FIXME add to old if any
        self.effects.insert(port_id, poly);
    }

    fn add_cause_polynomial(&mut self, port_id: PortID, poly: Polynomial) {
        for &link_id in poly.get_links() {
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

        // FIXME add to old if any
        self.causes.insert(port_id, poly);
    }

    /// Adds a [`Polynomial`] to this `CES` at a given `face` (cause
    /// or effect) of a node.
    ///
    /// The polynomial `poly` is added to another polynomial which is
    /// already, explicitly or implicitly (as the default _&theta;_),
    /// attached to a `node_id` at a given `face`.
    pub fn add_polynomial(&mut self, node_id: NodeID, face: node::Face, poly: Polynomial) {
        let mut port = Port::new(face, node_id);
        let port_id = self.context.lock().unwrap().share_port(&mut port);

        match face {
            node::Face::Rx => self.add_cause_polynomial(port_id, poly),
            node::Face::Tx => self.add_effect_polynomial(port_id, poly),
        }

        self.carrier.entry(node_id).or_insert_with(Default::default);
    }

    /// Creates a new c-e structure from a specification string and within
    /// a [`Context`] given by a [`ContextHandle`].
    ///
    /// [`Context`]: crate::Context
    pub fn from_str<S: AsRef<str>>(
        ctx: ContextHandle,
        raw_spec: S,
    ) -> Result<Self, Box<dyn Error>> {
        let spec = spec_from_str(&ctx, raw_spec)?;

        let mut ces = CES::new(ctx);

        for id in spec.get_carrier_ids() {
            let node_id = NodeID(id);

            if let Some(ref spec_poly) = spec.get_causes_by_id(id) {
                let mut port = Port::new(node::Face::Rx, node_id);
                let port_id = ces.context.lock().unwrap().share_port(&mut port);

                let poly = Polynomial::from_spec(ces.context.clone(), &port, spec_poly);

                ces.add_cause_polynomial(port_id, poly);
                ces.carrier.entry(node_id).or_insert_with(Default::default);
            }

            if let Some(ref spec_poly) = spec.get_effects_by_id(id) {
                let mut port = Port::new(node::Face::Tx, node_id);
                let port_id = ces.context.lock().unwrap().share_port(&mut port);

                let poly = Polynomial::from_spec(ces.context.clone(), &port, spec_poly);

                ces.add_effect_polynomial(port_id, poly);
                ces.carrier.entry(node_id).or_insert_with(Default::default);
            }
        }

        ces.spec = Some(spec);
        Ok(ces)
    }

    /// Creates a new c-e structure from a specification file to be
    /// found along the `path` and within a [`Context`] given by a
    /// [`ContextHandle`].
    ///
    /// [`Context`]: crate::Context
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
