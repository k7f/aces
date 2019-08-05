use std::{collections::BTreeMap, io::Read, fs::File, path::Path, error::Error};

use crate::{
    ContextHandle, Port, NodeID, PortID, LinkID, Polynomial, Content, content::content_from_str,
    node, sat,
};

// None: fat link; Some(face): thin, face-only link
type LinkState = Option<node::Face>;

/// A single c-e structure.
///
/// Internally, instances of this type own structural information (the
/// cause and effect polynomials), semantic properties (node
/// capacities for the carrier), the intermediate content
/// representation from which a c-e structure originated (optionally),
/// and some auxiliary recomputable data.  Other properties are
/// available indirectly: `CES` instance owns a [`ContextHandle`]
/// which resolves to a shared [`Context`] object.
///
/// [`Context`]: crate::Context
#[derive(Debug)]
pub struct CES {
    context:        ContextHandle,
    content:        Option<Box<dyn Content>>,
    causes:         BTreeMap<PortID, Polynomial<LinkID>>,
    effects:        BTreeMap<PortID, Polynomial<LinkID>>,
    links:          BTreeMap<LinkID, LinkState>,
    carrier:        BTreeMap<NodeID, node::Capacity>,
    num_thin_links: u32,
}

impl CES {
    /// Creates an empty c-e structure in a [`Context`] given by a
    /// [`ContextHandle`].
    ///
    /// [`Context`]: crate::Context
    pub fn new(ctx: ContextHandle) -> Self {
        Self {
            context:        ctx,
            content:        Default::default(),
            causes:         Default::default(),
            effects:        Default::default(),
            links:          Default::default(),
            carrier:        Default::default(),
            num_thin_links: 0,
        }
    }

    fn add_cause_polynomial(&mut self, port_id: PortID, poly: Polynomial<LinkID>) {
        for &lid in poly.get_atomics() {
            if let Some(what_missing) = self.links.get_mut(&lid) {
                if *what_missing == Some(node::Face::Rx) {
                    // Fat link: occurs in causes and effects.
                    *what_missing = None;
                    self.num_thin_links -= 1;
                } else {
                    // Link reoccurrence in causes.
                }
            } else {
                // Thin, cause-only link: occurs in causes, but not in effects.
                self.links.insert(lid, Some(node::Face::Tx));
                self.num_thin_links += 1;
            }
        }

        // FIXME add to old if any
        self.causes.insert(port_id, poly);
    }

    fn add_effect_polynomial(&mut self, port_id: PortID, poly: Polynomial<LinkID>) {
        for &lid in poly.get_atomics() {
            if let Some(what_missing) = self.links.get_mut(&lid) {
                if *what_missing == Some(node::Face::Tx) {
                    // Fat link: occurs in causes and effects.
                    *what_missing = None;
                    self.num_thin_links -= 1;
                } else {
                    // Link reoccurrence in effects.
                }
            } else {
                // Thin, effect-only link: occurs in effects, but not in causes.
                self.links.insert(lid, Some(node::Face::Rx));
                self.num_thin_links += 1;
            }
        }

        // FIXME add to old if any
        self.effects.insert(port_id, poly);
    }

    /// Adds a [`Polynomial`] to this `CES` at a given `face` (cause
    /// or effect) of a node.
    ///
    /// The polynomial `poly` is added to another polynomial which is
    /// already, explicitly or implicitly (as the default _&theta;_),
    /// attached to a `node_id` at a given `face`.
    pub fn add_polynomial(&mut self, node_id: NodeID, face: node::Face, poly: Polynomial<LinkID>) {
        let mut port = Port::new(face, node_id);
        let pid = self.context.lock().unwrap().share_port(&mut port);

        match face {
            node::Face::Rx => self.add_cause_polynomial(pid, poly),
            node::Face::Tx => self.add_effect_polynomial(pid, poly),
        }

        self.carrier.entry(node_id).or_insert_with(Default::default);
    }

    /// Creates a new c-e structure from a textual description and in
    /// a [`Context`] given by a [`ContextHandle`].
    ///
    /// [`Context`]: crate::Context
    pub fn from_str<S: AsRef<str>>(ctx: ContextHandle, script: S) -> Result<Self, Box<dyn Error>> {
        let content = content_from_str(&ctx, script)?;

        let mut ces = CES::new(ctx);

        for id in content.get_carrier_ids() {
            let node_id = NodeID(id);

            if let Some(ref poly_ids) = content.get_causes_by_id(id) {
                let mut port = Port::new(node::Face::Rx, node_id);
                let pid = ces.context.lock().unwrap().share_port(&mut port);

                let poly = Polynomial::from_port_and_ids(ces.context.clone(), &port, poly_ids);

                ces.add_cause_polynomial(pid, poly);
                ces.carrier.entry(node_id).or_insert_with(Default::default);
            }

            if let Some(ref poly_ids) = content.get_effects_by_id(id) {
                let mut port = Port::new(node::Face::Tx, node_id);
                let pid = ces.context.lock().unwrap().share_port(&mut port);

                let poly = Polynomial::from_port_and_ids(ces.context.clone(), &port, poly_ids);

                ces.add_effect_polynomial(pid, poly);
                ces.carrier.entry(node_id).or_insert_with(Default::default);
            }
        }

        ces.content = Some(content);
        Ok(ces)
    }

    /// Creates a new c-e structure from a script file to be found
    /// along the `path` and in a [`Context`] given by a
    /// [`ContextHandle`].
    ///
    /// [`Context`]: crate::Context
    pub fn from_file<P: AsRef<Path>>(ctx: ContextHandle, path: P) -> Result<Self, Box<dyn Error>> {
        let mut fp = File::open(path)?;
        let mut script = String::new();
        fp.read_to_string(&mut script)?;

        Self::from_str(ctx, &script)
    }

    pub fn get_name(&self) -> Option<&str> {
        if let Some(ref content) = self.content {
            content.get_name()
        } else {
            None
        }
    }

    /// Returns link coherence status indicating whether this object
    /// represents a proper c-e structure.
    ///
    /// C-e structure is coherent iff it has no thin links, where a
    /// link is thin iff it occurs either in causes or in effects, but
    /// not in both.  Internally, there is a thin links counter
    /// associated with each `CES` object.  This counter is updated
    /// whenever a polynomial is added to the structure.
    pub fn is_coherent(&self) -> bool {
        self.num_thin_links == 0
    }

    pub fn get_formula(&self) -> sat::Formula {
        let mut formula = sat::Formula::new(self.context.clone());

        for (&pid, poly) in self.causes.iter() {
            formula.add_polynomial(pid, poly);
            formula.add_antiport(pid);
        }

        for (&pid, poly) in self.effects.iter() {
            formula.add_polynomial(pid, poly);
        }

        for (&lid, _) in self.links.iter() {
            formula.add_link_coherence(lid);
        }

        formula
    }
}
