use std::{
    collections::{btree_map, BTreeMap},
    convert::TryInto,
    io::Read,
    fs::File,
    path::Path,
    error::Error,
};

use crate::{
    ContextHandle, Port, Fork, Join, NodeID, PortID, LinkID, ForkID, JoinID, Polynomial,
    FiringComponent, Content, content::content_from_str, node, sat, AcesError,
};

#[derive(PartialEq, Debug)]
enum LinkState {
    /// Single-face link (structure containing it is incoherent).  The
    /// [`node::Face`] value is the missing face.
    Thin(node::Face),
    Fat,
}

#[derive(Debug)]
enum Resolution {
    Unsolved,
    Incoherent,
    Deadlock,
    Solved(Vec<FiringComponent>),
}

impl Default for Resolution {
    fn default() -> Self {
        Resolution::Unsolved
    }
}

/// A single c-e structure.
///
/// Internally, instances of this type own structural information (the
/// cause and effect polynomials), semantic properties (node
/// capacities for the carrier), the intermediate content
/// representation from which a c-e structure originated (optionally),
/// and some auxiliary recomputable data.  Other properties are
/// available indirectly: `CEStructure` instance owns a
/// [`ContextHandle`] which resolves to a shared [`Context`] object.
///
/// [`Context`]: crate::Context
#[derive(Debug)]
pub struct CEStructure {
    context:        ContextHandle,
    content:        Option<Box<dyn Content>>,
    resolution:     Resolution,
    causes:         BTreeMap<PortID, Polynomial<LinkID>>,
    effects:        BTreeMap<PortID, Polynomial<LinkID>>,
    carrier:        BTreeMap<NodeID, node::Capacity>,
    links:          BTreeMap<LinkID, LinkState>,
    num_thin_links: u32,
    forks:          BTreeMap<NodeID, Vec<ForkID>>,
    joins:          BTreeMap<NodeID, Vec<JoinID>>,
    co_forks:       BTreeMap<JoinID, Vec<ForkID>>,
    co_joins:       BTreeMap<ForkID, Vec<JoinID>>,
}

impl CEStructure {
    /// Creates an empty c-e structure in a [`Context`] given by a
    /// [`ContextHandle`].
    ///
    /// [`Context`]: crate::Context
    pub fn new(ctx: &ContextHandle) -> Self {
        Self {
            context:        ctx.clone(),
            content:        Default::default(),
            resolution:     Default::default(),
            causes:         Default::default(),
            effects:        Default::default(),
            carrier:        Default::default(),
            links:          Default::default(),
            num_thin_links: 0,
            forks:          Default::default(),
            joins:          Default::default(),
            co_forks:       Default::default(),
            co_joins:       Default::default(),
        }
    }

    /// Constructs new [`Polynomial`] from a sequence of sequences of
    /// [`NodeID`]s and adds it to causes of a node of this
    /// `CEStructure`.
    ///
    /// This method is incremental: new polynomial is added to old
    /// polynomial that is already attached to the `node_id` as node's
    /// causes (there is always some polynomial attached, if not
    /// explicitly, then implicitly, as the default _&theta;_).
    pub fn add_causes<'a, I>(&mut self, node_id: NodeID, poly_ids: I) -> Result<(), AcesError>
    where
        I: IntoIterator + 'a,
        I::Item: IntoIterator<Item = &'a NodeID>,
    {
        let poly =
            Polynomial::from_nodes_in_context(&self.context, node::Face::Rx, node_id, poly_ids);

        let mut port = Port::new(node::Face::Rx, node_id);
        let port_id = self.context.lock().unwrap().share_port(&mut port);

        for &lid in poly.get_atomics() {
            if let Some(what_missing) = self.links.get_mut(&lid) {
                if *what_missing == LinkState::Thin(node::Face::Rx) {
                    // Fat link: occurs in causes and effects.
                    *what_missing = LinkState::Fat;
                    self.num_thin_links -= 1;
                } else {
                    // Link reoccurrence in causes.
                }
            } else {
                // Thin, cause-only link: occurs in causes, but not in effects.
                self.links.insert(lid, LinkState::Thin(node::Face::Tx));
                self.num_thin_links += 1;
            }
        }

        let mut ctx = self.context.lock().unwrap();

        for mono in poly.get_monomials() {
            let mut co_node_ids = Vec::new();

            for lid in mono {
                if let Some(link_state) = self.links.get(&lid) {
                    match link_state {
                        LinkState::Fat => {
                            if let Some(link) = ctx.get_link(lid) {
                                co_node_ids.push(link.get_tx_node_id());
                            } else {
                                return Err(AcesError::LinkMissingForID)
                            }
                        }
                        LinkState::Thin(_) => {} // Don't push a thin link.
                    }
                } else {
                    return Err(AcesError::UnlistedAtomicInMonomial)
                }
            }

            let mut co_forks = Vec::new();

            for nid in co_node_ids.iter() {
                if let Some(fork_ids) = self.forks.get(nid) {
                    for &fid in fork_ids {
                        if let Some(fork) = ctx.get_fork(fid) {
                            if fork.get_rx_node_ids().binary_search(nid).is_ok() {
                                co_forks.push(fid);
                            }
                        }
                    }
                } else {
                    // This co_node has no forks yet, a condition
                    // which should have been detected above as a thin
                    // link.
                    return Err(AcesError::IncoherencyLeak)
                }
            }

            let mut join = Join::new(co_node_ids, node_id, Default::default());
            let join_id = ctx.share_join(&mut join);

            match self.joins.entry(node_id) {
                btree_map::Entry::Vacant(entry) => {
                    entry.insert(vec![join_id]);
                }
                btree_map::Entry::Occupied(mut entry) => {
                    let jids = entry.get_mut();

                    if let Err(pos) = jids.binary_search(&join_id) {
                        jids.insert(pos, join_id);
                    } // else idempotency of addition.
                }
            }

            // For all co_forks update co_joins by adding this Join.
            for co_fork in co_forks.iter() {
                match self.co_joins.entry(*co_fork) {
                    btree_map::Entry::Vacant(entry) => {
                        entry.insert(vec![join_id]);
                    }
                    btree_map::Entry::Occupied(mut entry) => {
                        let jids = entry.get_mut();

                        if let Err(pos) = jids.binary_search(&join_id) {
                            jids.insert(pos, join_id);
                        } // else idempotency of addition.
                    }
                }
            }

            self.co_forks.insert(join_id, co_forks);
        }

        // FIXME add to old if any
        self.causes.insert(port_id, poly);

        self.carrier.entry(node_id).or_insert_with(Default::default);

        Ok(())
    }

    /// Constructs new [`Polynomial`] from a sequence of sequences of
    /// [`NodeID`]s and adds it to effects of a node of this
    /// `CEStructure`.
    ///
    /// This method is incremental: new polynomial is added to old
    /// polynomial that is already attached to the `node_id` as node's
    /// effects (there is always some polynomial attached, if not
    /// explicitly, then implicitly, as the default _&theta;_).
    pub fn add_effects<'a, I>(&mut self, node_id: NodeID, poly_ids: I) -> Result<(), AcesError>
    where
        I: IntoIterator + 'a,
        I::Item: IntoIterator<Item = &'a NodeID>,
    {
        let poly =
            Polynomial::from_nodes_in_context(&self.context, node::Face::Tx, node_id, poly_ids);

        let mut port = Port::new(node::Face::Tx, node_id);
        let port_id = self.context.lock().unwrap().share_port(&mut port);

        for &lid in poly.get_atomics() {
            if let Some(what_missing) = self.links.get_mut(&lid) {
                if *what_missing == LinkState::Thin(node::Face::Tx) {
                    // Fat link: occurs in causes and effects.
                    *what_missing = LinkState::Fat;
                    self.num_thin_links -= 1;
                } else {
                    // Link reoccurrence in effects.
                }
            } else {
                // Thin, effect-only link: occurs in effects, but not in causes.
                self.links.insert(lid, LinkState::Thin(node::Face::Rx));
                self.num_thin_links += 1;
            }
        }

        // FIXME add to old if any
        self.effects.insert(port_id, poly);

        self.carrier.entry(node_id).or_insert_with(Default::default);

        Ok(())
    }

    pub fn from_content(
        ctx: &ContextHandle,
        mut content: Box<dyn Content>,
    ) -> Result<Self, Box<dyn Error>> {
        let mut ces = CEStructure::new(ctx);

        for node_id in content.get_carrier_ids() {
            if let Some(poly_ids) = content.get_causes_by_id(node_id) {
                if poly_ids.is_empty() {
                    let node_name = ctx.lock().unwrap().get_node_name(node_id).unwrap().to_owned();

                    return Err(Box::new(AcesError::EmptyCausesOfInternalNode(node_name)))
                }

                ces.add_causes(node_id, poly_ids)?;
            }

            if let Some(poly_ids) = content.get_effects_by_id(node_id) {
                if poly_ids.is_empty() {
                    let node_name = ctx.lock().unwrap().get_node_name(node_id).unwrap().to_owned();

                    return Err(Box::new(AcesError::EmptyEffectsOfInternalNode(node_name)))
                }

                ces.add_effects(node_id, poly_ids)?;
            }
        }

        ces.content = Some(content);
        Ok(ces)
    }

    /// Creates a new c-e structure from a textual description and in
    /// a [`Context`] given by a [`ContextHandle`].
    ///
    /// [`Context`]: crate::Context
    pub fn from_str<S: AsRef<str>>(ctx: &ContextHandle, script: S) -> Result<Self, Box<dyn Error>> {
        let content = content_from_str(ctx, script)?;

        Self::from_content(ctx, content)
    }

    /// Creates a new c-e structure from a script file to be found
    /// along the `path` and in a [`Context`] given by a
    /// [`ContextHandle`].
    ///
    /// [`Context`]: crate::Context
    pub fn from_file<P: AsRef<Path>>(ctx: &ContextHandle, path: P) -> Result<Self, Box<dyn Error>> {
        let mut fp = File::open(path)?;
        let mut script = String::new();
        fp.read_to_string(&mut script)?;

        Self::from_str(ctx, &script)
    }

    pub fn get_context(&self) -> &ContextHandle {
        &self.context
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
    /// associated with each `CEStructure` object.  This counter is
    /// updated whenever a polynomial is added to the structure.
    pub fn is_coherent(&self) -> bool {
        self.num_thin_links == 0
    }

    pub fn get_port_link_formula(&self) -> sat::Formula {
        let mut formula = sat::Formula::new(&self.context);

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

    pub fn get_fork_join_formula(&self) -> sat::Formula {
        let mut formula = sat::Formula::new(&self.context);

        // FIXME

        // For each fork collect singularity constraints (anti-join
        // and side-fork clauses), and for each tine of that fork
        // collect all bijection constraints (cojoin clauses).

        // For each join collect singularity constraints (anti-fork
        // and side-join clauses), and for each lane of that join
        // collect all bijection constraints (cofork clauses).

        formula
    }

    pub fn solve(&mut self, minimal_mode: bool) -> Result<(), Box<dyn Error>> {
        if !self.is_coherent() {
            self.resolution = Resolution::Incoherent;

            Err(Box::new(AcesError::IncoherentStructure(
                self.get_name().unwrap_or("anonymous").to_owned(),
            )))
        } else {
            let formula = self.get_port_link_formula();

            debug!("Raw {:?}", formula);
            info!("Formula: {}", formula);

            let mut solver = sat::Solver::new(&self.context);
            solver.add_formula(&formula);
            solver.inhibit_empty_solution();

            info!("Start of {}-solution search", if minimal_mode { "min" } else { "all" });
            solver.set_minimal_mode(minimal_mode);

            if let Some(first_solution) = solver.next() {
                let mut fcs = Vec::new();

                debug!("1. Raw {:?}", first_solution);
                fcs.push(first_solution.try_into()?);

                for (count, solution) in solver.enumerate() {
                    debug!("{}. Raw {:?}", count + 2, solution);
                    fcs.push(solution.try_into()?);
                }

                self.resolution = Resolution::Solved(fcs);
            } else if solver.is_sat().is_some() {
                info!("\nStructural deadlock (found no solutions).");
                self.resolution = Resolution::Deadlock;
            } else if solver.was_interrupted() {
                warn!("Solving was interrupted");
                self.resolution = Resolution::Unsolved;
            } else if let Some(Err(err)) = solver.take_last_result() {
                error!("Solving failed: {}", err);
                self.resolution = Resolution::Unsolved;
            } else {
                unreachable!()
            }

            Ok(())
        }
    }

    pub fn get_firing_components(&self) -> Option<&[FiringComponent]> {
        if let Resolution::Solved(ref fcs) = self.resolution {
            Some(fcs.as_slice())
        } else {
            None
        }
    }
}
