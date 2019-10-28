use std::{
    collections::{btree_map, BTreeMap},
    convert::TryInto,
    io::Read,
    fs::File,
    path::Path,
    error::Error,
};
use log::Level::Trace;
use crate::{
    ContextHandle, Port, Split, AtomID, NodeID, PortID, LinkID, ForkID, JoinID, Polynomial,
    FiringSet, Content, content::content_from_str, node, sat, sat::Resolution, Solver, AcesError,
};

#[derive(PartialEq, Debug)]
enum LinkState {
    /// Single-face link (structure containing it is incoherent).  The
    /// [`node::Face`] value is the missing face.
    Thin(node::Face),
    Fat,
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
    content:        Vec<Box<dyn Content>>,
    resolution:     Resolution,
    causes:         BTreeMap<PortID, Polynomial<LinkID>>,
    effects:        BTreeMap<PortID, Polynomial<LinkID>>,
    carrier:        BTreeMap<NodeID, node::Capacity>,
    links:          BTreeMap<LinkID, LinkState>,
    num_thin_links: u32,
    forks:          BTreeMap<NodeID, Vec<AtomID>>,
    joins:          BTreeMap<NodeID, Vec<AtomID>>,
    // FIXME define `struct Cosplits`, grouped on demand (cf. `group_cosplits`)
    co_forks: BTreeMap<AtomID, Vec<AtomID>>, // Joins -> 2^Forks
    co_joins: BTreeMap<AtomID, Vec<AtomID>>, // Forks -> 2^Joins
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

    fn add_split_to_host(&mut self, split_id: AtomID, face: node::Face, node_id: NodeID) {
        let split_entry = match face {
            node::Face::Tx => self.forks.entry(node_id),
            node::Face::Rx => self.joins.entry(node_id),
        };

        match split_entry {
            btree_map::Entry::Vacant(entry) => {
                entry.insert(vec![split_id]);
            }
            btree_map::Entry::Occupied(mut entry) => {
                let sids = entry.get_mut();

                if let Err(pos) = sids.binary_search(&split_id) {
                    sids.insert(pos, split_id);
                } // else idempotency of addition.
            }
        }
    }

    fn add_split_to_suit(&mut self, split_id: AtomID, face: node::Face, co_split_ids: &[AtomID]) {
        for &co_split_id in co_split_ids.iter() {
            let split_entry = match face {
                node::Face::Tx => self.co_forks.entry(co_split_id),
                node::Face::Rx => self.co_joins.entry(co_split_id),
            };

            if log_enabled!(Trace) {
                let ctx = self.context.lock().unwrap();

                match face {
                    node::Face::Tx => {
                        trace!(
                            "Old join's co_forks[{}] -> {}",
                            ctx.with(&JoinID(co_split_id)),
                            ctx.with(&ForkID(split_id))
                        );
                    }
                    node::Face::Rx => {
                        trace!(
                            "Old fork's co_joins[{}] -> {}",
                            ctx.with(&ForkID(co_split_id)),
                            ctx.with(&JoinID(split_id))
                        );
                    }
                }
            }

            match split_entry {
                btree_map::Entry::Vacant(entry) => {
                    entry.insert(vec![split_id]);
                }
                btree_map::Entry::Occupied(mut entry) => {
                    let sids = entry.get_mut();

                    if let Err(pos) = sids.binary_search(&split_id) {
                        sids.insert(pos, split_id);
                    } // else idempotency of addition.
                }
            }
        }
    }

    fn create_splits(
        &mut self,
        face: node::Face,
        node_id: NodeID,
        poly: &Polynomial<LinkID>,
    ) -> Result<(), AcesError> {
        use node::Face::{Tx, Rx};

        for mono in poly.get_monomials() {
            let mut co_node_ids = Vec::new();
            let mut fat_co_node_ids = Vec::new();

            // Link sequence `mono` is ordered by link ID, reorder
            // first by conode ID.
            let mut co_node_map = BTreeMap::new();

            for lid in mono {
                if let Some(link_state) = self.links.get(&lid) {
                    let ctx = self.context.lock().unwrap();

                    if let Some(link) = ctx.get_link(lid) {
                        co_node_map.insert(link.get_node_id(!face), link_state);
                    } else {
                        return Err(AcesError::LinkMissingForID(lid))
                    }
                } else {
                    return Err(AcesError::UnlistedAtomicInMonomial)
                }
            }

            for (co_node_id, link_state) in co_node_map {
                co_node_ids.push(co_node_id);

                match link_state {
                    LinkState::Fat => {
                        fat_co_node_ids.push(co_node_id);
                    }
                    LinkState::Thin(_) => {} // Don't push a thin link.
                }
            }

            let mut co_splits = Vec::new();

            for nid in fat_co_node_ids.iter() {
                if let Some(split_ids) = match face {
                    Tx => self.joins.get(nid),
                    Rx => self.forks.get(nid),
                } {
                    let ctx = self.context.lock().unwrap();

                    for &sid in split_ids {
                        if let Some(split) = match face {
                            Tx => ctx.get_join(sid.into()),
                            Rx => ctx.get_fork(sid.into()),
                        } {
                            if split.get_suit_ids().binary_search(&node_id).is_ok() {
                                co_splits.push(sid);
                            }
                        }
                    }
                } else {
                    // This co_node has no co_splits yet, a condition
                    // which should have been detected above as a thin
                    // link.
                    return Err(AcesError::IncoherencyLeak)
                }
            }

            let split_id: AtomID = match face {
                Tx => {
                    let mut fork = Split::new_fork(node_id, co_node_ids, Default::default());
                    self.context.lock().unwrap().share_fork(&mut fork).into()
                }
                Rx => {
                    let mut join = Split::new_join(node_id, co_node_ids, Default::default());
                    self.context.lock().unwrap().share_join(&mut join).into()
                }
            };

            self.add_split_to_host(split_id, face, node_id);

            if !co_splits.is_empty() {
                self.add_split_to_suit(split_id, face, co_splits.as_slice());

                if log_enabled!(Trace) {
                    let ctx = self.context.lock().unwrap();

                    match face {
                        Tx => {
                            trace!(
                                "New fork's co_joins[{}] -> {:?}",
                                ctx.with(&ForkID(split_id)),
                                co_splits
                            );
                        }
                        Rx => {
                            trace!(
                                "New join's co_forks[{}] -> {:?}",
                                ctx.with(&JoinID(split_id)),
                                co_splits
                            );
                        }
                    }
                }

                match face {
                    Tx => self.co_joins.insert(split_id, co_splits),
                    Rx => self.co_forks.insert(split_id, co_splits),
                };
            }
        }

        Ok(())
    }

    /// Constructs new [`Polynomial`] from a sequence of sequences of
    /// [`NodeID`]s and adds it to causes or effects of a node of this
    /// `CEStructure`.
    ///
    /// The only direct callers of this are methods [`add_causes()`]
    /// and [`add_effects()`].
    ///
    /// [`add_causes()`]: CEStructure::add_causes()
    /// [`add_effects()`]: CEStructure::add_effects()
    fn add_causes_or_effects<'a, I>(
        &mut self,
        face: node::Face,
        node_id: NodeID,
        poly_ids: I,
    ) -> Result<(), AcesError>
    where
        I: IntoIterator + 'a,
        I::Item: IntoIterator<Item = &'a NodeID>,
    {
        let poly = Polynomial::from_nodes_in_context(&self.context, face, node_id, poly_ids);

        let mut port = Port::new(face, node_id);
        let port_id = self.context.lock().unwrap().share_port(&mut port);

        for &lid in poly.get_atomics() {
            if let Some(what_missing) = self.links.get_mut(&lid) {
                if *what_missing == LinkState::Thin(face) {
                    // Fat link: occurs in causes and effects.
                    *what_missing = LinkState::Fat;
                    self.num_thin_links -= 1;
                } else {
                    // Rx: Link reoccurrence in causes.
                    // Tx: Link reoccurrence in effects.
                }
            } else {
                // Rx: Thin, cause-only link: occurs in causes, but not in effects.
                // Tx: Thin, effect-only link: occurs in effects, but not in causes.
                self.links.insert(lid, LinkState::Thin(!face));
                self.num_thin_links += 1;
            }
        }

        self.create_splits(face, node_id, &poly)?;

        let poly_entry = match face {
            node::Face::Rx => self.causes.entry(port_id),
            node::Face::Tx => self.effects.entry(port_id),
        };

        match poly_entry {
            btree_map::Entry::Vacant(entry) => {
                entry.insert(poly);
            }
            btree_map::Entry::Occupied(mut entry) => {
                entry.get_mut().add_polynomial(&poly)?;
            }
        }

        self.carrier.entry(node_id).or_insert_with(Default::default);

        Ok(())
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
        self.add_causes_or_effects(node::Face::Rx, node_id, poly_ids)
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
        self.add_causes_or_effects(node::Face::Tx, node_id, poly_ids)
    }

    /// Extends this c-e structure with another one, which is created
    /// in the [`Context`] of the old c-e structure from a given
    /// [`Content`] trait object.
    ///
    /// [`Context`]: crate::Context
    pub fn with_content(mut self, mut content: Box<dyn Content>) -> Result<Self, Box<dyn Error>> {
        for node_id in content.get_carrier_ids() {
            if let Some(poly_ids) = content.get_causes_by_id(node_id) {
                if poly_ids.is_empty() {
                    let node_name =
                        self.context.lock().unwrap().get_node_name(node_id).unwrap().to_owned();

                    return Err(Box::new(AcesError::EmptyCausesOfInternalNode(node_name)))
                }

                self.add_causes(node_id, poly_ids)?;
            }

            if let Some(poly_ids) = content.get_effects_by_id(node_id) {
                if poly_ids.is_empty() {
                    let node_name =
                        self.context.lock().unwrap().get_node_name(node_id).unwrap().to_owned();

                    return Err(Box::new(AcesError::EmptyEffectsOfInternalNode(node_name)))
                }

                self.add_effects(node_id, poly_ids)?;
            }
        }

        self.content.push(content);

        Ok(self)
    }

    /// Extends this c-e structure with another one, which is created
    /// in the [`Context`] of the old c-e structure from a given
    /// textual description.
    ///
    /// [`Context`]: crate::Context
    pub fn with_str<S: AsRef<str>>(self, script: S) -> Result<Self, Box<dyn Error>> {
        let content = content_from_str(&self.context, script)?;

        self.with_content(content)
    }

    /// Creates a new c-e structure from a textual description, in a
    /// [`Context`] given by a [`ContextHandle`].
    ///
    /// [`Context`]: crate::Context
    pub fn from_str<S: AsRef<str>>(ctx: &ContextHandle, script: S) -> Result<Self, Box<dyn Error>> {
        Self::new(ctx).with_str(script)
    }

    /// Extends this c-e structure with another one, which is created
    /// in the [`Context`] of the old c-e structure from a script file
    /// to be found along the `path`.
    ///
    /// [`Context`]: crate::Context
    pub fn with_file<P: AsRef<Path>>(self, path: P) -> Result<Self, Box<dyn Error>> {
        let mut fp = File::open(path)?;
        let mut script = String::new();
        fp.read_to_string(&mut script)?;

        self.with_str(&script)
    }

    /// Creates a new c-e structure from a script file to be found
    /// along the `path`, in a [`Context`] given by a
    /// [`ContextHandle`].
    ///
    /// [`Context`]: crate::Context
    pub fn from_file<P: AsRef<Path>>(ctx: &ContextHandle, path: P) -> Result<Self, Box<dyn Error>> {
        Self::new(ctx).with_file(path)
    }

    pub fn get_context(&self) -> &ContextHandle {
        &self.context
    }

    pub fn get_name(&self) -> Option<&str> {
        if let Some(content) = self.content.first() {
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

    pub fn get_port_link_formula(&self) -> Result<sat::Formula, Box<dyn Error>> {
        let mut formula = sat::Formula::new(&self.context);

        for (&pid, poly) in self.causes.iter() {
            formula.add_polynomial(pid, poly)?;
            formula.add_antiport(pid)?;
        }

        for (&pid, poly) in self.effects.iter() {
            formula.add_polynomial(pid, poly)?;
        }

        for (&lid, _) in self.links.iter() {
            formula.add_link_coherence(lid)?;
        }

        Ok(formula)
    }

    /// Given a flat list of co-splits, groups them by their host
    /// nodes (i.e. by suit members of the considered split), and
    /// returns a vector of vectors of [`AtomID`]s.
    ///
    /// The result, if interpreted in terms of SAT encoding, is a
    /// conjunction of exclusive choices of co-splits.
    fn group_cosplits(&self, cosplit_ids: &[AtomID]) -> Result<Vec<Vec<AtomID>>, AcesError> {
        if cosplit_ids.len() < 2 {
            if cosplit_ids.is_empty() {
                Err(AcesError::IncoherencyLeak)
            } else {
                Ok(vec![cosplit_ids.to_vec()])
            }
        } else {
            let ctx = self.context.lock().unwrap();
            let mut cohost_map: BTreeMap<NodeID, Vec<AtomID>> = BTreeMap::new();

            for &cosplit_id in cosplit_ids.iter() {
                let cosplit = ctx.get_split(cosplit_id).ok_or(AcesError::SplitMissingForID)?;
                let cohost_id = cosplit.get_host_id();

                match cohost_map.entry(cohost_id) {
                    btree_map::Entry::Vacant(entry) => {
                        entry.insert(vec![cosplit_id]);
                    }
                    btree_map::Entry::Occupied(mut entry) => {
                        let sids = entry.get_mut();

                        if let Err(pos) = sids.binary_search(&cosplit_id) {
                            sids.insert(pos, cosplit_id);
                        } else {
                            warn!("Multiple occurrences of {} in cosplit array", ctx.with(cosplit));
                        }
                    }
                }
            }

            Ok(cohost_map.into_iter().map(|(_, v)| v).collect())
        }
    }

    pub fn get_fork_join_formula(&self) -> Result<sat::Formula, Box<dyn Error>> {
        let mut formula = sat::Formula::new(&self.context);

        for (node_id, fork_atom_ids) in self.forks.iter() {
            if let Some(join_atom_ids) = self.joins.get(node_id) {
                formula.add_antisplits(fork_atom_ids.as_slice(), join_atom_ids.as_slice())?;
            }

            formula.add_sidesplits(fork_atom_ids.as_slice())?;
        }

        for (_, join_atom_ids) in self.joins.iter() {
            formula.add_sidesplits(join_atom_ids.as_slice())?;
        }

        for (&join_id, cofork_ids) in self.co_forks.iter() {
            let cosplits = self.group_cosplits(cofork_ids.as_slice())?;
            formula.add_cosplits(join_id, cosplits)?;
        }

        for (&fork_id, cojoin_ids) in self.co_joins.iter() {
            let cosplits = self.group_cosplits(cojoin_ids.as_slice())?;
            formula.add_cosplits(fork_id, cosplits)?;
        }

        Ok(formula)
    }

    pub fn get_formula(&self) -> Result<sat::Formula, Box<dyn Error>> {
        // FIXME if not set, choose one or the other, heuristically
        let encoding =
            self.context.lock().unwrap().get_encoding().unwrap_or(sat::Encoding::ForkJoin);

        match encoding {
            sat::Encoding::PortLink => self.get_port_link_formula(),
            sat::Encoding::ForkJoin => self.get_fork_join_formula(),
        }
    }

    pub fn solve(&mut self) -> Result<(), Box<dyn Error>> {
        if !self.is_coherent() {
            self.resolution = Resolution::Incoherent;

            Err(Box::new(AcesError::IncoherentStructure(
                self.get_name().unwrap_or("anonymous").to_owned(),
            )))
        } else {
            let formula = self.get_formula()?;

            debug!("Raw {:?}", formula);
            info!("Formula: {}", formula);

            let mut solver = Solver::new(&self.context);
            solver.add_formula(&formula)?;
            solver.inhibit_empty_solution()?;

            let search =
                self.context.lock().unwrap().get_search().unwrap_or(sat::Search::MinSolutions);

            info!(
                "Start of {}-solution search",
                match search {
                    sat::Search::MinSolutions => "min",
                    sat::Search::AllSolutions => "all",
                }
            );

            if let Some(first_solution) = solver.next() {
                let mut fcs = Vec::new();

                debug!("1. Raw {:?}", first_solution);
                fcs.push(first_solution.try_into()?);

                for (count, solution) in solver.enumerate() {
                    debug!("{}. Raw {:?}", count + 2, solution);
                    fcs.push(solution.try_into()?);
                }

                self.resolution = Resolution::Solved(fcs.into());
            } else if solver.is_sat().is_some() {
                info!("\nStructural deadlock (found no solutions).");
                self.resolution = Resolution::Deadlock;
            } else if solver.was_interrupted() {
                warn!("Solving was interrupted");
                self.resolution = Resolution::Unsolved;
            } else if let Some(err) = solver.take_last_error() {
                error!("Solving failed: {}", err);
                self.resolution = Resolution::Unsolved;
            } else {
                unreachable!()
            }

            Ok(())
        }
    }

    pub fn get_firing_set(&self) -> Option<&FiringSet> {
        if let Resolution::Solved(ref fs) = self.resolution {
            Some(fs)
        } else {
            None
        }
    }
}
