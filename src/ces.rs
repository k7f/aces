use std::{
    collections::{btree_map, BTreeMap, BTreeSet},
    convert::TryInto,
    rc::Rc,
    io::Read,
    fs::File,
    path::Path,
    error::Error,
};
use log::Level::{Debug, Trace};
use crate::{
    ContextHandle, Contextual, ExclusivelyContextual, ContentFormat, InteractiveFormat, Port, Harc,
    AtomID, NodeID, PortID, LinkID, ForkID, JoinID, Polynomial, FiringSet, Content, node, sat,
    sat::Resolution, Solver, AcesError, AcesErrorKind,
};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum LinkState {
    /// Single-face link (structure containing it is incoherent).  The
    /// [`node::Face`] value is the missing face.
    Thin(node::Face),
    Fat,
}

/// A single c-e structure.
///
/// Internally, instances of this type own structural information (the
/// cause and effect polynomials), the intermediate content
/// representation from which a c-e structure originated (optionally),
/// and some auxiliary recomputable data.  Other properties are
/// available indirectly: `CEStructure` instance owns a
/// [`ContextHandle`] which resolves to a shared [`Context`] object.
///
/// [`Context`]: crate::Context
#[derive(Debug)]
pub struct CEStructure {
    context:        ContextHandle,
    origin:         Rc<dyn ContentFormat>,
    content:        Vec<Box<dyn Content>>,
    resolution:     Resolution,
    causes:         BTreeMap<PortID, Polynomial<LinkID>>,
    effects:        BTreeMap<PortID, Polynomial<LinkID>>,
    carrier:        BTreeSet<NodeID>,
    links:          BTreeMap<LinkID, LinkState>,
    num_thin_links: u32,
    forks:          BTreeMap<NodeID, Vec<AtomID>>,
    joins:          BTreeMap<NodeID, Vec<AtomID>>,
    // FIXME define `struct Coharcs`, grouped on demand (cf. `group_coharcs`)
    co_forks:       BTreeMap<AtomID, Vec<AtomID>>, // Joins -> 2^Forks
    co_joins:       BTreeMap<AtomID, Vec<AtomID>>, // Forks -> 2^Joins
}

impl CEStructure {
    /// Creates an empty c-e structure in a [`Context`] given by a
    /// [`ContextHandle`].
    ///
    /// See also a specialized variant of this method,
    /// [`new_interactive()`].
    ///
    /// [`Context`]: crate::Context
    /// [`new_interactive()`]: CEStructure::new_interactive()
    pub fn new(ctx: &ContextHandle, origin: Rc<dyn ContentFormat>) -> Self {
        Self {
            context: ctx.clone(),
            origin,
            content: Default::default(),
            resolution: Default::default(),
            causes: Default::default(),
            effects: Default::default(),
            carrier: Default::default(),
            links: Default::default(),
            num_thin_links: 0,
            forks: Default::default(),
            joins: Default::default(),
            co_forks: Default::default(),
            co_joins: Default::default(),
        }
    }

    /// Creates an empty c-e structure in a [`Context`] given by a
    /// [`ContextHandle`], and sets content origin to
    /// [`InteractiveFormat`].
    ///
    /// This is a specialized variant of the [`new()`] method.
    ///
    /// [`Context`]: crate::Context
    /// [`new()`]: CEStructure::new()
    pub fn new_interactive(ctx: &ContextHandle) -> Self {
        CEStructure::new(ctx, Rc::new(InteractiveFormat::new()))
    }

    fn add_harc_to_host(&mut self, harc_id: AtomID, face: node::Face, node_id: NodeID) {
        let harc_entry = match face {
            node::Face::Tx => self.forks.entry(node_id),
            node::Face::Rx => self.joins.entry(node_id),
        };

        match harc_entry {
            btree_map::Entry::Vacant(entry) => {
                entry.insert(vec![harc_id]);
            }
            btree_map::Entry::Occupied(mut entry) => {
                let sids = entry.get_mut();

                if let Err(pos) = sids.binary_search(&harc_id) {
                    sids.insert(pos, harc_id);
                } // else idempotency of addition.
            }
        }
    }

    fn add_harc_to_suit(&mut self, harc_id: AtomID, face: node::Face, co_harc_ids: &[AtomID]) {
        for &co_harc_id in co_harc_ids.iter() {
            let harc_entry = match face {
                node::Face::Tx => self.co_forks.entry(co_harc_id),
                node::Face::Rx => self.co_joins.entry(co_harc_id),
            };

            if log_enabled!(Trace) {
                match face {
                    node::Face::Tx => {
                        trace!(
                            "Old join's co_forks[{}] -> {}",
                            JoinID(co_harc_id).with(&self.context),
                            ForkID(harc_id).with(&self.context),
                        );
                    }
                    node::Face::Rx => {
                        trace!(
                            "Old fork's co_joins[{}] -> {}",
                            ForkID(co_harc_id).with(&self.context),
                            JoinID(harc_id).with(&self.context)
                        );
                    }
                }
            }

            match harc_entry {
                btree_map::Entry::Vacant(entry) => {
                    entry.insert(vec![harc_id]);
                }
                btree_map::Entry::Occupied(mut entry) => {
                    let sids = entry.get_mut();

                    if let Err(pos) = sids.binary_search(&harc_id) {
                        sids.insert(pos, harc_id);
                    } // else idempotency of addition.
                }
            }
        }
    }

    fn create_harcs(
        &mut self,
        face: node::Face,
        node_id: NodeID,
        poly: &Polynomial<LinkID>,
    ) -> Result<(), AcesError> {
        use node::Face::{Tx, Rx};

        for mono in poly.get_monomials() {
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
                        return Err(AcesErrorKind::LinkMissingForID(lid).with_context(&self.context))
                    }
                } else {
                    return Err(AcesErrorKind::UnlistedAtomicInMonomial.with_context(&self.context))
                }
            }

            for (&co_node_id, link_state) in co_node_map.iter() {
                match link_state {
                    LinkState::Fat => {
                        fat_co_node_ids.push(co_node_id);
                    }
                    LinkState::Thin(_) => {} // Don't push a thin link.
                }
            }

            let mut co_harcs = Vec::new();

            for nid in fat_co_node_ids.iter() {
                if let Some(harc_ids) = match face {
                    Tx => self.joins.get(nid),
                    Rx => self.forks.get(nid),
                } {
                    let ctx = self.context.lock().unwrap();

                    for &sid in harc_ids {
                        if let Some(harc) = match face {
                            Tx => ctx.get_join(sid.into()),
                            Rx => ctx.get_fork(sid.into()),
                        } {
                            if harc.get_suit_ids().binary_search(&node_id).is_ok() {
                                co_harcs.push(sid);
                            }
                        }
                    }
                } else {
                    // This co_node has no co_harcs yet, a condition
                    // which should have been detected above as a thin
                    // link.
                    return Err(AcesErrorKind::IncoherencyLeak.with_context(&self.context))
                }
            }

            let harc_id: AtomID = match face {
                Tx => {
                    let mut fork = Harc::new_fork_unchecked(node_id, co_node_map.keys().copied());
                    self.context.lock().unwrap().share_fork(&mut fork).into()
                }
                Rx => {
                    let mut join = Harc::new_join_unchecked(node_id, co_node_map.keys().copied());
                    self.context.lock().unwrap().share_join(&mut join).into()
                }
            };

            self.add_harc_to_host(harc_id, face, node_id);

            if !co_harcs.is_empty() {
                self.add_harc_to_suit(harc_id, face, co_harcs.as_slice());

                if log_enabled!(Trace) {
                    match face {
                        Tx => {
                            trace!(
                                "New fork's co_joins[{}] -> {:?}",
                                ForkID(harc_id).with(&self.context),
                                co_harcs
                            );
                        }
                        Rx => {
                            trace!(
                                "New join's co_forks[{}] -> {:?}",
                                JoinID(harc_id).with(&self.context),
                                co_harcs
                            );
                        }
                    }
                }

                match face {
                    Tx => self.co_joins.insert(harc_id, co_harcs),
                    Rx => self.co_forks.insert(harc_id, co_harcs),
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

        self.create_harcs(face, node_id, &poly)?;

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

        self.carrier.insert(node_id);

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
    pub fn add_from_content(
        &mut self,
        mut content: Box<dyn Content>,
    ) -> Result<(), Box<dyn Error>> {
        for node_id in content.get_carrier_ids() {
            if let Some(poly_ids) = content.get_causes_by_id(node_id) {
                if poly_ids.is_empty() {
                    let node_name =
                        self.context.lock().unwrap().get_node_name(node_id).unwrap().to_owned();

                    return Err(AcesErrorKind::EmptyCausesOfInternalNode(node_name)
                        .with_context(&self.context)
                        .into())
                }

                self.add_causes(node_id, poly_ids)?;
            }

            if let Some(poly_ids) = content.get_effects_by_id(node_id) {
                if poly_ids.is_empty() {
                    let node_name =
                        self.context.lock().unwrap().get_node_name(node_id).unwrap().to_owned();

                    return Err(AcesErrorKind::EmptyEffectsOfInternalNode(node_name)
                        .with_context(&self.context)
                        .into())
                }

                self.add_effects(node_id, poly_ids)?;
            }
        }

        self.content.push(content);

        Ok(())
    }

    /// Extends this c-e structure with another one, which is created
    /// in the [`Context`] of the old c-e structure from a given
    /// [`Content`] trait object.
    ///
    /// [`Context`]: crate::Context
    pub fn with_content(mut self, content: Box<dyn Content>) -> Result<Self, Box<dyn Error>> {
        self.add_from_content(content)?;

        Ok(self)
    }

    /// Extends this c-e structure with another one, which is created
    /// in the [`Context`] of the old c-e structure from a given
    /// textual description.
    ///
    /// The `script` is interpreted according to an appropriate format
    /// of content description listed in the `formats` array.
    ///
    /// On success, returns the chosen format as a [`ContentFormat`]
    /// trait object.
    ///
    /// [`Context`]: crate::Context
    pub fn add_from_str<S: AsRef<str>>(
        &mut self,
        script: S,
        formats: &[Rc<dyn ContentFormat>],
    ) -> Result<Rc<dyn ContentFormat>, Box<dyn Error>> {
        let script = script.as_ref();

        for format in formats {
            if format.script_is_acceptable(script) {
                let content = format.script_to_content(&self.context, script)?;

                return self.add_from_content(content).map(|_| format.clone())
            }
        }

        Err(AcesErrorKind::UnknownScriptFormat.with_context(&self.context).into())
    }

    /// Extends this c-e structure with another one, which is created
    /// in the [`Context`] of the old c-e structure from a given
    /// textual description.
    ///
    /// The `script` is interpreted according to an appropriate format
    /// of content description listed in the `formats` array.
    ///
    /// On success, stores the chosen format as the new content
    /// origin.
    ///
    /// [`Context`]: crate::Context
    pub fn add_from_str_as_origin<S: AsRef<str>>(
        &mut self,
        script: S,
        formats: &[Rc<dyn ContentFormat>],
    ) -> Result<(), Box<dyn Error>> {
        self.origin = self.add_from_str(script, formats)?;

        Ok(())
    }

    /// Creates a new c-e structure from a textual description, in a
    /// [`Context`] given by a [`ContextHandle`].
    ///
    /// [`Context`]: crate::Context
    pub fn from_str<S: AsRef<str>>(
        ctx: &ContextHandle,
        script: S,
        formats: &[Rc<dyn ContentFormat>],
    ) -> Result<Self, Box<dyn Error>> {
        let mut ces = Self::new_interactive(ctx);

        ces.add_from_str_as_origin(script, formats)?;

        Ok(ces)
    }

    /// Extends this c-e structure with another one, which is created
    /// in the [`Context`] of the old c-e structure from a script file
    /// to be found along the `path`.
    ///
    /// The script file is interpreted according to an appropriate
    /// format of content description listed in the `formats` array.
    ///
    /// On success, returns the chosen format as a [`ContentFormat`]
    /// trait object.
    ///
    /// [`Context`]: crate::Context
    pub fn add_from_file<P: AsRef<Path>>(
        &mut self,
        path: P,
        formats: &[Rc<dyn ContentFormat>],
    ) -> Result<Rc<dyn ContentFormat>, Box<dyn Error>> {
        let path = path.as_ref();
        let mut ok_formats = Vec::new();

        for format in formats {
            if format.path_is_acceptable(path) {
                ok_formats.push(format.clone());
            }
        }

        let mut fp = File::open(path)?;
        let mut script = String::new();
        fp.read_to_string(&mut script)?;

        self.add_from_str(
            &script,
            if ok_formats.is_empty() { formats } else { ok_formats.as_slice() },
        )
    }

    /// Extends this c-e structure with another one, which is created
    /// in the [`Context`] of the old c-e structure from a script file
    /// to be found along the `path`.
    ///
    /// The script file is interpreted according to an appropriate
    /// format of content description listed in the `formats` array.
    ///
    /// On success, stores the chosen format as the new content
    /// origin.
    ///
    /// [`Context`]: crate::Context
    pub fn add_from_file_as_origin<P: AsRef<Path>>(
        &mut self,
        path: P,
        formats: &[Rc<dyn ContentFormat>],
    ) -> Result<(), Box<dyn Error>> {
        self.origin = self.add_from_file(path, formats)?;

        Ok(())
    }

    /// Creates a new c-e structure from a script file to be found
    /// along the `path`, in a [`Context`] given by a
    /// [`ContextHandle`].
    ///
    /// [`Context`]: crate::Context
    pub fn from_file<P: AsRef<Path>>(
        ctx: &ContextHandle,
        path: P,
        formats: &[Rc<dyn ContentFormat>],
    ) -> Result<Self, Box<dyn Error>> {
        let mut ces = Self::new_interactive(ctx);

        ces.add_from_file_as_origin(path, formats)?;

        Ok(ces)
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

    /// Given a flat list of co-harcs, groups them by their host
    /// nodes (i.e. by suit members of the considered harc), and
    /// returns a vector of vectors of [`AtomID`]s.
    ///
    /// The result, if interpreted in terms of SAT encoding, is a
    /// conjunction of exclusive choices of co-harcs.
    fn group_coharcs(&self, coharc_ids: &[AtomID]) -> Result<Vec<Vec<AtomID>>, AcesError> {
        if coharc_ids.len() < 2 {
            if coharc_ids.is_empty() {
                Err(AcesErrorKind::IncoherencyLeak.with_context(&self.context))
            } else {
                Ok(vec![coharc_ids.to_vec()])
            }
        } else {
            let mut cohost_map: BTreeMap<NodeID, Vec<AtomID>> = BTreeMap::new();

            for &coharc_id in coharc_ids.iter() {
                let ctx = self.context.lock().unwrap();
                let coharc = ctx.get_harc(coharc_id).ok_or_else(|| {
                    AcesErrorKind::HarcMissingForID(coharc_id).with_context(&self.context)
                })?;
                let cohost_id = coharc.get_host_id();

                match cohost_map.entry(cohost_id) {
                    btree_map::Entry::Vacant(entry) => {
                        entry.insert(vec![coharc_id]);
                    }
                    btree_map::Entry::Occupied(mut entry) => {
                        let sids = entry.get_mut();

                        if let Err(pos) = sids.binary_search(&coharc_id) {
                            sids.insert(pos, coharc_id);
                        } else {
                            warn!(
                                "Multiple occurrences of {} in coharc array",
                                coharc.format_locked(&ctx).unwrap()
                            );
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
                formula.add_antiharcs(fork_atom_ids.as_slice(), join_atom_ids.as_slice())?;
            }

            formula.add_sideharcs(fork_atom_ids.as_slice())?;
        }

        for (_, join_atom_ids) in self.joins.iter() {
            formula.add_sideharcs(join_atom_ids.as_slice())?;
        }

        for (&join_id, cofork_ids) in self.co_forks.iter() {
            let coharcs = self.group_coharcs(cofork_ids.as_slice())?;
            formula.add_coharcs(join_id, coharcs)?;
        }

        for (&fork_id, cojoin_ids) in self.co_joins.iter() {
            let coharcs = self.group_coharcs(cojoin_ids.as_slice())?;
            formula.add_coharcs(fork_id, coharcs)?;
        }

        Ok(formula)
    }

    pub fn get_formula(&self) -> Result<sat::Formula, Box<dyn Error>> {
        // FIXME if not set, choose one or the other, heuristically
        let encoding =
            self.context.lock().unwrap().get_encoding().unwrap_or(sat::Encoding::ForkJoin);

        debug!("Using encoding {:?}", encoding);

        match encoding {
            sat::Encoding::PortLink => self.get_port_link_formula(),
            sat::Encoding::ForkJoin => self.get_fork_join_formula(),
        }
    }

    fn get_thin_link_names(
        &self,
        link_id: LinkID,
        missing_face: node::Face,
    ) -> Result<(String, String), Box<dyn Error>> {
        let ctx = self.context.lock().unwrap();

        if let Some(link) = ctx.get_link(link_id) {
            let node_id = link.get_node_id(!missing_face);
            let conode_id = link.get_node_id(missing_face);

            Ok((node_id.format_locked(&ctx)?, conode_id.format_locked(&ctx)?))
        } else {
            Err(AcesErrorKind::LinkMissingForID(link_id).with_context(&self.context).into())
        }
    }

    pub fn check_coherence(&self) -> Result<(), Box<dyn Error>> {
        if self.is_coherent() {
            Ok(())
        } else {
            let mut first_link_info = None;

            for (&link_id, &link_state) in self.links.iter() {
                if let LinkState::Thin(missing_face) = link_state {
                    if log_enabled!(Debug) || first_link_info.is_none() {
                        let (tx_name, rx_name) = self.get_thin_link_names(link_id, missing_face)?;

                        match missing_face {
                            node::Face::Rx => debug!("Tx-only link: {} -> {}", tx_name, rx_name,),
                            node::Face::Tx => debug!("Rx-only link: {} <- {}", tx_name, rx_name,),
                        }

                        if first_link_info.is_none() {
                            first_link_info = Some((!missing_face, tx_name, rx_name));
                        }
                    }
                }
            }

            Err(AcesErrorKind::IncoherentStructure(
                self.get_name().unwrap_or("anonymous").to_owned(),
                self.num_thin_links,
                first_link_info.unwrap(),
            )
            .with_context(&self.context)
            .into())
        }
    }

    pub fn solve(&mut self) -> Result<(), Box<dyn Error>> {
        if let Err(err) = self.check_coherence() {
            self.resolution = Resolution::Incoherent;

            Err(err)
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
