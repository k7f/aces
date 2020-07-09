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
    ContextHandle, Contextual, ExclusivelyContextual, ContentFormat, InteractiveFormat, Port,
    Wedge, Polarity, AtomId, DotId, PortId, LinkId, ForkId, JoinId, Polynomial, FiringSet, Content,
    sat, sat::Resolution, Solver, AcesError, AcesErrorKind,
};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum LinkState {
    /// Unipolar link (any structure containing it is incoherent).
    /// The [`Polarity`] value is the _missing_ orientation.
    Thin(Polarity),
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
    causes:         BTreeMap<PortId, Polynomial<LinkId>>,
    effects:        BTreeMap<PortId, Polynomial<LinkId>>,
    carrier:        BTreeSet<DotId>,
    links:          BTreeMap<LinkId, LinkState>,
    num_thin_links: u32,
    forks:          BTreeMap<DotId, Vec<AtomId>>,
    joins:          BTreeMap<DotId, Vec<AtomId>>,
    // FIXME define `struct Cowedges`, grouped on demand (cf. `group_cowedges`)
    co_forks:       BTreeMap<AtomId, Vec<AtomId>>, // Joins -> 2^Forks
    co_joins:       BTreeMap<AtomId, Vec<AtomId>>, // Forks -> 2^Joins
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

    fn add_wedge_to_tip(&mut self, wedge_id: AtomId, polarity: Polarity, dot_id: DotId) {
        let wedge_entry = match polarity {
            Polarity::Tx => self.forks.entry(dot_id),
            Polarity::Rx => self.joins.entry(dot_id),
        };

        match wedge_entry {
            btree_map::Entry::Vacant(entry) => {
                entry.insert(vec![wedge_id]);
            }
            btree_map::Entry::Occupied(mut entry) => {
                let sids = entry.get_mut();

                if let Err(pos) = sids.binary_search(&wedge_id) {
                    sids.insert(pos, wedge_id);
                } // else idempotency of addition.
            }
        }
    }

    fn add_wedge_to_pit(&mut self, wedge_id: AtomId, polarity: Polarity, co_wedge_ids: &[AtomId]) {
        for &co_wedge_id in co_wedge_ids.iter() {
            let wedge_entry = match polarity {
                Polarity::Tx => self.co_forks.entry(co_wedge_id),
                Polarity::Rx => self.co_joins.entry(co_wedge_id),
            };

            if log_enabled!(Trace) {
                match polarity {
                    Polarity::Tx => {
                        trace!(
                            "Old join's co_forks[{}] -> {}",
                            JoinId(co_wedge_id).with(&self.context),
                            ForkId(wedge_id).with(&self.context),
                        );
                    }
                    Polarity::Rx => {
                        trace!(
                            "Old fork's co_joins[{}] -> {}",
                            ForkId(co_wedge_id).with(&self.context),
                            JoinId(wedge_id).with(&self.context)
                        );
                    }
                }
            }

            match wedge_entry {
                btree_map::Entry::Vacant(entry) => {
                    entry.insert(vec![wedge_id]);
                }
                btree_map::Entry::Occupied(mut entry) => {
                    let sids = entry.get_mut();

                    if let Err(pos) = sids.binary_search(&wedge_id) {
                        sids.insert(pos, wedge_id);
                    } // else idempotency of addition.
                }
            }
        }
    }

    fn create_wedges(
        &mut self,
        polarity: Polarity,
        dot_id: DotId,
        poly: &Polynomial<LinkId>,
    ) -> Result<(), AcesError> {
        for mono in poly.get_monomials() {
            let mut fat_codot_ids = Vec::new();

            // Link sequence `mono` is ordered by `LinkId`, reorder
            // first by codot's `DotId`.
            let mut codot_map = BTreeMap::new();

            for lid in mono {
                if let Some(link_state) = self.links.get(&lid) {
                    let ctx = self.context.lock().unwrap();

                    if let Some(link) = ctx.get_link(lid) {
                        codot_map.insert(link.get_dot_id(!polarity), link_state);
                    } else {
                        return Err(AcesErrorKind::LinkMissingForId(lid).with_context(&self.context))
                    }
                } else {
                    return Err(AcesErrorKind::UnlistedAtomicInMonomial.with_context(&self.context))
                }
            }

            for (&codot_id, link_state) in codot_map.iter() {
                match link_state {
                    LinkState::Fat => {
                        fat_codot_ids.push(codot_id);
                    }
                    LinkState::Thin(_) => {} // Don't push a thin link.
                }
            }

            let mut co_wedges = Vec::new();

            for nid in fat_codot_ids.iter() {
                if let Some(wedge_ids) = match polarity {
                    Polarity::Tx => self.joins.get(nid),
                    Polarity::Rx => self.forks.get(nid),
                } {
                    let ctx = self.context.lock().unwrap();

                    for &sid in wedge_ids {
                        if let Some(wedge) = match polarity {
                            Polarity::Tx => ctx.get_join(sid.into()),
                            Polarity::Rx => ctx.get_fork(sid.into()),
                        } {
                            let pit_id = wedge.get_pit_id();
                            let pit = ctx.get_dotset(pit_id).ok_or_else(|| {
                                AcesError::from(AcesErrorKind::DotsetMissingForId(pit_id))
                            })?;

                            if pit.get_dot_ids().binary_search(&dot_id).is_ok() {
                                co_wedges.push(sid);
                            }
                        }
                    }
                } else {
                    // This codot has no co_wedges yet, a condition
                    // which should have been detected above as a thin
                    // link.
                    return Err(AcesErrorKind::IncoherencyLeak.with_context(&self.context))
                }
            }

            let wedge_id: AtomId = match polarity {
                Polarity::Tx => {
                    Wedge::new_fork_unchecked(&self.context, dot_id, codot_map.keys().copied())
                        .into()
                    // let ctx = self.context.lock().unwrap();
                    // let mut fork = Wedge::new_fork_unchecked(ctx, dot_id, codot_map.keys().copied());
                    // ctx.share_fork(&mut fork).into()
                }
                Polarity::Rx => {
                    Wedge::new_join_unchecked(&self.context, dot_id, codot_map.keys().copied())
                        .into()
                    // let ctx = self.context.lock().unwrap();
                    // let mut join = Wedge::new_join_unchecked(ctx, dot_id, codot_map.keys().copied());
                    // ctx.share_join(&mut join).into()
                }
            };

            self.add_wedge_to_tip(wedge_id, polarity, dot_id);

            if !co_wedges.is_empty() {
                self.add_wedge_to_pit(wedge_id, polarity, co_wedges.as_slice());

                if log_enabled!(Trace) {
                    match polarity {
                        Polarity::Tx => {
                            trace!(
                                "New fork's co_joins[{}] -> {:?}",
                                ForkId(wedge_id).with(&self.context),
                                co_wedges
                            );
                        }
                        Polarity::Rx => {
                            trace!(
                                "New join's co_forks[{}] -> {:?}",
                                JoinId(wedge_id).with(&self.context),
                                co_wedges
                            );
                        }
                    }
                }

                match polarity {
                    Polarity::Tx => self.co_joins.insert(wedge_id, co_wedges),
                    Polarity::Rx => self.co_forks.insert(wedge_id, co_wedges),
                };
            }
        }

        Ok(())
    }

    /// Constructs new [`Polynomial`] from a sequence of sequences of
    /// [`DotId`]s and adds it to causes or effects of a dot of this
    /// `CEStructure`.
    ///
    /// The only direct callers of this are methods [`add_causes()`]
    /// and [`add_effects()`].
    ///
    /// [`add_causes()`]: CEStructure::add_causes()
    /// [`add_effects()`]: CEStructure::add_effects()
    fn add_causes_or_effects<'a, I>(
        &mut self,
        polarity: Polarity,
        dot_id: DotId,
        poly_ids: I,
    ) -> Result<(), AcesError>
    where
        I: IntoIterator + 'a,
        I::Item: IntoIterator<Item = &'a DotId>,
    {
        let poly = Polynomial::from_dots_in_context(&self.context, polarity, dot_id, poly_ids);

        let mut port = Port::new(polarity, dot_id);
        let port_id = self.context.lock().unwrap().share_port(&mut port);

        for &lid in poly.get_atomics() {
            if let Some(what_missing) = self.links.get_mut(&lid) {
                if *what_missing == LinkState::Thin(polarity) {
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
                self.links.insert(lid, LinkState::Thin(!polarity));
                self.num_thin_links += 1;
            }
        }

        self.create_wedges(polarity, dot_id, &poly)?;

        let poly_entry = match polarity {
            Polarity::Rx => self.causes.entry(port_id),
            Polarity::Tx => self.effects.entry(port_id),
        };

        match poly_entry {
            btree_map::Entry::Vacant(entry) => {
                entry.insert(poly);
            }
            btree_map::Entry::Occupied(mut entry) => {
                entry.get_mut().add_polynomial(&poly)?;
            }
        }

        self.carrier.insert(dot_id);

        Ok(())
    }

    /// Constructs new [`Polynomial`] from a sequence of sequences of
    /// [`DotId`]s and adds it to causes of a dot of this
    /// `CEStructure`.
    ///
    /// This method is incremental: new polynomial is added to old
    /// polynomial that is already attached to the `dot_id` as dot's
    /// causes (there is always some polynomial attached, if not
    /// explicitly, then implicitly, as the default _&theta;_).
    pub fn add_causes<'a, I>(&mut self, dot_id: DotId, poly_ids: I) -> Result<(), AcesError>
    where
        I: IntoIterator + 'a,
        I::Item: IntoIterator<Item = &'a DotId>,
    {
        self.add_causes_or_effects(Polarity::Rx, dot_id, poly_ids)
    }

    /// Constructs new [`Polynomial`] from a sequence of sequences of
    /// [`DotId`]s and adds it to effects of a dot of this
    /// `CEStructure`.
    ///
    /// This method is incremental: new polynomial is added to old
    /// polynomial that is already attached to the `dot_id` as dot's
    /// effects (there is always some polynomial attached, if not
    /// explicitly, then implicitly, as the default _&theta;_).
    pub fn add_effects<'a, I>(&mut self, dot_id: DotId, poly_ids: I) -> Result<(), AcesError>
    where
        I: IntoIterator + 'a,
        I::Item: IntoIterator<Item = &'a DotId>,
    {
        self.add_causes_or_effects(Polarity::Tx, dot_id, poly_ids)
    }

    /// Extends this c-e structure with another one, which is created
    /// in the [`Context`] of the old c-e structure from a given
    /// [`Content`] trait object.
    ///
    /// [`Context`]: crate::Context
    pub fn add_from_content(&mut self, mut content: Box<dyn Content>) -> Result<(), AcesError> {
        for dot_id in content.get_carrier_ids() {
            if let Some(poly_ids) = content.get_causes_by_id(dot_id) {
                if poly_ids.is_empty() {
                    let dot_name =
                        self.context.lock().unwrap().get_dot_name(dot_id).unwrap().to_owned();

                    return Err(AcesErrorKind::EmptyCausesOfInternalDot(dot_name)
                        .with_context(&self.context))
                }

                self.add_causes(dot_id, poly_ids)?;
            }

            if let Some(poly_ids) = content.get_effects_by_id(dot_id) {
                if poly_ids.is_empty() {
                    let dot_name =
                        self.context.lock().unwrap().get_dot_name(dot_id).unwrap().to_owned();

                    return Err(AcesErrorKind::EmptyEffectsOfInternalDot(dot_name)
                        .with_context(&self.context))
                }

                self.add_effects(dot_id, poly_ids)?;
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
    pub fn with_content(mut self, content: Box<dyn Content>) -> Result<Self, AcesError> {
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
                let content = format.script_to_content(&self.context, script, Some("Main"))?;

                return self.add_from_content(content).map(|_| format.clone()).map_err(Into::into)
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

    #[inline]
    pub fn get_context(&self) -> &ContextHandle {
        &self.context
    }

    #[inline]
    pub fn get_name(&self) -> Option<&str> {
        self.content.iter().find_map(|c| c.get_name())
    }

    #[inline]
    pub fn is_module(&self) -> bool {
        self.content.iter().all(|c| c.is_module())
    }

    /// Returns link coherence status indicating whether this object
    /// represents a proper c-e structure.
    ///
    /// C-e structure is coherent iff it has no thin links, where a
    /// link is thin iff it occurs either in causes or in effects, but
    /// not in both.  Internally, there is a thin links counter
    /// associated with each `CEStructure` object.  This counter is
    /// updated whenever a polynomial is added to the structure.
    #[inline]
    pub fn is_coherent(&self) -> bool {
        self.num_thin_links == 0
    }

    pub fn get_port_link_formula(&self) -> Result<sat::Formula, AcesError> {
        let mut formula = sat::Formula::new(&self.context);

        for (&pid, poly) in self.causes.iter() {
            formula.add_polynomial(pid, poly)?;
            formula.add_anti_port(pid)?;
        }

        for (&pid, poly) in self.effects.iter() {
            formula.add_polynomial(pid, poly)?;
        }

        for (&lid, _) in self.links.iter() {
            formula.add_link_coherence(lid)?;
        }

        Ok(formula)
    }

    /// Given a flat list of co-wedges, groups them by their tip dots
    /// (i.e. by the arms of the considered wedge), and returns a
    /// vector of vectors of [`AtomId`]s.
    ///
    /// The result, if interpreted in terms of SAT encoding, is a
    /// conjunction of exclusive choices of co-wedges.
    fn group_cowedges(&self, cowedge_ids: &[AtomId]) -> Result<Vec<Vec<AtomId>>, AcesError> {
        if cowedge_ids.len() < 2 {
            if cowedge_ids.is_empty() {
                Err(AcesErrorKind::IncoherencyLeak.with_context(&self.context))
            } else {
                Ok(vec![cowedge_ids.to_vec()])
            }
        } else {
            let mut cotip_map: BTreeMap<DotId, Vec<AtomId>> = BTreeMap::new();

            for &cowedge_id in cowedge_ids.iter() {
                let ctx = self.context.lock().unwrap();
                let cowedge = ctx.get_wedge(cowedge_id).ok_or_else(|| {
                    AcesErrorKind::WedgeMissingForId(cowedge_id).with_context(&self.context)
                })?;
                let cotip_id = cowedge.get_tip_id();

                match cotip_map.entry(cotip_id) {
                    btree_map::Entry::Vacant(entry) => {
                        entry.insert(vec![cowedge_id]);
                    }
                    btree_map::Entry::Occupied(mut entry) => {
                        let sids = entry.get_mut();

                        if let Err(pos) = sids.binary_search(&cowedge_id) {
                            sids.insert(pos, cowedge_id);
                        } else {
                            warn!(
                                "Multiple occurrences of {} in cowedge array",
                                cowedge.format_locked(&ctx)?
                            );
                        }
                    }
                }
            }

            Ok(cotip_map.into_iter().map(|(_, v)| v).collect())
        }
    }

    pub fn get_fork_join_formula(&self) -> Result<sat::Formula, AcesError> {
        let mut formula = sat::Formula::new(&self.context);

        for (dot_id, fork_atom_ids) in self.forks.iter() {
            if let Some(join_atom_ids) = self.joins.get(dot_id) {
                formula.add_anti_wedges(fork_atom_ids.as_slice(), join_atom_ids.as_slice())?;
            }

            formula.add_branch_wedges(fork_atom_ids.as_slice())?;
        }

        for (_, join_atom_ids) in self.joins.iter() {
            formula.add_branch_wedges(join_atom_ids.as_slice())?;
        }

        for (&join_id, cofork_ids) in self.co_forks.iter() {
            let cowedges = self.group_cowedges(cofork_ids.as_slice())?;
            formula.add_cowedges(join_id, cowedges)?;
        }

        for (&fork_id, cojoin_ids) in self.co_joins.iter() {
            let cowedges = self.group_cowedges(cojoin_ids.as_slice())?;
            formula.add_cowedges(fork_id, cowedges)?;
        }

        Ok(formula)
    }

    pub fn get_formula(&self) -> Result<sat::Formula, AcesError> {
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
        link_id: LinkId,
        missing_polarity: Polarity,
    ) -> Result<(String, String), AcesError> {
        let ctx = self.context.lock().unwrap();

        if let Some(link) = ctx.get_link(link_id) {
            let dot_id = link.get_dot_id(!missing_polarity);
            let codot_id = link.get_dot_id(missing_polarity);

            Ok((dot_id.format_locked(&ctx)?, codot_id.format_locked(&ctx)?))
        } else {
            Err(AcesErrorKind::LinkMissingForId(link_id).with_context(&self.context))
        }
    }

    pub fn check_coherence(&self) -> Result<(), AcesError> {
        if self.is_coherent() {
            Ok(())
        } else {
            let mut first_link_info = None;

            for (&link_id, &link_state) in self.links.iter() {
                if let LinkState::Thin(missing_polarity) = link_state {
                    if log_enabled!(Debug) || first_link_info.is_none() {
                        let (tx_name, rx_name) =
                            self.get_thin_link_names(link_id, missing_polarity)?;

                        match missing_polarity {
                            Polarity::Rx => debug!("Tx-only link: {} -> {}", tx_name, rx_name,),
                            Polarity::Tx => debug!("Rx-only link: {} <- {}", tx_name, rx_name,),
                        }

                        if first_link_info.is_none() {
                            first_link_info = Some((!missing_polarity, tx_name, rx_name));
                        }
                    }
                }
            }

            Err(AcesErrorKind::IncoherentStructure(
                self.get_name().unwrap_or("anonymous").to_owned(),
                self.num_thin_links,
                first_link_info.unwrap(),
            )
            .with_context(&self.context))
        }
    }

    pub fn solve(&mut self) -> Result<(), AcesError> {
        if self.is_module() {
            Err(AcesErrorKind::ModuleSolving.with_context(&self.context))
        } else if self.carrier.is_empty() {
            Err(AcesErrorKind::EmptySolving.with_context(&self.context))
        } else if let Err(err) = self.check_coherence() {
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
