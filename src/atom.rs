use std::{
    fmt, hash,
    collections::{BTreeMap, BTreeSet, HashMap},
};
use crate::{
    Polarity, AnyId, DotId, Context, ContextHandle, Contextual, ExclusivelyContextual, InContext,
    AcesError, AcesErrorKind, sat,
    domain::{Dotset, DotsetId},
};

/// An abstract structural identifier serving as the common base of
/// [`PortId`], [`LinkId`], [`ForkId`] and [`JoinId`].
///
/// Since this is a numeric identifier, which is serial and one-based,
/// it trivially maps into numeric codes of variables in the DIMACS
/// SAT format.
///
/// See [`AnyId`] for more details.
pub type AtomId = AnyId;

/// An identifier of a [`Port`], a type derived from [`AtomId`].
///
/// There is a trivial bijection between values of this type and
/// numeric codes of DIMACS variables.  This mapping simplifies the
/// construction of SAT queries and interpretation of solutions.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct PortId(pub(crate) AtomId);

impl PortId {
    #[inline]
    pub const fn get(self) -> AtomId {
        self.0
    }
}

impl From<AtomId> for PortId {
    #[inline]
    fn from(id: AtomId) -> Self {
        PortId(id)
    }
}

impl From<PortId> for AtomId {
    #[inline]
    fn from(id: PortId) -> Self {
        id.0
    }
}

impl ExclusivelyContextual for PortId {
    fn format_locked(&self, ctx: &Context) -> Result<String, AcesError> {
        let port = ctx
            .get_port(*self)
            .ok_or_else(|| AcesError::from(AcesErrorKind::PortMissingForId(*self)))?;
        port.format_locked(ctx)
    }
}

/// An identifier of a [`Link`], a type derived from [`AtomId`].
///
/// There is a trivial bijection between values of this type and
/// numeric codes of DIMACS variables.  This mapping simplifies the
/// construction of SAT queries and interpretation of solutions.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(transparent)]
pub struct LinkId(pub(crate) AtomId);

impl LinkId {
    #[inline]
    pub const fn get(self) -> AtomId {
        self.0
    }
}

impl From<AtomId> for LinkId {
    #[inline]
    fn from(id: AtomId) -> Self {
        LinkId(id)
    }
}

impl From<LinkId> for AtomId {
    #[inline]
    fn from(id: LinkId) -> Self {
        id.0
    }
}

impl ExclusivelyContextual for LinkId {
    fn format_locked(&self, ctx: &Context) -> Result<String, AcesError> {
        let link = ctx
            .get_link(*self)
            .ok_or_else(|| AcesError::from(AcesErrorKind::LinkMissingForId(*self)))?;
        link.format_locked(ctx)
    }
}

/// An identifier of a [`Fork`], a type derived from [`AtomId`].
///
/// There is a trivial bijection between values of this type and
/// numeric codes of DIMACS variables.  This mapping simplifies the
/// construction of SAT queries and interpretation of solutions.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct ForkId(pub(crate) AtomId);

impl ForkId {
    #[inline]
    pub const fn get(self) -> AtomId {
        self.0
    }
}

impl From<AtomId> for ForkId {
    #[inline]
    fn from(id: AtomId) -> Self {
        ForkId(id)
    }
}

impl From<ForkId> for AtomId {
    #[inline]
    fn from(id: ForkId) -> Self {
        id.0
    }
}

impl ExclusivelyContextual for ForkId {
    fn format_locked(&self, ctx: &Context) -> Result<String, AcesError> {
        let fork = ctx
            .get_fork(*self)
            .ok_or_else(|| AcesError::from(AcesErrorKind::ForkMissingForId(*self)))?;
        fork.format_locked(ctx)
    }
}

/// An identifier of a [`Join`], a type derived from [`AtomId`].
///
/// There is a trivial bijection between values of this type and
/// numeric codes of DIMACS variables.  This mapping simplifies the
/// construction of SAT queries and interpretation of solutions.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct JoinId(pub(crate) AtomId);

impl JoinId {
    #[inline]
    pub const fn get(self) -> AtomId {
        self.0
    }
}

impl From<AtomId> for JoinId {
    #[inline]
    fn from(id: AtomId) -> Self {
        JoinId(id)
    }
}

impl From<JoinId> for AtomId {
    #[inline]
    fn from(id: JoinId) -> Self {
        id.0
    }
}

impl ExclusivelyContextual for JoinId {
    fn format_locked(&self, ctx: &Context) -> Result<String, AcesError> {
        let join = ctx
            .get_join(*self)
            .ok_or_else(|| AcesError::from(AcesErrorKind::JoinMissingForId(*self)))?;
        join.format_locked(ctx)
    }
}

/// An identifier of a [`Fuset`], a type derived from [`AtomId`].
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct FusetId(pub(crate) AtomId);

impl FusetId {
    #[inline]
    pub const fn get(self) -> AtomId {
        self.0
    }
}

impl From<AtomId> for FusetId {
    #[inline]
    fn from(id: AtomId) -> Self {
        FusetId(id)
    }
}

impl From<FusetId> for AtomId {
    #[inline]
    fn from(id: FusetId) -> Self {
        id.0
    }
}

impl ExclusivelyContextual for FusetId {
    fn format_locked(&self, ctx: &Context) -> Result<String, AcesError> {
        let fuset = ctx
            .get_fuset(*self)
            .ok_or_else(|| AcesError::from(AcesErrorKind::FusetMissingForId(*self)))?;
        fuset.format_locked(ctx)
    }
}

/// A collection of [`Atom`]s: [`Port`]s, [`Link`]s, [`Fork`]s,
/// [`Join`]s and [`Dotset`]s.
///
/// [`AtomSpace`] maintains a mapping from [`Atom`]s to [`AtomId`]s,
/// its inverse, and a mapping from [`DotId`]s to [`PortId`]s.  For
/// the reverse mapping, from [`PortId`]s to [`DotId`]s, call
/// [`AtomSpace::get_port()`] followed by [`Port::get_dot_id()`].
#[derive(Clone, Debug)]
pub(crate) struct AtomSpace {
    atoms:           Vec<Atom>,
    atom_ids:        HashMap<Atom, AtomId>,
    source_dots:   BTreeMap<DotId, PortId>,
    sink_dots:     BTreeMap<DotId, PortId>,
    internal_dots: BTreeMap<DotId, (PortId, PortId)>,
}

impl Default for AtomSpace {
    fn default() -> Self {
        Self {
            atoms:           vec![Atom::Bottom],
            atom_ids:        Default::default(),
            source_dots:   Default::default(),
            sink_dots:     Default::default(),
            internal_dots: Default::default(),
        }
    }
}

impl AtomSpace {
    fn do_share_atom(&mut self, mut new_atom: Atom) -> AtomId {
        if let Some(old_atom_id) = self.get_atom_id(&new_atom) {
            if new_atom.get_atom_id().is_none() {
                trace!("Resharing: {:?}", new_atom);

                old_atom_id
            } else {
                panic!("Attempt to reset identifier of atom {:?}", new_atom);
            }
        } else {
            let atom_id = unsafe { AtomId::new_unchecked(self.atoms.len()) };
            new_atom.set_atom_id(atom_id);

            trace!("New share: {:?}", new_atom);

            self.atoms.push(new_atom.clone());
            self.atom_ids.insert(new_atom, atom_id);

            atom_id
        }
    }

    pub(crate) fn share_port(&mut self, port: &mut Port) -> PortId {
        let tip = port.dot_id;

        match port.polarity {
            Polarity::Tx => {
                let atom_id = self.do_share_atom(Atom::Tx(port.clone()));

                port.atom_id = Some(atom_id);

                let pid = PortId(atom_id);

                if let Some(&rx_id) = self.sink_dots.get(&tip) {
                    self.sink_dots.remove(&tip);
                    self.internal_dots.insert(tip, (pid, rx_id));
                } else {
                    self.source_dots.insert(tip, pid);
                }

                pid
            }
            Polarity::Rx => {
                let atom_id = self.do_share_atom(Atom::Rx(port.clone()));

                port.atom_id = Some(atom_id);

                let pid = PortId(atom_id);

                if let Some(&tx_id) = self.source_dots.get(&tip) {
                    self.source_dots.remove(&tip);
                    self.internal_dots.insert(tip, (tx_id, pid));
                } else {
                    self.sink_dots.insert(tip, pid);
                }

                pid
            }
        }
    }

    #[inline]
    pub(crate) fn share_link(&mut self, link: &mut Link) -> LinkId {
        let atom_id = self.do_share_atom(Atom::Link(link.clone()));

        link.atom_id = Some(atom_id);

        LinkId(atom_id)
    }

    #[inline]
    pub(crate) fn share_fork(&mut self, fork: &mut Fork) -> ForkId {
        let atom_id = self.do_share_atom(Atom::Fork(fork.clone()));

        fork.atom_id = Some(atom_id);

        ForkId(atom_id)
    }

    #[inline]
    pub(crate) fn share_join(&mut self, join: &mut Join) -> JoinId {
        let atom_id = self.do_share_atom(Atom::Join(join.clone()));

        join.atom_id = Some(atom_id);

        JoinId(atom_id)
    }

    pub(crate) fn share_fork_from_tip_and_pit(
        &mut self,
        tip_id: DotId,
        mut pit: Dotset,
    ) -> ForkId {
        let pit_id = self.share_dotset(&mut pit);
        let mut fork = Wedge::new(Polarity::Tx, tip_id, pit_id);

        self.share_fork(&mut fork)
    }

    pub(crate) fn share_join_from_tip_and_pit(
        &mut self,
        tip_id: DotId,
        mut pit: Dotset,
    ) -> JoinId {
        let pit_id = self.share_dotset(&mut pit);
        let mut join = Wedge::new(Polarity::Rx, tip_id, pit_id);

        self.share_join(&mut join)
    }

    #[inline]
    pub(crate) fn share_dotset(&mut self, pit: &mut Dotset) -> DotsetId {
        let atom_id = self.do_share_atom(Atom::Pit(pit.clone()));

        pit.atom_id = Some(atom_id);

        DotsetId(atom_id)
    }

    #[inline]
    pub(crate) fn share_fuset(&mut self, fuset: &mut Fuset) -> FusetId {
        let atom_id = self.do_share_atom(Atom::Fuset(fuset.clone()));

        fuset.atom_id = Some(atom_id);

        FusetId(atom_id)
    }

    #[inline]
    pub(crate) fn get_atom(&self, atom_id: AtomId) -> Option<&Atom> {
        self.atoms.get(atom_id.get())
    }

    #[inline]
    pub(crate) fn get_atom_id(&self, atom: &Atom) -> Option<AtomId> {
        self.atom_ids.get(atom).copied()
    }

    #[inline]
    pub(crate) fn is_port(&self, atom_id: AtomId) -> bool {
        match self.get_atom(atom_id) {
            Some(Atom::Tx(_)) | Some(Atom::Rx(_)) => true,
            _ => false,
        }
    }

    #[inline]
    pub(crate) fn get_port(&self, pid: PortId) -> Option<&Port> {
        match self.get_atom(pid.into()) {
            Some(Atom::Tx(a)) => Some(a),
            Some(Atom::Rx(a)) => Some(a),
            _ => None,
        }
    }

    #[inline]
    pub(crate) fn is_link(&self, atom_id: AtomId) -> bool {
        match self.get_atom(atom_id) {
            Some(Atom::Link(_)) => true,
            _ => false,
        }
    }

    #[inline]
    pub(crate) fn get_link(&self, lid: LinkId) -> Option<&Link> {
        match self.get_atom(lid.into()) {
            Some(Atom::Link(a)) => Some(a),
            _ => None,
        }
    }

    #[inline]
    pub(crate) fn is_wedge(&self, atom_id: AtomId) -> bool {
        match self.get_atom(atom_id) {
            Some(Atom::Fork(_)) | Some(Atom::Join(_)) => true,
            _ => false,
        }
    }

    #[inline]
    pub(crate) fn get_wedge(&self, aid: AtomId) -> Option<&Wedge> {
        match self.get_atom(aid) {
            Some(Atom::Fork(a)) => Some(a),
            Some(Atom::Join(a)) => Some(a),
            _ => None,
        }
    }

    #[inline]
    pub(crate) fn is_fork(&self, atom_id: AtomId) -> bool {
        match self.get_atom(atom_id) {
            Some(Atom::Fork(_)) => true,
            _ => false,
        }
    }

    #[inline]
    pub(crate) fn get_fork(&self, fid: ForkId) -> Option<&Fork> {
        match self.get_atom(fid.into()) {
            Some(Atom::Fork(a)) => Some(a),
            _ => None,
        }
    }

    #[inline]
    pub(crate) fn is_join(&self, atom_id: AtomId) -> bool {
        match self.get_atom(atom_id) {
            Some(Atom::Join(_)) => true,
            _ => false,
        }
    }

    #[inline]
    pub(crate) fn get_join(&self, jid: JoinId) -> Option<&Join> {
        match self.get_atom(jid.into()) {
            Some(Atom::Join(a)) => Some(a),
            _ => None,
        }
    }

    #[inline]
    #[allow(dead_code)]
    pub(crate) fn is_dotset(&self, atom_id: AtomId) -> bool {
        match self.get_atom(atom_id) {
            Some(Atom::Pit(_)) => true,
            _ => false,
        }
    }

    #[inline]
    pub(crate) fn get_dotset(&self, sid: DotsetId) -> Option<&Dotset> {
        match self.get_atom(sid.into()) {
            Some(Atom::Pit(a)) => Some(a),
            _ => None,
        }
    }

    #[inline]
    pub(crate) fn get_fuset(&self, fid: FusetId) -> Option<&Fuset> {
        match self.get_atom(fid.into()) {
            Some(Atom::Fuset(a)) => Some(a),
            _ => None,
        }
    }

    pub fn get_anti_port_id(&self, pid: PortId) -> Option<PortId> {
        if let Some(port) = self.get_port(pid) {
            if let Some(&(tx_id, rx_id)) = self.internal_dots.get(&port.dot_id) {
                match port.polarity {
                    Polarity::Tx => {
                        if tx_id == pid {
                            return Some(rx_id)
                        } else {
                            panic!("Corrupt atom space")
                        }
                    }
                    Polarity::Rx => {
                        if rx_id == pid {
                            return Some(tx_id)
                        } else {
                            panic!("Corrupt atom space")
                        }
                    }
                }
            }
        }

        None
    }
}

#[derive(Clone, Eq, Debug)]
pub(crate) enum Atom {
    Tx(Port),
    Rx(Port),
    Link(Link),
    Fork(Fork),
    Join(Join),
    Pit(Dotset),
    Fuset(Fuset),
    Bottom,
}

impl Atom {
    fn set_atom_id(&mut self, atom_id: AtomId) {
        use Atom::*;

        let prev_id = match self {
            Tx(p) => &mut p.atom_id,
            Rx(p) => &mut p.atom_id,
            Link(l) => &mut l.atom_id,
            Fork(f) => &mut f.atom_id,
            Join(j) => &mut j.atom_id,
            Pit(s) => &mut s.atom_id,
            Fuset(f) => &mut f.atom_id,
            Bottom => panic!("Attempt to set identifier of the bottom atom"),
        };

        if *prev_id == None {
            *prev_id = Some(atom_id);
        } else {
            panic!("Attempt to reset identifier of atom {:?}", self);
        }
    }

    fn get_atom_id(&self) -> Option<AtomId> {
        use Atom::*;

        match self {
            Tx(p) => p.atom_id,
            Rx(p) => p.atom_id,
            Link(l) => l.atom_id,
            Fork(f) => f.atom_id,
            Join(j) => j.atom_id,
            Pit(s) => s.atom_id,
            Fuset(f) => f.atom_id,
            Bottom => panic!("Attempt to get identifier of the bottom atom"),
        }
    }
}

impl PartialEq for Atom {
    #[rustfmt::skip]
    fn eq(&self, other: &Self) -> bool {
        use Atom::*;

        match self {
            Tx(p) => if let Tx(o) = other { p == o } else { false },
            Rx(p) => if let Rx(o) = other { p == o } else { false },
            Link(l) => if let Link(o) = other { l == o } else { false },
            Fork(f) => if let Fork(o) = other { f == o } else { false },
            Join(j) => if let Join(o) = other { j == o } else { false },
            Pit(s) => if let Pit(o) = other { s == o } else { false },
            Fuset(f) => if let Fuset(o) = other { f == o } else { false },
            Bottom => panic!("Attempt to access the bottom atom"),
        }
    }
}

impl hash::Hash for Atom {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        use Atom::*;

        match self {
            Tx(p) | Rx(p) => p.hash(state),
            Link(l) => l.hash(state),
            Fork(f) => f.hash(state),
            Join(j) => j.hash(state),
            Pit(s) => s.hash(state),
            Fuset(f) => f.hash(state),
            Bottom => panic!("Attempt to access the bottom atom"),
        }
    }
}

/// Representation of a port.
///
/// This is an oriented dot, i.e. a dot coupled with a specific
/// [`Polarity`].
#[derive(Clone, Eq, Debug)]
pub struct Port {
    polarity: Polarity,
    atom_id:  Option<AtomId>,
    dot_id:   DotId,
}

impl Port {
    pub(crate) fn new(polarity: Polarity, dot_id: DotId) -> Self {
        Self { polarity, atom_id: None, dot_id }
    }

    pub(crate) fn get_polarity(&self) -> Polarity {
        self.polarity
    }

    pub fn get_atom_id(&self) -> AtomId {
        self.atom_id.expect("Attempt to access an uninitialized port")
    }

    pub fn get_dot_id(&self) -> DotId {
        self.dot_id
    }
}

impl PartialEq for Port {
    fn eq(&self, other: &Self) -> bool {
        self.dot_id == other.dot_id
    }
}

impl hash::Hash for Port {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.polarity.hash(state);
        self.dot_id.hash(state);
    }
}

impl ExclusivelyContextual for Port {
    fn format_locked(&self, ctx: &Context) -> Result<String, AcesError> {
        let dot_name = ctx
            .get_dot_name(self.get_dot_id())
            .ok_or_else(|| AcesError::from(AcesErrorKind::DotMissingForPort(self.get_polarity())))?;

        Ok(format!("[{} {}]", dot_name, self.get_polarity()))
    }
}

/// Representation of a link.
///
/// This is a fat (bipolar) link, if used on its own, or a thin
/// (unipolar) link, if paired with a [`Polarity`].  See [`CEStructure`]'s
/// private field `links`, or the implementation of
/// [`CEStructure::check_coherence()`].
///
/// [`CEStructure`]: crate::CEStructure
/// [`CEStructure::check_coherence()`]: crate::CEStructure::check_coherence()
#[derive(Clone, Eq, Debug)]
pub struct Link {
    atom_id:     Option<AtomId>,
    tx_port_id:  PortId,
    tx_dot_id: DotId,
    rx_port_id:  PortId,
    rx_dot_id: DotId,
}

impl Link {
    pub fn new(
        tx_port_id: PortId,
        tx_dot_id: DotId,
        rx_port_id: PortId,
        rx_dot_id: DotId,
    ) -> Self {
        Self { atom_id: None, tx_port_id, tx_dot_id, rx_port_id, rx_dot_id }
    }

    pub fn get_atom_id(&self) -> AtomId {
        self.atom_id.expect("Attempt to access an uninitialized link")
    }

    pub fn get_link_id(&self) -> LinkId {
        LinkId(self.get_atom_id())
    }

    pub fn get_port_id(&self, polarity: Polarity) -> PortId {
        if polarity == Polarity::Rx {
            self.rx_port_id
        } else {
            self.tx_port_id
        }
    }

    pub fn get_dot_id(&self, polarity: Polarity) -> DotId {
        if polarity == Polarity::Rx {
            self.rx_dot_id
        } else {
            self.tx_dot_id
        }
    }

    pub fn get_tx_port_id(&self) -> PortId {
        self.tx_port_id
    }

    pub fn get_tx_dot_id(&self) -> DotId {
        self.tx_dot_id
    }

    pub fn get_rx_port_id(&self) -> PortId {
        self.rx_port_id
    }

    pub fn get_rx_dot_id(&self) -> DotId {
        self.rx_dot_id
    }
}

impl PartialEq for Link {
    fn eq(&self, other: &Self) -> bool {
        self.tx_port_id == other.tx_port_id && self.rx_dot_id == other.rx_dot_id
    }
}

impl hash::Hash for Link {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.tx_port_id.hash(state);
        self.tx_dot_id.hash(state);
        self.rx_port_id.hash(state);
        self.rx_dot_id.hash(state);
    }
}

impl ExclusivelyContextual for Link {
    fn format_locked(&self, ctx: &Context) -> Result<String, AcesError> {
        let tx_dot_name = ctx
            .get_dot_name(self.get_tx_dot_id())
            .ok_or_else(|| AcesError::from(AcesErrorKind::DotMissingForLink(Polarity::Tx)))?;
        let rx_dot_name = ctx
            .get_dot_name(self.get_rx_dot_id())
            .ok_or_else(|| AcesError::from(AcesErrorKind::DotMissingForLink(Polarity::Rx)))?;

        Ok(format!("({} > {})", tx_dot_name, rx_dot_name))
    }
}

/// A common type of one-to-many and many-to-one elements of the fuset
/// representation of c-e structures.
///
/// A wide edge represents a monomial attached to a dot.  There are
/// two variants of a `Wedge`: [`Join`] represents causes and [`Fork`]
/// represents effects.
#[derive(Clone, Eq)]
pub struct Wedge {
    atom_id:  Option<AtomId>,
    polarity: Polarity,
    tip_id:   DotId,
    pit_id:   DotsetId,
}

impl Wedge {
    pub(crate) fn new(polarity: Polarity, tip_id: DotId, pit_id: DotsetId) -> Self {
        Wedge { atom_id: None, polarity, tip_id, pit_id }
    }

    /// [`Fork`]'s constructor.
    ///
    /// See also  [`Wedge::new_fork_unchecked()`].
    pub fn new_fork<I>(ctx: &ContextHandle, tip_id: DotId, arm_ids: I) -> ForkId
    where
        I: IntoIterator<Item = DotId>,
    {
        let pit = Dotset::new(arm_ids);
        trace!("New fork: {:?} -> {:?}", tip_id, pit);
        ctx.lock().unwrap().share_fork_from_tip_and_pit(tip_id, pit)
    }

    /// [`Join`]'s constructor.
    ///
    /// See also  [`Wedge::new_join_unchecked()`].
    pub fn new_join<I>(ctx: &ContextHandle, tip_id: DotId, arm_ids: I) -> JoinId
    where
        I: IntoIterator<Item = DotId>,
    {
        let pit = Dotset::new(arm_ids);
        trace!("New join: {:?} <- {:?}", tip_id, pit);
        ctx.lock().unwrap().share_join_from_tip_and_pit(tip_id, pit)
    }

    /// A more efficient variant of [`Wedge::new_fork()`].
    ///
    /// Note: new [`Fork`] is created under the assumption that
    /// `arm_ids` are nonempty and listed in ascending order.  If the
    /// caller fails to provide an ordered pit, the library may panic
    /// in some other call (the constructor itself panics immediately
    /// in debug mode).
    #[inline]
    pub fn new_fork_unchecked<I>(ctx: &ContextHandle, tip_id: DotId, arm_ids: I) -> ForkId
    where
        I: IntoIterator<Item = DotId>,
    {
        let pit = Dotset::new_unchecked(arm_ids);
        trace!("New fork: {:?} -> {:?}", tip_id, pit);
        ctx.lock().unwrap().share_fork_from_tip_and_pit(tip_id, pit)
    }

    /// A more efficient variant of [`Wedge::new_join()`].
    ///
    /// Note: new [`Join`] is created under the assumption that
    /// `arm_ids` are nonempty and listed in ascending order.  If the
    /// caller fails to provide an ordered pit, the library may panic
    /// in some other call (the constructor itself panics immediately
    /// in debug mode).
    #[inline]
    pub fn new_join_unchecked<I>(ctx: &ContextHandle, tip_id: DotId, arm_ids: I) -> JoinId
    where
        I: IntoIterator<Item = DotId>,
    {
        let pit = Dotset::new_unchecked(arm_ids);
        trace!("New join: {:?} <- {:?}", tip_id, pit);
        ctx.lock().unwrap().share_join_from_tip_and_pit(tip_id, pit)
    }

    #[inline]
    pub fn get_atom_id(&self) -> AtomId {
        match self.polarity {
            Polarity::Tx => self.atom_id.expect("Attempt to access an uninitialized fork"),
            Polarity::Rx => self.atom_id.expect("Attempt to access an uninitialized join"),
        }
    }

    #[inline]
    pub fn get_fork_id(&self) -> Option<ForkId> {
        match self.polarity {
            Polarity::Tx => Some(ForkId(self.get_atom_id())),
            Polarity::Rx => None,
        }
    }

    #[inline]
    pub fn get_join_id(&self) -> Option<JoinId> {
        match self.polarity {
            Polarity::Tx => None,
            Polarity::Rx => Some(JoinId(self.get_atom_id())),
        }
    }

    #[inline]
    pub fn get_polarity(&self) -> Polarity {
        self.polarity
    }

    #[inline]
    pub fn get_tip_id(&self) -> DotId {
        self.tip_id
    }

    #[inline]
    pub fn get_pit_id(&self) -> DotsetId {
        self.pit_id
    }
}

impl fmt::Debug for Wedge {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {{ atom_id: ",
            match self.polarity {
                Polarity::Tx => "Fork",
                Polarity::Rx => "Join",
            }
        )?;
        self.atom_id.fmt(f)?;
        write!(f, ", tip_id: ")?;
        self.tip_id.fmt(f)?;
        write!(f, ", pit_id: ")?;
        self.pit_id.fmt(f)?;
        write!(f, " }}")
    }
}

impl PartialEq for Wedge {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.polarity == other.polarity && self.tip_id == other.tip_id && self.pit_id == other.pit_id
    }
}

impl hash::Hash for Wedge {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.polarity.hash(state);
        self.tip_id.hash(state);
        self.pit_id.hash(state);
    }
}

impl ExclusivelyContextual for Wedge {
    fn format_locked(&self, ctx: &Context) -> Result<String, AcesError> {
        let tip_name = ctx.get_dot_name(self.tip_id).ok_or(match self.polarity {
            Polarity::Tx => AcesError::from(AcesErrorKind::DotMissingForFork(Polarity::Tx)),
            Polarity::Rx => AcesError::from(AcesErrorKind::DotMissingForJoin(Polarity::Rx)),
        })?;

        let pit = ctx
            .get_dotset(self.pit_id)
            .ok_or_else(|| AcesError::from(AcesErrorKind::DotsetMissingForId(self.pit_id)))?;

        let arm_names: Result<Vec<_>, AcesError> = pit
            .get_dot_ids()
            .iter()
            .map(|&dot_id| {
                ctx.get_dot_name(dot_id).ok_or(match self.polarity {
                    Polarity::Tx => AcesError::from(AcesErrorKind::DotMissingForFork(Polarity::Rx)),
                    Polarity::Rx => AcesError::from(AcesErrorKind::DotMissingForJoin(Polarity::Tx)),
                })
            })
            .collect();

        match self.polarity {
            Polarity::Tx => Ok(format!("({} > {:?})", tip_name, arm_names?)),
            Polarity::Rx => Ok(format!("({:?} > {})", arm_names?, tip_name)),
        }
    }
}

/// Forward wide edge: representation of effects.
pub type Fork = Wedge;

/// Backward wide edge: representation of causes.
pub type Join = Wedge;

/// A trait of an identifier convertible into [`DotId`] and into
/// [`sat::Literal`].
pub trait Atomic:
    From<AtomId> + Into<AtomId> + Contextual + Copy + PartialEq + Eq + PartialOrd + Ord
{
    fn into_dot_id(this: InContext<Self>) -> Option<DotId>;

    fn into_dot_id_docked(this: InContext<Self>, _dock: Polarity) -> Option<DotId> {
        Self::into_dot_id(this)
    }

    fn into_sat_literal(self, negated: bool) -> sat::Literal;
}

impl Atomic for PortId {
    fn into_dot_id(this: InContext<Self>) -> Option<DotId> {
        this.using_context(|pid, ctx| ctx.get_port(*pid).map(|port| port.get_dot_id()))
    }

    #[inline]
    fn into_sat_literal(self, negated: bool) -> sat::Literal {
        sat::Literal::from_atom_id(self.get(), negated)
    }
}

impl Atomic for LinkId {
    fn into_dot_id(_this: InContext<Self>) -> Option<DotId> {
        None
    }

    fn into_dot_id_docked(this: InContext<Self>, dock: Polarity) -> Option<DotId> {
        this.using_context(|lid, ctx| ctx.get_link(*lid).map(|link| link.get_dot_id(dock)))
    }

    #[inline]
    fn into_sat_literal(self, negated: bool) -> sat::Literal {
        sat::Literal::from_atom_id(self.get(), negated)
    }
}

impl Atomic for ForkId {
    fn into_dot_id(this: InContext<Self>) -> Option<DotId> {
        this.using_context(|fid, ctx| ctx.get_fork(*fid).map(|fork| fork.get_tip_id()))
    }

    #[inline]
    fn into_sat_literal(self, negated: bool) -> sat::Literal {
        sat::Literal::from_atom_id(self.get(), negated)
    }
}

impl Atomic for JoinId {
    fn into_dot_id(this: InContext<Self>) -> Option<DotId> {
        this.using_context(|jid, ctx| ctx.get_join(*jid).map(|join| join.get_tip_id()))
    }

    #[inline]
    fn into_sat_literal(self, negated: bool) -> sat::Literal {
        sat::Literal::from_atom_id(self.get(), negated)
    }
}

/// Fuset: a set of forks and joins.
///
/// Represented as two ordered and deduplicated `Vec`s, one of
/// [`ForkId`]s, and another of [`JoinId`]s.
#[derive(Clone, Eq, Debug)]
pub struct Fuset {
    pub(crate) atom_id:  Option<AtomId>,
    pub(crate) fork_ids: Vec<ForkId>,
    pub(crate) join_ids: Vec<JoinId>,
}

impl Fuset {
    /// [`Fuset`] constructor.
    ///
    /// See also  [`Fuset::new_unchecked()`].
    pub fn new<I, J>(fork_ids: I, join_ids: J) -> Self
    where
        I: IntoIterator<Item = ForkId>,
        J: IntoIterator<Item = JoinId>,
    {
        let fork_ids: BTreeSet<_> = fork_ids.into_iter().collect();
        let join_ids: BTreeSet<_> = join_ids.into_iter().collect();

        if fork_ids.is_empty() {
            // FIXME
        }

        if join_ids.is_empty() {
            // FIXME
        }

        Self::new_unchecked(fork_ids, join_ids)
    }

    /// A more efficient variant of [`Fuset::new()`].
    ///
    /// Note: new [`Fuset`] is created under the assumption that
    /// `fork_ids` and `join_ids` are nonempty and listed in ascending
    /// order.  If the caller fails to provide ordered sets of forks
    /// and joins, the library may panic in some other call (the
    /// constructor itself panics immediately in debug mode).
    pub fn new_unchecked<I, J>(fork_ids: I, join_ids: J) -> Self
    where
        I: IntoIterator<Item = ForkId>,
        J: IntoIterator<Item = JoinId>,
    {
        let fork_ids: Vec<_> = fork_ids.into_iter().collect();
        let join_ids: Vec<_> = join_ids.into_iter().collect();
        trace!("New fuset: {:?}->{:?}", fork_ids, join_ids);

        if cfg!(debug_assertions) {
            let mut fiter = fork_ids.iter();

            if let Some(fid) = fiter.next() {
                let mut prev_fid = *fid;

                for &fid in fiter {
                    assert!(prev_fid < fid, "Unordered set of forks");
                    prev_fid = fid;
                }
            } else {
                panic!("Empty set of forks")
            }

            let mut jiter = join_ids.iter();

            if let Some(jid) = jiter.next() {
                let mut prev_jid = *jid;

                for &jid in jiter {
                    assert!(prev_jid < jid, "Unordered set of joins");
                    prev_jid = jid;
                }
            } else {
                panic!("Empty set of joins")
            }
        }

        Fuset { atom_id: None, fork_ids, join_ids }
    }

    #[inline]
    pub fn get_atom_id(&self) -> AtomId {
        self.atom_id.expect("Attempt to access an uninitialized fuset")
    }

    #[inline]
    pub fn get_id(&self) -> Option<FusetId> {
        self.atom_id.map(FusetId)
    }

    #[inline]
    pub fn get_fork_ids(&self) -> &[ForkId] {
        self.fork_ids.as_slice()
    }

    #[inline]
    pub fn get_join_ids(&self) -> &[JoinId] {
        self.join_ids.as_slice()
    }
}

impl PartialEq for Fuset {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.fork_ids == other.fork_ids && self.join_ids == other.join_ids
    }
}

impl hash::Hash for Fuset {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.fork_ids.hash(state);
        self.join_ids.hash(state);
    }
}

impl ExclusivelyContextual for Fuset {
    fn format_locked(&self, ctx: &Context) -> Result<String, AcesError> {
        let forks: Result<Vec<_>, AcesError> = self
            .fork_ids
            .iter()
            .map(|&fork_id| {
                ctx.get_fork(fork_id).ok_or_else(|| AcesErrorKind::ForkMissingForId(fork_id).into())
            })
            .collect();

        let joins: Result<Vec<_>, AcesError> = self
            .join_ids
            .iter()
            .map(|&join_id| {
                ctx.get_join(join_id).ok_or_else(|| AcesErrorKind::JoinMissingForId(join_id).into())
            })
            .collect();

        Ok(format!("({:?}->{:?})", forks?, joins?))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn new_tx_port(id: usize) -> Port {
        Port::new(Polarity::Tx, DotId(unsafe { AnyId::new_unchecked(id) }))
    }

    fn new_fork(atoms: &mut AtomSpace, tip_id: usize, pit_size: usize) -> ForkId {
        let arm_ids = (tip_id + 1..=tip_id + pit_size)
            .map(|id| DotId(unsafe { AnyId::new_unchecked(id) }));
        let pit = Dotset::new(arm_ids);
        atoms.share_fork_from_tip_and_pit(DotId(unsafe { AnyId::new_unchecked(tip_id) }), pit)
    }

    fn new_dotset(first_id: usize, size: usize) -> Dotset {
        let dot_ids =
            (first_id..first_id + size).map(|id| DotId(unsafe { AnyId::new_unchecked(id) }));
        Dotset::new(dot_ids)
    }

    #[test]
    #[should_panic(expected = "uninitialized")]
    fn test_atom_uninitialized() {
        let atom = Atom::Tx(new_tx_port(1));
        let _ = atom.get_atom_id().expect("uninitialized");
    }

    #[test]
    #[should_panic(expected = "bottom")]
    fn test_atom_bottom() {
        let mut atoms = AtomSpace::default();
        let atom = Atom::Bottom;
        let _ = atoms.do_share_atom(atom);
    }

    #[test]
    #[should_panic(expected = "reset")]
    fn test_atom_reset_id() {
        let mut atoms = AtomSpace::default();
        let mut atom = Atom::Tx(new_tx_port(1));
        atom.set_atom_id(unsafe { AtomId::new_unchecked(1) });
        let _ = atoms.do_share_atom(atom);
    }

    #[test]
    fn test_atom_id() {
        let mut atoms = AtomSpace::default();
        let atom = Atom::Tx(new_tx_port(1));
        let atom_id = atoms.do_share_atom(atom);
        let atom = atoms.get_atom(atom_id).unwrap();
        assert_eq!(atom.get_atom_id().unwrap(), atom_id);
    }

    #[test]
    fn test_fork_resharing() {
        let mut atoms = AtomSpace::default();
        let f1_id = new_fork(&mut atoms, 1, 2);
        let f2_id = new_fork(&mut atoms, 1, 2);
        assert_eq!(f1_id, f2_id);
    }

    #[test]
    fn test_pit_resharing() {
        let mut atoms = AtomSpace::default();
        let s1 = Atom::Pit(new_dotset(1, 5));
        let s1_id = atoms.do_share_atom(s1);
        let s2 = Atom::Pit(new_dotset(1, 5));
        let s2_id = atoms.do_share_atom(s2);
        assert_eq!(s1_id, s2_id);
    }
}
