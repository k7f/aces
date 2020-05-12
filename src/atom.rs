use std::{
    fmt, hash,
    collections::{BTreeMap, HashMap},
};
use crate::{
    Face, AnyId, NodeId, Context, ContextHandle, Contextual, ExclusivelyContextual, InContext,
    AcesError, AcesErrorKind, sat,
    node::{NodeSet, NodeSetId},
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
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
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
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
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

/// A collection of [`Atom`]s: [`Port`]s, [`Link`]s, [`Fork`]s,
/// [`Join`]s and [`NodeSet`]s.
///
/// [`AtomSpace`] maintains a mapping from [`Atom`]s to [`AtomId`]s,
/// its inverse, and a mapping from [`NodeId`]s to [`PortId`]s.  For
/// the reverse mapping, from [`PortId`]s to [`NodeId`]s, call
/// [`AtomSpace::get_port()`] followed by [`Port::get_node_id()`].
#[derive(Clone, Debug)]
pub(crate) struct AtomSpace {
    atoms:          Vec<Atom>,
    atom_ids:       HashMap<Atom, AtomId>,
    source_nodes:   BTreeMap<NodeId, PortId>,
    sink_nodes:     BTreeMap<NodeId, PortId>,
    internal_nodes: BTreeMap<NodeId, (PortId, PortId)>,
}

impl Default for AtomSpace {
    fn default() -> Self {
        Self {
            atoms:          vec![Atom::Bottom],
            atom_ids:       Default::default(),
            source_nodes:   Default::default(),
            sink_nodes:     Default::default(),
            internal_nodes: Default::default(),
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
        let host = port.node_id;

        match port.face {
            Face::Tx => {
                let atom_id = self.do_share_atom(Atom::Tx(port.clone()));

                port.atom_id = Some(atom_id);

                let pid = PortId(atom_id);

                if let Some(&rx_id) = self.sink_nodes.get(&host) {
                    self.sink_nodes.remove(&host);
                    self.internal_nodes.insert(host, (pid, rx_id));
                } else {
                    self.source_nodes.insert(host, pid);
                }

                pid
            }
            Face::Rx => {
                let atom_id = self.do_share_atom(Atom::Rx(port.clone()));

                port.atom_id = Some(atom_id);

                let pid = PortId(atom_id);

                if let Some(&tx_id) = self.source_nodes.get(&host) {
                    self.source_nodes.remove(&host);
                    self.internal_nodes.insert(host, (tx_id, pid));
                } else {
                    self.sink_nodes.insert(host, pid);
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

    pub(crate) fn share_fork_from_host_and_suit(
        &mut self,
        host_id: NodeId,
        mut suit: NodeSet,
    ) -> ForkId {
        let suit_id = self.share_node_set(&mut suit);
        let mut fork = Harc::new(Face::Tx, host_id, suit_id);

        self.share_fork(&mut fork)
    }

    pub(crate) fn share_join_from_host_and_suit(
        &mut self,
        host_id: NodeId,
        mut suit: NodeSet,
    ) -> JoinId {
        let suit_id = self.share_node_set(&mut suit);
        let mut join = Harc::new(Face::Rx, host_id, suit_id);

        self.share_join(&mut join)
    }

    #[inline]
    pub(crate) fn share_node_set(&mut self, mono: &mut NodeSet) -> NodeSetId {
        let atom_id = self.do_share_atom(Atom::Mono(mono.clone()));

        mono.atom_id = Some(atom_id);

        NodeSetId(atom_id)
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
    pub(crate) fn is_harc(&self, atom_id: AtomId) -> bool {
        match self.get_atom(atom_id) {
            Some(Atom::Fork(_)) | Some(Atom::Join(_)) => true,
            _ => false,
        }
    }

    #[inline]
    pub(crate) fn get_harc(&self, aid: AtomId) -> Option<&Harc> {
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
    pub(crate) fn is_node_set(&self, atom_id: AtomId) -> bool {
        match self.get_atom(atom_id) {
            Some(Atom::Mono(_)) => true,
            _ => false,
        }
    }

    #[inline]
    pub(crate) fn get_node_set(&self, mid: NodeSetId) -> Option<&NodeSet> {
        match self.get_atom(mid.into()) {
            Some(Atom::Mono(a)) => Some(a),
            _ => None,
        }
    }

    pub fn get_anti_port_id(&self, pid: PortId) -> Option<PortId> {
        if let Some(port) = self.get_port(pid) {
            if let Some(&(tx_id, rx_id)) = self.internal_nodes.get(&port.node_id) {
                match port.face {
                    Face::Tx => {
                        if tx_id == pid {
                            return Some(rx_id)
                        } else {
                            panic!("Corrupt atom space")
                        }
                    }
                    Face::Rx => {
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
    Mono(NodeSet),
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
            Mono(m) => &mut m.atom_id,
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
            Mono(m) => m.atom_id,
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
            Mono(m) => if let Mono(o) = other { m == o } else { false },
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
            Mono(m) => m.hash(state),
            Bottom => panic!("Attempt to access the bottom atom"),
        }
    }
}

/// Representation of a port.
///
/// This is one of the two [`Face`]s of a node.
#[derive(Clone, Eq, Debug)]
pub struct Port {
    face:    Face,
    atom_id: Option<AtomId>,
    node_id: NodeId,
}

impl Port {
    pub(crate) fn new(face: Face, node_id: NodeId) -> Self {
        Self { face, atom_id: None, node_id }
    }

    pub(crate) fn get_face(&self) -> Face {
        self.face
    }

    pub fn get_atom_id(&self) -> AtomId {
        self.atom_id.expect("Attempt to access an uninitialized port")
    }

    pub fn get_node_id(&self) -> NodeId {
        self.node_id
    }
}

impl PartialEq for Port {
    fn eq(&self, other: &Self) -> bool {
        self.node_id == other.node_id
    }
}

impl hash::Hash for Port {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.face.hash(state);
        self.node_id.hash(state);
    }
}

impl ExclusivelyContextual for Port {
    fn format_locked(&self, ctx: &Context) -> Result<String, AcesError> {
        let node_name = ctx
            .get_node_name(self.get_node_id())
            .ok_or_else(|| AcesError::from(AcesErrorKind::NodeMissingForPort(self.get_face())))?;

        Ok(format!("[{} {}]", node_name, self.get_face()))
    }
}

/// Representation of a link.
///
/// This is a fat link, if used on its own, or a thin link, if paired
/// with a [`Face`].  See [`CEStructure`]'s private field `links`, or
/// the implementation of [`CEStructure::check_coherence()`].
///
/// [`CEStructure`]: crate::CEStructure
/// [`CEStructure::check_coherence()`]: crate::CEStructure::check_coherence()
#[derive(Clone, Eq, Debug)]
pub struct Link {
    atom_id:    Option<AtomId>,
    tx_port_id: PortId,
    tx_node_id: NodeId,
    rx_port_id: PortId,
    rx_node_id: NodeId,
}

impl Link {
    pub fn new(
        tx_port_id: PortId,
        tx_node_id: NodeId,
        rx_port_id: PortId,
        rx_node_id: NodeId,
    ) -> Self {
        Self { atom_id: None, tx_port_id, tx_node_id, rx_port_id, rx_node_id }
    }

    pub fn get_atom_id(&self) -> AtomId {
        self.atom_id.expect("Attempt to access an uninitialized link")
    }

    pub fn get_link_id(&self) -> LinkId {
        LinkId(self.get_atom_id())
    }

    pub fn get_port_id(&self, face: Face) -> PortId {
        if face == Face::Rx {
            self.rx_port_id
        } else {
            self.tx_port_id
        }
    }

    pub fn get_node_id(&self, face: Face) -> NodeId {
        if face == Face::Rx {
            self.rx_node_id
        } else {
            self.tx_node_id
        }
    }

    pub fn get_tx_port_id(&self) -> PortId {
        self.tx_port_id
    }

    pub fn get_tx_node_id(&self) -> NodeId {
        self.tx_node_id
    }

    pub fn get_rx_port_id(&self) -> PortId {
        self.rx_port_id
    }

    pub fn get_rx_node_id(&self) -> NodeId {
        self.rx_node_id
    }
}

impl PartialEq for Link {
    fn eq(&self, other: &Self) -> bool {
        self.tx_port_id == other.tx_port_id && self.rx_node_id == other.rx_node_id
    }
}

impl hash::Hash for Link {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.tx_port_id.hash(state);
        self.tx_node_id.hash(state);
        self.rx_port_id.hash(state);
        self.rx_node_id.hash(state);
    }
}

impl ExclusivelyContextual for Link {
    fn format_locked(&self, ctx: &Context) -> Result<String, AcesError> {
        let tx_node_name = ctx
            .get_node_name(self.get_tx_node_id())
            .ok_or_else(|| AcesError::from(AcesErrorKind::NodeMissingForLink(Face::Tx)))?;
        let rx_node_name = ctx
            .get_node_name(self.get_rx_node_id())
            .ok_or_else(|| AcesError::from(AcesErrorKind::NodeMissingForLink(Face::Rx)))?;

        Ok(format!("({} > {})", tx_node_name, rx_node_name))
    }
}

/// A common type of one-to-many and many-to-one arcs of the
/// BF-hypergraph representation of c-e structures.
///
/// A hyperarc represents a monomial attached to a node.  There are
/// two possible interpretations of a `Harc`: a [`Join`] is a B-arc
/// which represents causes and a [`Fork`] is an F-arc representing
/// effects.
#[derive(Clone, Eq)]
pub struct Harc {
    atom_id: Option<AtomId>,
    face:    Face,
    host_id: NodeId,
    suit_id: NodeSetId,
}

impl Harc {
    pub(crate) fn new(face: Face, host_id: NodeId, suit_id: NodeSetId) -> Self {
        Harc { atom_id: None, face, host_id, suit_id }
    }

    /// [`Fork`]'s constructor.
    ///
    /// See also  [`Harc::new_fork_unchecked()`].
    pub fn new_fork<I>(ctx: &ContextHandle, host_id: NodeId, suit_ids: I) -> ForkId
    where
        I: IntoIterator<Item = NodeId>,
    {
        let suit = NodeSet::new(suit_ids);
        trace!("New fork: {:?} -> {:?}", host_id, suit);
        ctx.lock().unwrap().share_fork_from_host_and_suit(host_id, suit)
    }

    /// [`Join`]'s constructor.
    ///
    /// See also  [`Harc::new_join_unchecked()`].
    pub fn new_join<I>(ctx: &ContextHandle, host_id: NodeId, suit_ids: I) -> JoinId
    where
        I: IntoIterator<Item = NodeId>,
    {
        let suit = NodeSet::new(suit_ids);
        trace!("New join: {:?} <- {:?}", host_id, suit);
        ctx.lock().unwrap().share_join_from_host_and_suit(host_id, suit)
    }

    /// A more efficient variant of [`Harc::new_fork()`].
    ///
    /// Note: new [`Fork`] is created under the assumption that
    /// `suit_ids` are nonempty and listed in ascending order.  If the
    /// caller fails to provide an ordered suit, the library may panic
    /// in some other call (the constructor itself panics immediately
    /// in debug mode).
    #[inline]
    pub fn new_fork_unchecked<I>(ctx: &ContextHandle, host_id: NodeId, suit_ids: I) -> ForkId
    where
        I: IntoIterator<Item = NodeId>,
    {
        let suit = NodeSet::new_unchecked(suit_ids);
        trace!("New fork: {:?} -> {:?}", host_id, suit);
        ctx.lock().unwrap().share_fork_from_host_and_suit(host_id, suit)
    }

    /// A more efficient variant of [`Harc::new_join()`].
    ///
    /// Note: new [`Join`] is created under the assumption that
    /// `suit_ids` are nonempty and listed in ascending order.  If the
    /// caller fails to provide an ordered suit, the library may panic
    /// in some other call (the constructor itself panics immediately
    /// in debug mode).
    #[inline]
    pub fn new_join_unchecked<I>(ctx: &ContextHandle, host_id: NodeId, suit_ids: I) -> JoinId
    where
        I: IntoIterator<Item = NodeId>,
    {
        let suit = NodeSet::new_unchecked(suit_ids);
        trace!("New join: {:?} <- {:?}", host_id, suit);
        ctx.lock().unwrap().share_join_from_host_and_suit(host_id, suit)
    }

    #[inline]
    pub fn get_atom_id(&self) -> AtomId {
        match self.face {
            Face::Tx => self.atom_id.expect("Attempt to access an uninitialized fork"),
            Face::Rx => self.atom_id.expect("Attempt to access an uninitialized join"),
        }
    }

    #[inline]
    pub fn get_fork_id(&self) -> Option<ForkId> {
        match self.face {
            Face::Tx => Some(ForkId(self.get_atom_id())),
            Face::Rx => None,
        }
    }

    #[inline]
    pub fn get_join_id(&self) -> Option<JoinId> {
        match self.face {
            Face::Tx => None,
            Face::Rx => Some(JoinId(self.get_atom_id())),
        }
    }

    #[inline]
    pub fn get_face(&self) -> Face {
        self.face
    }

    #[inline]
    pub fn get_host_id(&self) -> NodeId {
        self.host_id
    }

    #[inline]
    pub fn get_suit_id(&self) -> NodeSetId {
        self.suit_id
    }
}

impl fmt::Debug for Harc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {{ atom_id: ",
            match self.face {
                Face::Tx => "Fork",
                Face::Rx => "Join",
            }
        )?;
        self.atom_id.fmt(f)?;
        write!(f, ", host_id: ")?;
        self.host_id.fmt(f)?;
        write!(f, ", suit_id: ")?;
        self.suit_id.fmt(f)?;
        write!(f, " }}")
    }
}

impl PartialEq for Harc {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.face == other.face && self.host_id == other.host_id && self.suit_id == other.suit_id
    }
}

impl hash::Hash for Harc {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.face.hash(state);
        self.host_id.hash(state);
        self.suit_id.hash(state);
    }
}

impl ExclusivelyContextual for Harc {
    fn format_locked(&self, ctx: &Context) -> Result<String, AcesError> {
        let host_name = ctx.get_node_name(self.host_id).ok_or(match self.face {
            Face::Tx => AcesError::from(AcesErrorKind::NodeMissingForFork(Face::Tx)),
            Face::Rx => AcesError::from(AcesErrorKind::NodeMissingForJoin(Face::Rx)),
        })?;

        let suit = ctx
            .get_node_set(self.suit_id)
            .ok_or_else(|| AcesError::from(AcesErrorKind::NodeSetMissingForId(self.suit_id)))?;

        let suit_names: Result<Vec<_>, AcesError> = suit
            .get_node_ids()
            .iter()
            .map(|&node_id| {
                ctx.get_node_name(node_id).ok_or(match self.face {
                    Face::Tx => AcesError::from(AcesErrorKind::NodeMissingForFork(Face::Rx)),
                    Face::Rx => AcesError::from(AcesErrorKind::NodeMissingForJoin(Face::Tx)),
                })
            })
            .collect();

        match self.face {
            Face::Tx => Ok(format!("({} > {:?})", host_name, suit_names?)),
            Face::Rx => Ok(format!("({:?} > {})", suit_names?, host_name)),
        }
    }
}

/// Forward hyperarc representation of effects.
pub type Fork = Harc;

/// Backward hyperarc representation of causes.
pub type Join = Harc;

/// A trait of an identifier convertible into [`NodeId`] and into
/// [`sat::Literal`].
pub trait Atomic:
    From<AtomId> + Into<AtomId> + Contextual + Copy + PartialEq + Eq + PartialOrd + Ord
{
    fn into_node_id(this: InContext<Self>) -> Option<NodeId>;

    fn into_node_id_docked(this: InContext<Self>, _dock: Face) -> Option<NodeId> {
        Self::into_node_id(this)
    }

    fn into_sat_literal(self, negated: bool) -> sat::Literal;
}

impl Atomic for PortId {
    fn into_node_id(this: InContext<Self>) -> Option<NodeId> {
        this.using_context(|pid, ctx| ctx.get_port(*pid).map(|port| port.get_node_id()))
    }

    #[inline]
    fn into_sat_literal(self, negated: bool) -> sat::Literal {
        sat::Literal::from_atom_id(self.get(), negated)
    }
}

impl Atomic for LinkId {
    fn into_node_id(_this: InContext<Self>) -> Option<NodeId> {
        None
    }

    fn into_node_id_docked(this: InContext<Self>, dock: Face) -> Option<NodeId> {
        this.using_context(|lid, ctx| ctx.get_link(*lid).map(|link| link.get_node_id(dock)))
    }

    #[inline]
    fn into_sat_literal(self, negated: bool) -> sat::Literal {
        sat::Literal::from_atom_id(self.get(), negated)
    }
}

impl Atomic for ForkId {
    fn into_node_id(this: InContext<Self>) -> Option<NodeId> {
        this.using_context(|fid, ctx| ctx.get_fork(*fid).map(|fork| fork.get_host_id()))
    }

    #[inline]
    fn into_sat_literal(self, negated: bool) -> sat::Literal {
        sat::Literal::from_atom_id(self.get(), negated)
    }
}

impl Atomic for JoinId {
    fn into_node_id(this: InContext<Self>) -> Option<NodeId> {
        this.using_context(|jid, ctx| ctx.get_join(*jid).map(|join| join.get_host_id()))
    }

    #[inline]
    fn into_sat_literal(self, negated: bool) -> sat::Literal {
        sat::Literal::from_atom_id(self.get(), negated)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn new_tx_port(id: usize) -> Port {
        Port::new(Face::Tx, NodeId(unsafe { AnyId::new_unchecked(id) }))
    }

    fn new_fork(atoms: &mut AtomSpace, host_id: usize, suit_size: usize) -> ForkId {
        let suit_ids = (host_id + 1..=host_id + suit_size)
            .map(|id| NodeId(unsafe { AnyId::new_unchecked(id) }));
        let suit = NodeSet::new(suit_ids);
        atoms.share_fork_from_host_and_suit(NodeId(unsafe { AnyId::new_unchecked(host_id) }), suit)
    }

    fn new_node_set(first_id: usize, size: usize) -> NodeSet {
        let node_ids =
            (first_id..first_id + size).map(|id| NodeId(unsafe { AnyId::new_unchecked(id) }));
        NodeSet::new(node_ids)
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
    fn test_mono_resharing() {
        let mut atoms = AtomSpace::default();
        let m1 = Atom::Mono(new_node_set(1, 5));
        let m1_id = atoms.do_share_atom(m1);
        let m2 = Atom::Mono(new_node_set(1, 5));
        let m2_id = atoms.do_share_atom(m2);
        assert_eq!(m1_id, m2_id);
    }
}
