use std::{
    fmt, hash,
    collections::{BTreeMap, BTreeSet, HashMap},
    error::Error,
};
use crate::{
    ID, NodeID, Context, Contextual, ExclusivelyContextual, InContext, AcesError, AcesErrorKind,
    node, sat,
};

/// An abstract structural identifier serving as the common base of
/// [`PortID`], [`LinkID`], [`ForkID`] and [`JoinID`].
///
/// Since this is a numeric identifier, which is serial and one-based,
/// it trivially maps into numeric codes of variables in the DIMACS
/// SAT format.
///
/// See [`ID`] for more details.
pub type AtomID = ID;

/// An identifier of a [`Port`], a type derived from [`AtomID`].
///
/// There is a trivial bijection between values of this type and
/// numeric codes of DIMACS variables.  This mapping simplifies the
/// construction of SAT queries and interpretation of solutions.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct PortID(pub(crate) AtomID);

impl PortID {
    #[inline]
    pub const fn get(self) -> AtomID {
        self.0
    }
}

impl From<AtomID> for PortID {
    #[inline]
    fn from(id: AtomID) -> Self {
        PortID(id)
    }
}

impl From<PortID> for AtomID {
    #[inline]
    fn from(id: PortID) -> Self {
        id.0
    }
}

impl ExclusivelyContextual for PortID {
    fn format_locked(&self, ctx: &Context) -> Result<String, Box<dyn Error>> {
        let port = ctx
            .get_port(*self)
            .ok_or_else(|| AcesError::from(AcesErrorKind::PortMissingForID(*self)))?;
        port.format_locked(ctx)
    }
}

/// An identifier of a [`Link`], a type derived from [`AtomID`].
///
/// There is a trivial bijection between values of this type and
/// numeric codes of DIMACS variables.  This mapping simplifies the
/// construction of SAT queries and interpretation of solutions.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(transparent)]
pub struct LinkID(pub(crate) AtomID);

impl LinkID {
    #[inline]
    pub const fn get(self) -> AtomID {
        self.0
    }
}

impl From<AtomID> for LinkID {
    #[inline]
    fn from(id: AtomID) -> Self {
        LinkID(id)
    }
}

impl From<LinkID> for AtomID {
    #[inline]
    fn from(id: LinkID) -> Self {
        id.0
    }
}

impl ExclusivelyContextual for LinkID {
    fn format_locked(&self, ctx: &Context) -> Result<String, Box<dyn Error>> {
        let link = ctx
            .get_link(*self)
            .ok_or_else(|| AcesError::from(AcesErrorKind::LinkMissingForID(*self)))?;
        link.format_locked(ctx)
    }
}

/// An identifier of a [`Fork`], a type derived from [`AtomID`].
///
/// There is a trivial bijection between values of this type and
/// numeric codes of DIMACS variables.  This mapping simplifies the
/// construction of SAT queries and interpretation of solutions.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(transparent)]
pub struct ForkID(pub(crate) AtomID);

impl ForkID {
    #[inline]
    pub const fn get(self) -> AtomID {
        self.0
    }
}

impl From<AtomID> for ForkID {
    #[inline]
    fn from(id: AtomID) -> Self {
        ForkID(id)
    }
}

impl From<ForkID> for AtomID {
    #[inline]
    fn from(id: ForkID) -> Self {
        id.0
    }
}

impl ExclusivelyContextual for ForkID {
    fn format_locked(&self, ctx: &Context) -> Result<String, Box<dyn Error>> {
        let fork = ctx
            .get_fork(*self)
            .ok_or_else(|| AcesError::from(AcesErrorKind::ForkMissingForID(*self)))?;
        fork.format_locked(ctx)
    }
}

/// An identifier of a [`Join`], a type derived from [`AtomID`].
///
/// There is a trivial bijection between values of this type and
/// numeric codes of DIMACS variables.  This mapping simplifies the
/// construction of SAT queries and interpretation of solutions.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(transparent)]
pub struct JoinID(pub(crate) AtomID);

impl JoinID {
    #[inline]
    pub const fn get(self) -> AtomID {
        self.0
    }
}

impl From<AtomID> for JoinID {
    #[inline]
    fn from(id: AtomID) -> Self {
        JoinID(id)
    }
}

impl From<JoinID> for AtomID {
    #[inline]
    fn from(id: JoinID) -> Self {
        id.0
    }
}

impl ExclusivelyContextual for JoinID {
    fn format_locked(&self, ctx: &Context) -> Result<String, Box<dyn Error>> {
        let join = ctx
            .get_join(*self)
            .ok_or_else(|| AcesError::from(AcesErrorKind::JoinMissingForID(*self)))?;
        join.format_locked(ctx)
    }
}

/// A collection of [`Atom`]s: [`Port`]s, [`Link`]s, [`Fork`]s and
/// [`Join`]s.
///
/// [`AtomSpace`] maintains a mapping from [`Atom`]s to [`AtomID`]s,
/// its inverse, and a mapping from [`NodeID`]s to [`PortID`]s.  For
/// the reverse mapping, from [`PortID`]s to [`NodeID`]s, call
/// [`AtomSpace::get_port()`] followed by [`Port::get_node_id()`].
#[derive(Clone, Debug)]
pub(crate) struct AtomSpace {
    atoms:          Vec<Atom>,
    atom_ids:       HashMap<Atom, AtomID>,
    source_nodes:   BTreeMap<NodeID, PortID>,
    sink_nodes:     BTreeMap<NodeID, PortID>,
    internal_nodes: BTreeMap<NodeID, (PortID, PortID)>,
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
    fn do_share_atom(&mut self, mut new_atom: Atom) -> AtomID {
        if let Some(old_atom_id) = self.get_atom_id(&new_atom) {
            if new_atom.get_atom_id().is_none() {
                trace!("Resharing: {:?}", new_atom);

                old_atom_id
            } else {
                panic!("Attempt to reset ID of atom {:?}", new_atom);
            }
        } else {
            let atom_id = unsafe { AtomID::new_unchecked(self.atoms.len()) };
            new_atom.set_atom_id(atom_id);

            trace!("New share: {:?}", new_atom);

            self.atoms.push(new_atom.clone());
            self.atom_ids.insert(new_atom, atom_id);

            atom_id
        }
    }

    pub(crate) fn share_port(&mut self, port: &mut Port) -> PortID {
        let host = port.node_id;

        match port.face {
            node::Face::Tx => {
                let atom_id = self.do_share_atom(Atom::Tx(port.clone()));

                port.atom_id = Some(atom_id);

                let pid = PortID(atom_id);

                if let Some(&rx_id) = self.sink_nodes.get(&host) {
                    self.sink_nodes.remove(&host);
                    self.internal_nodes.insert(host, (pid, rx_id));
                } else {
                    self.source_nodes.insert(host, pid);
                }

                pid
            }
            node::Face::Rx => {
                let atom_id = self.do_share_atom(Atom::Rx(port.clone()));

                port.atom_id = Some(atom_id);

                let pid = PortID(atom_id);

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
    pub(crate) fn share_link(&mut self, link: &mut Link) -> LinkID {
        let atom_id = self.do_share_atom(Atom::Link(link.clone()));

        link.atom_id = Some(atom_id);

        LinkID(atom_id)
    }

    #[inline]
    pub(crate) fn share_fork(&mut self, fork: &mut Fork) -> ForkID {
        let atom_id = self.do_share_atom(Atom::Fork(fork.clone()));

        fork.atom_id = Some(atom_id);

        ForkID(atom_id)
    }

    #[inline]
    pub(crate) fn share_join(&mut self, join: &mut Join) -> JoinID {
        let atom_id = self.do_share_atom(Atom::Join(join.clone()));

        join.atom_id = Some(atom_id);

        JoinID(atom_id)
    }

    #[inline]
    pub(crate) fn get_atom(&self, atom_id: AtomID) -> Option<&Atom> {
        self.atoms.get(atom_id.get())
    }

    #[inline]
    pub(crate) fn get_atom_id(&self, atom: &Atom) -> Option<AtomID> {
        self.atom_ids.get(atom).copied()
    }

    #[inline]
    pub(crate) fn is_port(&self, atom_id: AtomID) -> bool {
        match self.get_atom(atom_id) {
            Some(Atom::Tx(_)) | Some(Atom::Rx(_)) => true,
            _ => false,
        }
    }

    #[inline]
    pub(crate) fn get_port(&self, pid: PortID) -> Option<&Port> {
        match self.get_atom(pid.into()) {
            Some(Atom::Tx(a)) => Some(a),
            Some(Atom::Rx(a)) => Some(a),
            _ => None,
        }
    }

    #[inline]
    pub(crate) fn is_link(&self, atom_id: AtomID) -> bool {
        match self.get_atom(atom_id) {
            Some(Atom::Link(_)) => true,
            _ => false,
        }
    }

    #[inline]
    pub(crate) fn get_link(&self, lid: LinkID) -> Option<&Link> {
        match self.get_atom(lid.into()) {
            Some(Atom::Link(a)) => Some(a),
            _ => None,
        }
    }

    #[inline]
    pub(crate) fn is_harc(&self, atom_id: AtomID) -> bool {
        match self.get_atom(atom_id) {
            Some(Atom::Fork(_)) | Some(Atom::Join(_)) => true,
            _ => false,
        }
    }

    #[inline]
    pub(crate) fn get_harc(&self, aid: AtomID) -> Option<&Harc> {
        match self.get_atom(aid) {
            Some(Atom::Fork(a)) => Some(a),
            Some(Atom::Join(a)) => Some(a),
            _ => None,
        }
    }

    #[inline]
    pub(crate) fn is_fork(&self, atom_id: AtomID) -> bool {
        match self.get_atom(atom_id) {
            Some(Atom::Fork(_)) => true,
            _ => false,
        }
    }

    #[inline]
    pub(crate) fn get_fork(&self, fid: ForkID) -> Option<&Fork> {
        match self.get_atom(fid.into()) {
            Some(Atom::Fork(a)) => Some(a),
            _ => None,
        }
    }

    #[inline]
    pub(crate) fn is_join(&self, atom_id: AtomID) -> bool {
        match self.get_atom(atom_id) {
            Some(Atom::Join(_)) => true,
            _ => false,
        }
    }

    #[inline]
    pub(crate) fn get_join(&self, jid: JoinID) -> Option<&Join> {
        match self.get_atom(jid.into()) {
            Some(Atom::Join(a)) => Some(a),
            _ => None,
        }
    }

    pub fn get_antiport_id(&self, pid: PortID) -> Option<PortID> {
        if let Some(port) = self.get_port(pid) {
            if let Some(&(tx_id, rx_id)) = self.internal_nodes.get(&port.node_id) {
                match port.face {
                    node::Face::Tx => {
                        if tx_id == pid {
                            return Some(rx_id)
                        } else {
                            panic!("Corrupt atom space")
                        }
                    }
                    node::Face::Rx => {
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
    Bottom,
}

impl Atom {
    fn set_atom_id(&mut self, atom_id: AtomID) {
        use Atom::*;

        let prev_id = match self {
            Tx(p) => &mut p.atom_id,
            Rx(p) => &mut p.atom_id,
            Link(l) => &mut l.atom_id,
            Fork(f) => &mut f.atom_id,
            Join(j) => &mut j.atom_id,
            Bottom => panic!("Attempt to set ID of the bottom atom"),
        };

        if *prev_id == None {
            *prev_id = Some(atom_id);
        } else {
            panic!("Attempt to reset ID of atom {:?}", self);
        }
    }

    fn get_atom_id(&self) -> Option<AtomID> {
        use Atom::*;

        match self {
            Tx(p) => p.atom_id,
            Rx(p) => p.atom_id,
            Link(l) => l.atom_id,
            Fork(f) => f.atom_id,
            Join(j) => j.atom_id,
            Bottom => panic!("Attempt to get ID of the bottom atom"),
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
            Bottom => panic!("Attempt to access the bottom atom"),
        }
    }
}

/// Representation of a port.
///
/// This is one of the two [`Face`]s of a node.
///
/// [`Face`]: node::Face
#[derive(Clone, Eq, Debug)]
pub struct Port {
    face:    node::Face,
    atom_id: Option<AtomID>,
    node_id: NodeID,
}

impl Port {
    pub(crate) fn new(face: node::Face, node_id: NodeID) -> Self {
        Self { face, atom_id: None, node_id }
    }

    pub(crate) fn get_face(&self) -> node::Face {
        self.face
    }

    pub fn get_atom_id(&self) -> AtomID {
        self.atom_id.expect("Attempt to access an uninitialized port")
    }

    pub fn get_node_id(&self) -> NodeID {
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
    fn format_locked(&self, ctx: &Context) -> Result<String, Box<dyn Error>> {
        let node_name = ctx
            .get_node_name(self.get_node_id())
            .ok_or_else(|| AcesError::from(AcesErrorKind::NodeMissingForPort(self.get_face())))?;

        Ok(format!("[{} {}]", node_name, self.get_face()))
    }
}

/// Representation of a link.
///
/// This is a fat link, if used on its own, or a thin link, if paired
/// with a [`node::Face`].  See [`CEStructure`]'s private field
/// `links`, or the implementation of
/// [`CEStructure::check_coherence()`].
///
/// [`CEStructure`]: crate::CEStructure
/// [`CEStructure::check_coherence()`]: crate::CEStructure::check_coherence()
#[derive(Clone, Eq, Debug)]
pub struct Link {
    atom_id:    Option<AtomID>,
    tx_port_id: PortID,
    tx_node_id: NodeID,
    rx_port_id: PortID,
    rx_node_id: NodeID,
}

impl Link {
    pub fn new(
        tx_port_id: PortID,
        tx_node_id: NodeID,
        rx_port_id: PortID,
        rx_node_id: NodeID,
    ) -> Self {
        Self { atom_id: None, tx_port_id, tx_node_id, rx_port_id, rx_node_id }
    }

    pub fn get_atom_id(&self) -> AtomID {
        self.atom_id.expect("Attempt to access an uninitialized link")
    }

    pub fn get_link_id(&self) -> LinkID {
        LinkID(self.get_atom_id())
    }

    pub fn get_port_id(&self, face: node::Face) -> PortID {
        if face == node::Face::Rx {
            self.rx_port_id
        } else {
            self.tx_port_id
        }
    }

    pub fn get_node_id(&self, face: node::Face) -> NodeID {
        if face == node::Face::Rx {
            self.rx_node_id
        } else {
            self.tx_node_id
        }
    }

    pub fn get_tx_port_id(&self) -> PortID {
        self.tx_port_id
    }

    pub fn get_tx_node_id(&self) -> NodeID {
        self.tx_node_id
    }

    pub fn get_rx_port_id(&self) -> PortID {
        self.rx_port_id
    }

    pub fn get_rx_node_id(&self) -> NodeID {
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
    fn format_locked(&self, ctx: &Context) -> Result<String, Box<dyn Error>> {
        let tx_node_name = ctx
            .get_node_name(self.get_tx_node_id())
            .ok_or_else(|| AcesError::from(AcesErrorKind::NodeMissingForLink(node::Face::Tx)))?;
        let rx_node_name = ctx
            .get_node_name(self.get_rx_node_id())
            .ok_or_else(|| AcesError::from(AcesErrorKind::NodeMissingForLink(node::Face::Rx)))?;

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
    atom_id:  Option<AtomID>,
    face:     node::Face,
    host_id:  NodeID,
    suit_ids: Vec<NodeID>,
}

impl Harc {
    fn new_unchecked(face: node::Face, host_id: NodeID, suit_ids: Vec<NodeID>) -> Self {
        if cfg!(debug_assertions) {
            let mut sit = suit_ids.iter();

            if let Some(nid) = sit.next() {
                let mut prev_nid = *nid;

                for &nid in sit {
                    assert!(prev_nid < nid, "Unordered suit");
                    prev_nid = nid;
                }
            } else {
                panic!("Empty suit")
            }
        }
        Harc { atom_id: None, face, host_id, suit_ids }
    }

    /// [`Fork`]'s constructor.
    ///
    /// See also  [`Harc::new_fork_unchecked()`].
    pub fn new_fork<I>(host_id: NodeID, suit_ids: I) -> Self
    where
        I: IntoIterator<Item = NodeID>,
    {
        let suit_ids: BTreeSet<_> = suit_ids.into_iter().collect();

        Self::new_fork_unchecked(host_id, suit_ids)
    }

    /// [`Join`]'s constructor.
    ///
    /// See also  [`Harc::new_join_unchecked()`].
    pub fn new_join<I>(host_id: NodeID, suit_ids: I) -> Self
    where
        I: IntoIterator<Item = NodeID>,
    {
        let suit_ids: BTreeSet<_> = suit_ids.into_iter().collect();

        Self::new_join_unchecked(host_id, suit_ids)
    }

    /// A more efficient variant of [`Harc::new_fork()`].
    ///
    /// Note: new [`Fork`] is created under the assumption that
    /// `suit_ids` are listed in ascending order.  If the caller fails
    /// to provide an ordered suit, the library may panic in some
    /// other call (the constructor itself panics immediately in debug
    /// mode).
    pub fn new_fork_unchecked<I>(host_id: NodeID, suit_ids: I) -> Self
    where
        I: IntoIterator<Item = NodeID>,
    {
        let suit_ids: Vec<_> = suit_ids.into_iter().collect();
        trace!("New fork: {:?} -> {:?}", host_id, suit_ids);
        Harc::new_unchecked(node::Face::Tx, host_id, suit_ids)
    }

    /// A more efficient variant of [`Harc::new_join()`].
    ///
    /// Note: new [`Join`] is created under the assumption that
    /// `suit_ids` are listed in ascending order.  If the caller fails
    /// to provide an ordered suit, the library may panic in some
    /// other call (the constructor itself panics immediately in debug
    /// mode).
    pub fn new_join_unchecked<I>(host_id: NodeID, suit_ids: I) -> Self
    where
        I: IntoIterator<Item = NodeID>,
    {
        let suit_ids: Vec<_> = suit_ids.into_iter().collect();
        trace!("New join: {:?} <- {:?}", host_id, suit_ids);
        Harc::new_unchecked(node::Face::Rx, host_id, suit_ids)
    }

    pub fn get_atom_id(&self) -> AtomID {
        match self.face {
            node::Face::Tx => self.atom_id.expect("Attempt to access an uninitialized fork"),
            node::Face::Rx => self.atom_id.expect("Attempt to access an uninitialized join"),
        }
    }

    pub fn get_fork_id(&self) -> Option<ForkID> {
        match self.face {
            node::Face::Tx => Some(ForkID(self.get_atom_id())),
            node::Face::Rx => None,
        }
    }

    pub fn get_join_id(&self) -> Option<JoinID> {
        match self.face {
            node::Face::Tx => None,
            node::Face::Rx => Some(JoinID(self.get_atom_id())),
        }
    }

    pub fn get_host_id(&self) -> NodeID {
        self.host_id
    }

    pub fn get_suit_ids(&self) -> &[NodeID] {
        self.suit_ids.as_slice()
    }
}

impl fmt::Debug for Harc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {{ atom_id: ",
            match self.face {
                node::Face::Tx => "Fork",
                node::Face::Rx => "Join",
            }
        )?;
        self.atom_id.fmt(f)?;
        write!(f, ", host_id: ")?;
        self.host_id.fmt(f)?;
        write!(f, ", suit_ids: ")?;
        self.suit_ids.fmt(f)?;
        write!(f, " }}")
    }
}

impl PartialEq for Harc {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.face == other.face && self.host_id == other.host_id && self.suit_ids == other.suit_ids
    }
}

impl hash::Hash for Harc {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.face.hash(state);
        self.host_id.hash(state);
        self.suit_ids.hash(state);
    }
}

impl ExclusivelyContextual for Harc {
    fn format_locked(&self, ctx: &Context) -> Result<String, Box<dyn Error>> {
        let host_name = ctx.get_node_name(self.get_host_id()).ok_or(match self.face {
            node::Face::Tx => AcesError::from(AcesErrorKind::NodeMissingForFork(node::Face::Tx)),
            node::Face::Rx => AcesError::from(AcesErrorKind::NodeMissingForJoin(node::Face::Rx)),
        })?;

        let suit_names: Result<Vec<_>, AcesError> = self
            .get_suit_ids()
            .iter()
            .map(|&node_id| {
                ctx.get_node_name(node_id).ok_or(match self.face {
                    node::Face::Tx => {
                        AcesError::from(AcesErrorKind::NodeMissingForFork(node::Face::Rx))
                    }
                    node::Face::Rx => {
                        AcesError::from(AcesErrorKind::NodeMissingForJoin(node::Face::Tx))
                    }
                })
            })
            .collect();

        match self.face {
            node::Face::Tx => Ok(format!("({} > {:?})", host_name, suit_names?)),
            node::Face::Rx => Ok(format!("({:?} > {})", suit_names?, host_name)),
        }
    }
}

/// Forward hyperarc representation of effects.
pub type Fork = Harc;

/// Backward hyperarc representation of causes.
pub type Join = Harc;

/// A trait of an identifier convertible into [`NodeID`] and into
/// [`sat::Literal`].
pub trait Atomic:
    From<AtomID> + Into<AtomID> + Contextual + Copy + PartialEq + Eq + PartialOrd + Ord
{
    fn into_node_id(this: InContext<Self>) -> Option<NodeID>;

    fn into_node_id_docked(this: InContext<Self>, _dock: node::Face) -> Option<NodeID> {
        Self::into_node_id(this)
    }

    fn into_sat_literal(self, negated: bool) -> sat::Literal;
}

impl Atomic for PortID {
    fn into_node_id(this: InContext<Self>) -> Option<NodeID> {
        this.using_context(|pid, ctx| ctx.get_port(*pid).map(|port| port.get_node_id()))
    }

    #[inline]
    fn into_sat_literal(self, negated: bool) -> sat::Literal {
        sat::Literal::from_atom_id(self.get(), negated)
    }
}

impl Atomic for LinkID {
    fn into_node_id(_this: InContext<Self>) -> Option<NodeID> {
        None
    }

    fn into_node_id_docked(this: InContext<Self>, dock: node::Face) -> Option<NodeID> {
        this.using_context(|lid, ctx| ctx.get_link(*lid).map(|link| link.get_node_id(dock)))
    }

    #[inline]
    fn into_sat_literal(self, negated: bool) -> sat::Literal {
        sat::Literal::from_atom_id(self.get(), negated)
    }
}

impl Atomic for ForkID {
    fn into_node_id(this: InContext<Self>) -> Option<NodeID> {
        this.using_context(|fid, ctx| ctx.get_fork(*fid).map(|fork| fork.get_host_id()))
    }

    #[inline]
    fn into_sat_literal(self, negated: bool) -> sat::Literal {
        sat::Literal::from_atom_id(self.get(), negated)
    }
}

impl Atomic for JoinID {
    fn into_node_id(this: InContext<Self>) -> Option<NodeID> {
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
        Port::new(node::Face::Tx, NodeID(unsafe { ID::new_unchecked(id) }))
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
        atom.set_atom_id(unsafe { AtomID::new_unchecked(1) });
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
}
