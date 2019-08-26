use std::{cmp, collections::BTreeMap, error::Error};
use crate::{ID, NodeID, Context, Contextual, InContext, node, monomial, sat, error::AcesError};

/// An abstract structural identifier serving as the common base of
/// [`PortID`], [`LinkID`], [`ForkID`] and [`JoinID`].
///
/// Since this is a numeric identifier, which is serial and one-based,
/// it trivially maps into numeric codes of variables in the DIMACS
/// SAT format.
///
/// See [`ID`] for more details.
pub(crate) type AtomID = ID;

/// An identifier of a [`Port`], a type derived from [`AtomID`].
///
/// There is a trivial bijection between values of this type and
/// numeric codes of DIMACS variables.  This mapping simplifies the
/// construction of SAT queries and interpretation of solutions.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(transparent)]
pub struct PortID(pub(crate) AtomID);

impl PortID {
    #[inline]
    pub const fn get(self) -> AtomID {
        self.0
    }
}

impl From<PortID> for AtomID {
    fn from(id: PortID) -> Self {
        id.0
    }
}

impl Contextual for PortID {
    fn format(&self, ctx: &Context, dock: Option<node::Face>) -> Result<String, Box<dyn Error>> {
        let port = ctx.get_port(*self).ok_or(AcesError::PortMissingForID)?;
        port.format(ctx, dock)
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

impl From<LinkID> for AtomID {
    fn from(id: LinkID) -> Self {
        id.0
    }
}

impl Contextual for LinkID {
    fn format(&self, ctx: &Context, dock: Option<node::Face>) -> Result<String, Box<dyn Error>> {
        let link = ctx.get_link(*self).ok_or(AcesError::LinkMissingForID)?;
        link.format(ctx, dock)
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

impl From<ForkID> for AtomID {
    fn from(id: ForkID) -> Self {
        id.0
    }
}

impl Contextual for ForkID {
    fn format(&self, ctx: &Context, dock: Option<node::Face>) -> Result<String, Box<dyn Error>> {
        let fork = ctx.get_fork(*self).ok_or(AcesError::ForkMissingForID)?;
        fork.format(ctx, dock)
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

impl From<JoinID> for AtomID {
    fn from(id: JoinID) -> Self {
        id.0
    }
}

impl Contextual for JoinID {
    fn format(&self, ctx: &Context, dock: Option<node::Face>) -> Result<String, Box<dyn Error>> {
        let join = ctx.get_join(*self).ok_or(AcesError::JoinMissingForID)?;
        join.format(ctx, dock)
    }
}

/// A collection of [`Atom`]s ([`PortID`]s, [`LinkID`]s, [`ForkID`]s
/// and [`JoinID`]s) and a mapping from [`NodeID`]s to [`PortID`]s.
///
/// For the reverse mapping, from [`PortID`]s to [`NodeID`]s, call
/// [`AtomSpace::get_port()`] followed by [`Port::get_node_id()`].
#[derive(Clone, Debug)]
pub(crate) struct AtomSpace {
    atoms:          Vec<Atom>,
    source_nodes:   BTreeMap<NodeID, PortID>,
    sink_nodes:     BTreeMap<NodeID, PortID>,
    internal_nodes: BTreeMap<NodeID, (PortID, PortID)>,
}

impl Default for AtomSpace {
    fn default() -> Self {
        Self {
            atoms:          vec![Atom::Bottom],
            source_nodes:   Default::default(),
            sink_nodes:     Default::default(),
            internal_nodes: Default::default(),
        }
    }
}

impl AtomSpace {
    fn do_share_atom(&mut self, mut new_atom: Atom) -> AtomID {
        for old_atom in self.atoms.iter() {
            if old_atom == &new_atom {
                if new_atom.do_get_atom_id().is_some() {
                    panic!("Attempt to reset ID of atom {:?}", new_atom);
                }
                return old_atom.get_atom_id()
            }
        }

        let atom_id = unsafe { AtomID::new_unchecked(self.atoms.len()) };
        new_atom.set_atom_id(atom_id);
        self.atoms.push(new_atom);

        atom_id
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
    pub(crate) fn get_atom_mut(&mut self, atom_id: AtomID) -> Option<&mut Atom> {
        self.atoms.get_mut(atom_id.get())
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
    pub(crate) fn get_port_mut(&mut self, pid: PortID) -> Option<&mut Port> {
        match self.get_atom_mut(pid.into()) {
            Some(Atom::Tx(a)) => Some(a),
            Some(Atom::Rx(a)) => Some(a),
            _ => None,
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
    pub(crate) fn get_link_mut(&mut self, lid: LinkID) -> Option<&mut Link> {
        match self.get_atom_mut(lid.into()) {
            Some(Atom::Link(a)) => Some(a),
            _ => None,
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
    pub(crate) fn get_fork_mut(&mut self, fid: ForkID) -> Option<&mut Fork> {
        match self.get_atom_mut(fid.into()) {
            Some(Atom::Fork(a)) => Some(a),
            _ => None,
        }
    }

    #[inline]
    pub(crate) fn get_join(&self, jid: JoinID) -> Option<&Join> {
        match self.get_atom(jid.into()) {
            Some(Atom::Join(a)) => Some(a),
            _ => None,
        }
    }

    #[inline]
    pub(crate) fn get_join_mut(&mut self, jid: JoinID) -> Option<&mut Join> {
        match self.get_atom_mut(jid.into()) {
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

#[derive(Clone, Debug)]
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

    fn do_get_atom_id(&self) -> Option<AtomID> {
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

    pub fn get_atom_id(&self) -> AtomID {
        self.do_get_atom_id().expect("Attempt to access an uninitialized atom")
    }
}

impl cmp::PartialEq for Atom {
    #[rustfmt::skip]
    fn eq(&self, other: &Self) -> bool {
        use Atom::*;

        match self {
            Tx(p) => if let Tx(o) = other { p == o } else { false },
            Rx(p) => if let Rx(o) = other { p == o } else { false },
            Link(l) => if let Link(o) = other { l == o } else { false },
            Fork(f) => if let Fork(o) = other { f == o } else { false },
            Join(j) => if let Join(o) = other { j == o } else { false },
            Bottom => false, // irreflexive, hence no `impl cmp::Eq for Atom`
        }
    }
}

#[derive(Clone, Debug)]
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

impl cmp::PartialEq for Port {
    fn eq(&self, other: &Self) -> bool {
        self.node_id == other.node_id
    }
}

impl cmp::Eq for Port {}

impl Contextual for Port {
    fn format(&self, ctx: &Context, _dock: Option<node::Face>) -> Result<String, Box<dyn Error>> {
        let node_name = ctx
            .get_node_name(self.get_node_id())
            .ok_or_else(|| AcesError::NodeMissingForPort(self.get_face()))?;

        Ok(format!("[{} {}]", node_name, self.get_face()))
    }
}

#[derive(Clone, Debug)]
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

impl cmp::PartialEq for Link {
    fn eq(&self, other: &Self) -> bool {
        self.tx_port_id == other.tx_port_id && self.rx_node_id == other.rx_node_id
    }
}

impl cmp::Eq for Link {}

impl Contextual for Link {
    fn format(&self, ctx: &Context, _dock: Option<node::Face>) -> Result<String, Box<dyn Error>> {
        let tx_node_name = ctx
            .get_node_name(self.get_tx_node_id())
            .ok_or(AcesError::NodeMissingForLink(node::Face::Tx))?;
        let rx_node_name = ctx
            .get_node_name(self.get_rx_node_id())
            .ok_or(AcesError::NodeMissingForLink(node::Face::Rx))?;

        Ok(format!("({} > {})", tx_node_name, rx_node_name))
    }
}

#[derive(Clone, Debug)]
pub struct Fork {
    atom_id:     Option<AtomID>,
    tx_node_id:  NodeID,
    rx_node_ids: Vec<NodeID>,
    weight:      monomial::Weight,
}

impl Fork {
    pub fn new(
        tx_node_id: NodeID,
        rx_node_ids: Vec<NodeID>, // FIXME
        weight: monomial::Weight,
    ) -> Self {
        Self { atom_id: None, tx_node_id, rx_node_ids, weight }
    }

    pub fn get_atom_id(&self) -> AtomID {
        self.atom_id.expect("Attempt to access an uninitialized fork")
    }

    pub fn get_fork_id(&self) -> ForkID {
        ForkID(self.get_atom_id())
    }

    pub fn get_tx_node_id(&self) -> NodeID {
        self.tx_node_id
    }

    pub fn get_rx_node_ids(&self) -> &[NodeID] {
        self.rx_node_ids.as_slice()
    }

    pub fn get_weight(&self) -> monomial::Weight {
        self.weight
    }
}

impl cmp::PartialEq for Fork {
    fn eq(&self, other: &Self) -> bool {
        let result = self.tx_node_id == other.tx_node_id && self.rx_node_ids == other.rx_node_ids;

        // FIXME
        if result && self.weight != other.weight {
            panic!("Fork weight mismatch")
        }

        result
    }
}

impl cmp::Eq for Fork {}

impl Contextual for Fork {
    fn format(&self, ctx: &Context, _dock: Option<node::Face>) -> Result<String, Box<dyn Error>> {
        let tx_node_name = ctx
            .get_node_name(self.get_tx_node_id())
            .ok_or(AcesError::NodeMissingForFork(node::Face::Tx))?;

        let rx_node_names: Result<Vec<_>, AcesError> = self
            .get_rx_node_ids()
            .iter()
            .map(|&node_id| {
                ctx.get_node_name(node_id).ok_or(AcesError::NodeMissingForFork(node::Face::Rx))
            })
            .collect();

        Ok(format!("({} > {:?})", tx_node_name, rx_node_names?))
    }
}

#[derive(Clone, Debug)]
pub struct Join {
    atom_id:     Option<AtomID>,
    tx_node_ids: Vec<NodeID>,
    rx_node_id:  NodeID,
    weight:      monomial::Weight,
}

impl Join {
    pub fn new(
        tx_node_ids: Vec<NodeID>, // FIXME
        rx_node_id: NodeID,
        weight: monomial::Weight,
    ) -> Self {
        Self { atom_id: None, tx_node_ids, rx_node_id, weight }
    }

    pub fn get_atom_id(&self) -> AtomID {
        self.atom_id.expect("Attempt to access an uninitialized join")
    }

    pub fn get_join_id(&self) -> JoinID {
        JoinID(self.get_atom_id())
    }

    pub fn get_tx_node_ids(&self) -> &[NodeID] {
        self.tx_node_ids.as_slice()
    }

    pub fn get_rx_node_id(&self) -> NodeID {
        self.rx_node_id
    }

    pub fn get_weight(&self) -> monomial::Weight {
        self.weight
    }
}

impl cmp::PartialEq for Join {
    fn eq(&self, other: &Self) -> bool {
        let result = self.tx_node_ids == other.tx_node_ids && self.rx_node_id == other.rx_node_id;

        // FIXME
        if result && self.weight != other.weight {
            panic!("Join weight mismatch")
        }

        result
    }
}

impl cmp::Eq for Join {}

impl Contextual for Join {
    fn format(&self, ctx: &Context, _dock: Option<node::Face>) -> Result<String, Box<dyn Error>> {
        let tx_node_names: Result<Vec<_>, AcesError> = self
            .get_tx_node_ids()
            .iter()
            .map(|&node_id| {
                ctx.get_node_name(node_id).ok_or(AcesError::NodeMissingForJoin(node::Face::Tx))
            })
            .collect();

        let rx_node_name = ctx
            .get_node_name(self.get_rx_node_id())
            .ok_or(AcesError::NodeMissingForJoin(node::Face::Rx))?;

        Ok(format!("({:?} > {})", tx_node_names?, rx_node_name))
    }
}

/// A trait of an identifier convertible into [`NodeID`] and into
/// [`sat::Literal`].
pub trait Atomic: Contextual + Copy + PartialEq + Eq + PartialOrd + Ord {
    fn into_node_id(this: InContext<Self>) -> Option<NodeID>;

    fn into_sat_literal(self, negated: bool) -> sat::Literal;
}

impl Atomic for PortID {
    fn into_node_id(this: InContext<Self>) -> Option<NodeID> {
        let ctx = this.get_context();
        let pid = this.get_thing();

        ctx.get_port(*pid).map(|port| port.get_node_id())
    }

    fn into_sat_literal(self, negated: bool) -> sat::Literal {
        sat::Literal::from_atom_id(self.get(), negated)
    }
}

impl Atomic for LinkID {
    fn into_node_id(this: InContext<Self>) -> Option<NodeID> {
        let ctx = this.get_context();
        let dock = this.get_dock();
        let lid = this.get_thing();

        ctx.get_link(*lid).and_then(|link| dock.map(|face| link.get_node_id(face)))
    }

    fn into_sat_literal(self, negated: bool) -> sat::Literal {
        sat::Literal::from_atom_id(self.get(), negated)
    }
}

impl Atomic for ForkID {
    fn into_node_id(this: InContext<Self>) -> Option<NodeID> {
        let ctx = this.get_context();
        let fid = this.get_thing();

        ctx.get_fork(*fid).map(|fork| fork.get_tx_node_id())
    }

    fn into_sat_literal(self, negated: bool) -> sat::Literal {
        sat::Literal::from_atom_id(self.get(), negated)
    }
}

impl Atomic for JoinID {
    fn into_node_id(this: InContext<Self>) -> Option<NodeID> {
        let ctx = this.get_context();
        let jid = this.get_thing();

        ctx.get_join(*jid).map(|join| join.get_rx_node_id())
    }

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
        let _ = atom.get_atom_id();
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
        assert_eq!(atom.get_atom_id(), atom_id);
    }
}
