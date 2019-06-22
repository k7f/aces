use std::{cmp, collections::BTreeMap, error::Error};
use crate::{ID, NodeID, Context, Contextual, node, sat, error::AcesError};

/// An abstract structural identifier serving as the common base of
/// [`PortID`] and [`LinkID`].
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

    pub fn as_sat_literal(self, negated: bool) -> sat::Literal {
        sat::Literal::from_atom_id(self.get(), negated)
    }
}

impl From<PortID> for AtomID {
    fn from(id: PortID) -> Self {
        id.0
    }
}

impl Contextual for PortID {
    fn format(&self, ctx: &Context) -> Result<String, Box<dyn Error>> {
        let port = ctx.get_port(*self).ok_or(AcesError::PortMissingForID)?;
        port.format(ctx)
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

    pub fn as_sat_literal(self, negated: bool) -> sat::Literal {
        sat::Literal::from_atom_id(self.get(), negated)
    }
}

impl From<LinkID> for AtomID {
    fn from(id: LinkID) -> Self {
        id.0
    }
}

impl Contextual for LinkID {
    fn format(&self, ctx: &Context) -> Result<String, Box<dyn Error>> {
        let link = ctx.get_link(*self).ok_or(AcesError::LinkMissingForID)?;
        link.format(ctx)
    }
}

/// A collection of [`Atom`]s ([`PortID`]s and [`LinkID`]s) and a
/// mapping from [`NodeID`]s to [`PortID`]s.
///
/// For the reverse mapping, from [`PortID`]s to [`NodeID`]s, call
/// [`AtomSpace::get_port()`] followed by [`Port::get_node_id()`].
#[derive(Debug)]
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
        let node_id = port.node_id;

        match port.face {
            node::Face::Tx => {
                let atom_id = self.do_share_atom(Atom::Tx(port.clone()));

                port.atom_id = Some(atom_id);

                let port_id = PortID(atom_id);

                if let Some(&rx_id) = self.sink_nodes.get(&node_id) {
                    self.sink_nodes.remove(&node_id);
                    self.internal_nodes.insert(node_id, (port_id, rx_id));
                } else {
                    self.source_nodes.insert(node_id, port_id);
                }

                port_id
            }
            node::Face::Rx => {
                let atom_id = self.do_share_atom(Atom::Rx(port.clone()));

                port.atom_id = Some(atom_id);

                let port_id = PortID(atom_id);

                if let Some(&tx_id) = self.source_nodes.get(&node_id) {
                    self.source_nodes.remove(&node_id);
                    self.internal_nodes.insert(node_id, (tx_id, port_id));
                } else {
                    self.sink_nodes.insert(node_id, port_id);
                }

                port_id
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
    pub(crate) fn get_port(&self, port_id: PortID) -> Option<&Port> {
        match self.get_atom(port_id.into()) {
            Some(Atom::Tx(a)) => Some(a),
            Some(Atom::Rx(a)) => Some(a),
            _ => None,
        }
    }

    #[inline]
    pub(crate) fn get_port_mut(&mut self, port_id: PortID) -> Option<&mut Port> {
        match self.get_atom_mut(port_id.into()) {
            Some(Atom::Tx(a)) => Some(a),
            Some(Atom::Rx(a)) => Some(a),
            _ => None,
        }
    }

    #[inline]
    pub(crate) fn get_link(&self, link_id: LinkID) -> Option<&Link> {
        match self.get_atom(link_id.into()) {
            Some(Atom::Link(a)) => Some(a),
            _ => None,
        }
    }

    #[inline]
    pub(crate) fn get_link_mut(&mut self, link_id: LinkID) -> Option<&mut Link> {
        match self.get_atom_mut(link_id.into()) {
            Some(Atom::Link(a)) => Some(a),
            _ => None,
        }
    }

    pub fn get_antiport_id(&self, port_id: PortID) -> Option<PortID> {
        if let Some(port) = self.get_port(port_id) {
            if let Some(&(tx_id, rx_id)) = self.internal_nodes.get(&port.node_id) {
                match port.face {
                    node::Face::Tx => {
                        if tx_id == port_id {
                            return Some(rx_id)
                        } else {
                            panic!("Corrupt atom space")
                        }
                    }
                    node::Face::Rx => {
                        if rx_id == port_id {
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

#[derive(Debug)]
pub(crate) enum Atom {
    Tx(Port),
    Rx(Port),
    Link(Link),
    Bottom,
}

impl Atom {
    fn set_atom_id(&mut self, atom_id: AtomID) {
        use Atom::*;

        let prev_id = match self {
            Tx(p) => &mut p.atom_id,
            Rx(p) => &mut p.atom_id,
            Link(l) => &mut l.atom_id,
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

    pub fn as_sat_literal(&self, negated: bool) -> sat::Literal {
        sat::Literal::from_atom_id(self.get_atom_id(), negated)
    }
}

impl cmp::PartialEq for Port {
    fn eq(&self, other: &Self) -> bool {
        self.node_id == other.node_id
    }
}

impl cmp::Eq for Port {}

impl Contextual for Port {
    fn format(&self, ctx: &Context) -> Result<String, Box<dyn Error>> {
        let node_name = ctx
            .get_node_name(self.get_node_id())
            .ok_or(AcesError::NodeMissingForPort(node::Face::Tx))?;

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

    pub fn as_sat_literal(&self, negated: bool) -> sat::Literal {
        sat::Literal::from_atom_id(self.get_atom_id(), negated)
    }
}

impl cmp::PartialEq for Link {
    fn eq(&self, other: &Self) -> bool {
        self.tx_port_id == other.tx_port_id && self.rx_node_id == other.rx_node_id
    }
}

impl cmp::Eq for Link {}

impl Contextual for Link {
    fn format(&self, ctx: &Context) -> Result<String, Box<dyn Error>> {
        let tx_node_name = ctx
            .get_node_name(self.get_tx_node_id())
            .ok_or(AcesError::NodeMissingForLink(node::Face::Tx))?;
        let rx_node_name = ctx
            .get_node_name(self.get_rx_node_id())
            .ok_or(AcesError::NodeMissingForLink(node::Face::Rx))?;

        Ok(format!("({} > {})", tx_node_name, rx_node_name))
    }
}
