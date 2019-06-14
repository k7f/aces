use std::{cmp, fmt, collections::BTreeMap};
use crate::{ID, NodeID, sat};

pub(crate) type AtomID = ID;

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

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Face {
    Tx,
    Rx,
}

impl std::ops::Not for Face {
    type Output = Face;

    fn not(self) -> Self::Output {
        match self {
            Face::Tx => Face::Rx,
            Face::Rx => Face::Tx,
        }
    }
}

impl fmt::Display for Face {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Face::Tx => write!(f, ">"),
            Face::Rx => write!(f, "<"),
        }
    }
}

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
    pub(crate) fn take_atom(&mut self, mut atom: Atom) -> AtomID {
        for old_atom in self.atoms.iter() {
            if old_atom == &atom {
                return old_atom.get_atom_id()
            }
        }

        let atom_id = unsafe { AtomID::new_unchecked(self.atoms.len()) };
        atom.set_atom_id(atom_id);
        self.atoms.push(atom);

        atom_id
    }

    pub(crate) fn take_port(&mut self, port: Port) -> PortID {
        let node_id = port.node_id;

        match port.face {
            Face::Tx => {
                let port_id = PortID(self.take_atom(Atom::Tx(port)));

                if let Some(&rx_id) = self.sink_nodes.get(&node_id) {
                    self.sink_nodes.remove(&node_id);
                    self.internal_nodes.insert(node_id, (port_id, rx_id));
                } else {
                    self.source_nodes.insert(node_id, port_id);
                }

                port_id
            }
            Face::Rx => {
                let port_id = PortID(self.take_atom(Atom::Rx(port)));

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
    pub(crate) fn take_link(&mut self, link: Link) -> LinkID {
        LinkID(self.take_atom(Atom::Link(link)))
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
                    Face::Tx => {
                        if tx_id == port_id {
                            return Some(rx_id)
                        } else {
                            panic!("Corrupt atom space")
                        }
                    }
                    Face::Rx => {
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
            Tx(a) => &mut a.atom_id,
            Rx(a) => &mut a.atom_id,
            Link(a) => &mut a.atom_id,
            Bottom => panic!("Attempt to set ID of the bottom atom"),
        };

        if *prev_id == None {
            *prev_id = Some(atom_id);
        } else {
            panic!("Attempt to reset ID of atom {}", self);
        }
    }

    pub fn get_atom_id(&self) -> AtomID {
        use Atom::*;

        match self {
            Tx(a) => a.atom_id,
            Rx(a) => a.atom_id,
            Link(a) => a.atom_id,
            Bottom => panic!("Attempt to get ID of the bottom atom"),
        }
        .expect("Attempt to access an uninitialized atom")
    }
}

impl cmp::PartialEq for Atom {
    #[rustfmt::skip]
    fn eq(&self, other: &Self) -> bool {
        use Atom::*;

        match self {
            Tx(a) => if let Tx(o) = other { a == o } else { false },
            Rx(a) => if let Rx(o) = other { a == o } else { false },
            Link(a) => if let Link(o) = other { a == o } else { false },
            Bottom => false, // irreflexive, hence no `impl cmp::Eq for Atom`
        }
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Atom::*;

        match self {
            Tx(a) => a.fmt(f),
            Rx(a) => a.fmt(f),
            Link(a) => a.fmt(f),
            Bottom => panic!("Attempt to display the bottom atom"),
        }
    }
}

#[derive(Debug)]
pub struct Port {
    face:      Face,
    atom_id:   Option<AtomID>,
    node_name: String,
    node_id:   NodeID,
}

impl Port {
    pub(crate) fn new(face: Face, node_name: String, node_id: NodeID) -> Self {
        Self { face, atom_id: None, node_name, node_id }
    }

    pub(crate) fn get_face(&self) -> Face {
        self.face
    }

    pub fn get_atom_id(&self) -> AtomID {
        self.atom_id.expect("Attempt to access an uninitialized port")
    }

    pub fn get_node_name(&self) -> &str {
        self.node_name.as_str()
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

impl fmt::Display for Port {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{} {}]", &self.node_name, self.face)
    }
}

#[derive(Debug)]
pub struct Link {
    atom_id:      Option<AtomID>,
    tx_port_id:   PortID,
    tx_node_name: String,
    tx_node_id:   NodeID,
    rx_port_id:   PortID,
    rx_node_name: String,
    rx_node_id:   NodeID,
}

impl Link {
    pub fn new(
        tx_port_id: PortID,
        tx_node_name: String,
        tx_node_id: NodeID,
        rx_port_id: PortID,
        rx_node_name: String,
        rx_node_id: NodeID,
    ) -> Self {
        Self {
            atom_id: None,
            tx_port_id,
            tx_node_name,
            tx_node_id,
            rx_port_id,
            rx_node_name,
            rx_node_id,
        }
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

impl fmt::Display for Link {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} > {})", &self.tx_node_name, &self.rx_node_name)
    }
}
