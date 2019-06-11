use std::{cmp, fmt, collections::BTreeMap};
use crate::sat::{self, CESLit};

// FIXME replace with wrapping structs
pub(crate) type NodeID = usize;
pub(crate) type TxID = usize;
pub(crate) type RxID = usize;
pub(crate) type LinkID = usize;

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
    source_nodes:   BTreeMap<NodeID, TxID>,
    sink_nodes:     BTreeMap<NodeID, RxID>,
    internal_nodes: BTreeMap<NodeID, (TxID, RxID)>,
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
    pub(crate) fn take_atom(&mut self, mut atom: Atom) -> usize {
        for old_atom in self.atoms.iter() {
            if old_atom == &atom {
                return old_atom.get_id()
            }
        }

        let id = self.atoms.len();
        atom.set_id(id);
        self.atoms.push(atom);

        id
    }

    pub(crate) fn take_port(&mut self, port: Port) -> usize {
        let node_id = port.node_id;

        match port.face {
            Face::Tx => {
                let port_id = self.take_atom(Atom::Tx(port));

                if let Some(&rx_id) = self.sink_nodes.get(&node_id) {
                    self.sink_nodes.remove(&node_id);
                    self.internal_nodes.insert(node_id, (port_id, rx_id));
                } else {
                    self.source_nodes.insert(node_id, port_id);
                }

                port_id
            }
            Face::Rx => {
                let port_id = self.take_atom(Atom::Rx(port));

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

    pub(crate) fn take_link(&mut self, link: Link) -> usize {
        self.take_atom(Atom::Link(link))
    }

    pub(crate) fn get_atom(&self, id: usize) -> Option<&Atom> {
        assert_ne!(id, 0, "Attempt to get to the bottom atom");

        self.atoms.get(id)
    }

    pub(crate) fn get_atom_mut(&mut self, id: usize) -> Option<&mut Atom> {
        assert_ne!(id, 0, "Attempt to modify the bottom atom");

        self.atoms.get_mut(id)
    }

    pub(crate) fn get_port(&self, id: usize) -> Option<&Port> {
        match self.get_atom(id) {
            Some(Atom::Tx(a)) => Some(a),
            Some(Atom::Rx(a)) => Some(a),
            _ => None,
        }
    }

    pub(crate) fn get_port_mut(&mut self, id: usize) -> Option<&mut Port> {
        match self.get_atom_mut(id) {
            Some(Atom::Tx(a)) => Some(a),
            Some(Atom::Rx(a)) => Some(a),
            _ => None,
        }
    }

    pub(crate) fn get_link(&self, id: usize) -> Option<&Link> {
        match self.get_atom(id) {
            Some(Atom::Link(a)) => Some(a),
            _ => None,
        }
    }

    pub(crate) fn get_link_mut(&mut self, id: usize) -> Option<&mut Link> {
        match self.get_atom_mut(id) {
            Some(Atom::Link(a)) => Some(a),
            _ => None,
        }
    }

    pub fn get_antiport_id(&self, port_id: usize) -> Option<usize> {
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
pub enum Atom {
    Tx(Port),
    Rx(Port),
    Link(Link),
    Bottom,
}

impl Atom {
    fn set_id(&mut self, id: usize) {
        use Atom::*;
        let prev_id = match self {
            Tx(a) => &mut a.atom_id,
            Rx(a) => &mut a.atom_id,
            Link(a) => &mut a.atom_id,
            Bottom => panic!("Attempt to set ID of the bottom atom"),
        };

        if *prev_id == 0 {
            *prev_id = id;
        } else {
            panic!("Attempt to reset ID of atom {}", self);
        }
    }

    pub fn get_id(&self) -> usize {
        use Atom::*;
        let id = match self {
            Tx(a) => a.atom_id,
            Rx(a) => a.atom_id,
            Link(a) => a.atom_id,
            Bottom => panic!("Attempt to get ID of the bottom atom"),
        };

        assert_ne!(id, 0, "Attempt to use uninitialized atom {}", self);

        id
    }

    pub fn as_sat_literal(&self, negated: bool) -> sat::Lit {
        sat::Lit::from_atom_id(self.get_id(), negated)
    }
}

impl cmp::PartialEq for Atom {
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
    atom_id:   usize,
    node_name: String,
    node_id:   usize,
}

impl Port {
    pub(crate) fn new(face: Face, node_name: String, node_id: usize) -> Self {
        Self { face: face, atom_id: 0, node_name, node_id }
    }

    pub(crate) fn get_face(&self) -> Face {
        self.face
    }

    pub fn get_id(&self) -> usize {
        self.atom_id
    }

    pub fn get_node_name(&self) -> &str {
        self.node_name.as_str()
    }

    pub fn get_node_id(&self) -> usize {
        self.node_id
    }

    pub fn as_sat_literal(&self, negated: bool) -> sat::Lit {
        sat::Lit::from_atom_id(self.atom_id, negated)
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
    atom_id:      usize,
    tx_atom_id:   usize,
    tx_node_name: String,
    tx_node_id:   usize,
    rx_atom_id:   usize,
    rx_node_name: String,
    rx_node_id:   usize,
}

impl Link {
    pub fn new(
        tx_atom_id:   usize,
        tx_node_name: String,
        tx_node_id:   usize,
        rx_atom_id:   usize,
        rx_node_name: String,
        rx_node_id:   usize,
    ) -> Self
    {
        Self {
            atom_id: 0,
            tx_atom_id,
            tx_node_name,
            tx_node_id,
            rx_atom_id,
            rx_node_name,
            rx_node_id,
        }
    }

    pub fn get_id(&self) -> usize {
        self.atom_id
    }

    pub fn get_tx_atom_id(&self) -> usize {
        self.tx_atom_id
    }

    pub fn get_tx_node_name(&self) -> &str {
        self.tx_node_name.as_str()
    }

    pub fn get_tx_node_id(&self) -> usize {
        self.tx_node_id
    }

    pub fn get_rx_atom_id(&self) -> usize {
        self.rx_atom_id
    }

    pub fn get_rx_node_name(&self) -> &str {
        self.rx_node_name.as_str()
    }

    pub fn get_rx_node_id(&self) -> usize {
        self.rx_node_id
    }

    pub fn as_sat_literal(&self, negated: bool) -> sat::Lit {
        sat::Lit::from_atom_id(self.atom_id, negated)
    }
}

impl cmp::PartialEq for Link {
    fn eq(&self, other: &Self) -> bool {
        self.tx_node_id == other.tx_node_id && self.rx_node_id == other.rx_node_id
    }
}

impl cmp::Eq for Link {}

impl fmt::Display for Link {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} > {})", &self.tx_node_name, &self.rx_node_name)
    }
}
