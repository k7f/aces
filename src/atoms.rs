use std::{cmp, fmt};
use crate::sat::{self, FromAtomID};

#[derive(PartialEq, Debug)]
enum Side {
    Tx,
    Rx,
}

impl fmt::Display for Side {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Side::Tx => write!(f, ">"),
            Side::Rx => write!(f, "<"),
        }
    }
}

#[derive(Debug)]
pub(crate) struct AtomSpace {
    atoms: Vec<Atom>,
}

impl Default for AtomSpace {
    fn default() -> Self {
        Self { atoms: vec![Atom::Bottom] }
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
        match port.side {
            Side::Tx => self.take_atom(Atom::Tx(port)),
            Side::Rx => self.take_atom(Atom::Rx(port)),
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
    side:      Side,
    atom_id:   usize,
    node_name: String,
    node_id:   usize,
}

impl Port {
    pub fn new_tx(node_name: String, node_id: usize) -> Self {
        Self { side: Side::Tx, atom_id: 0, node_name, node_id }
    }

    pub fn new_rx(node_name: String, node_id: usize) -> Self {
        Self { side: Side::Rx, atom_id: 0, node_name, node_id }
    }

    pub fn is_tx(&self) -> bool {
        self.side == Side::Tx
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
        write!(f, "[{} {}]", &self.node_name, self.side)
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
