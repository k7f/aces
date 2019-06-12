use std::collections::BTreeMap;
use crate::atom::{AtomSpace, Atom, Port, Link};

#[derive(Default, Debug)]
pub(crate) struct NameSpace {
    names: Vec<String>,
    ids:   BTreeMap<String, usize>, // FIXME borrow names
}

impl NameSpace {
    pub(crate) fn get_name(&self, id: usize) -> Option<&str> {
        self.names.get(id).map(|s| s.as_str())
    }

    pub(crate) fn get_id(&self, name: &str) -> Option<usize> {
        self.ids.get(name).copied()
    }

    pub(crate) fn take_id(&mut self, name: &str) -> usize {
        self.ids.get(name).copied().unwrap_or_else(|| {
            let id = self.names.len();

            self.names.push(name.to_string());
            self.ids.insert(name.to_string(), id);

            id
        })
    }
}

#[derive(Default, Debug)]
pub struct Context {
    pub(crate) globals: NameSpace,
    pub(crate) nodes:   NameSpace,
    pub(crate) atoms:   AtomSpace,
}

impl Context {
    pub fn new() -> Self {
        Default::default()
    }

    // Nodes

    pub fn take_node_id(&mut self, node_name: &str) -> usize {
        self.nodes.take_id(node_name)
    }

    pub fn get_node_name(&self, node_id: usize) -> Option<&str> {
        self.nodes.get_name(node_id)
    }

    pub fn get_node_id(&self, node_name: &str) -> Option<usize> {
        self.nodes.get_id(node_name)
    }

    // Atoms

    pub fn take_atom(&mut self, atom: Atom) -> usize {
        self.atoms.take_atom(atom)
    }

    pub fn take_port(&mut self, port: Port) -> usize {
        self.atoms.take_port(port)
    }

    pub fn take_link(&mut self, link: Link) -> usize {
        self.atoms.take_link(link)
    }

    pub fn get_atom(&self, atom_id: usize) -> Option<&Atom> {
        self.atoms.get_atom(atom_id)
    }

    pub fn get_atom_mut(&mut self, atom_id: usize) -> Option<&mut Atom> {
        self.atoms.get_atom_mut(atom_id)
    }

    pub fn is_port(&self, atom_id: usize) -> bool {
        self.atoms.is_port(atom_id)
    }

    pub fn get_port(&self, atom_id: usize) -> Option<&Port> {
        self.atoms.get_port(atom_id)
    }

    pub fn get_port_mut(&mut self, atom_id: usize) -> Option<&mut Port> {
        self.atoms.get_port_mut(atom_id)
    }

    pub fn get_link(&self, atom_id: usize) -> Option<&Link> {
        self.atoms.get_link(atom_id)
    }

    pub fn get_link_mut(&mut self, atom_id: usize) -> Option<&mut Link> {
        self.atoms.get_link_mut(atom_id)
    }

    pub fn get_antiport_id(&self, port_id: usize) -> Option<usize> {
        self.atoms.get_antiport_id(port_id)
    }
}
