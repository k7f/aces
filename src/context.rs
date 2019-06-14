use std::collections::BTreeMap;
use crate::{ID, Port, Link, NodeID, PortID, LinkID, atom::{AtomSpace, AtomID}};

#[derive(Debug)]
pub(crate) struct NameSpace {
    names: Vec<String>,
    ids:   BTreeMap<String, ID>, // FIXME borrow names
}

impl NameSpace {
    pub(crate) fn get_name(&self, id: ID) -> Option<&str> {
        self.names.get(id.get()).map(|s| s.as_str())
    }

    pub(crate) fn get_id(&self, name: &str) -> Option<ID> {
        self.ids.get(name).copied()
    }

    pub(crate) fn take_id(&mut self, name: &str) -> ID {
        self.ids.get(name).copied().unwrap_or_else(|| {
            let id = unsafe {
                ID::new_unchecked(self.names.len())
            };

            self.names.push(name.to_string());
            self.ids.insert(name.to_string(), id);

            id
        })
    }
}

impl Default for NameSpace {
    fn default() -> Self {
        Self {
            names: vec![String::new()],
            ids:   Default::default(),
        }
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

    pub fn take_node_id(&mut self, node_name: &str) -> NodeID {
        NodeID(self.nodes.take_id(node_name))
    }

    pub fn get_node_name(&self, node_id: NodeID) -> Option<&str> {
        self.nodes.get_name(node_id.get().into())
    }

    pub fn get_node_id(&self, node_name: &str) -> Option<NodeID> {
        self.nodes.get_id(node_name).map(|id| NodeID(id))
    }

    // Atoms

    pub(crate) fn is_port(&self, atom_id: AtomID) -> bool {
        self.atoms.is_port(atom_id)
    }

    pub fn take_port(&mut self, port: Port) -> PortID {
        self.atoms.take_port(port)
    }

    pub fn take_link(&mut self, link: Link) -> LinkID {
        self.atoms.take_link(link)
    }

    pub fn get_port(&self, port_id: PortID) -> Option<&Port> {
        self.atoms.get_port(port_id)
    }

    pub fn get_port_mut(&mut self, port_id: PortID) -> Option<&mut Port> {
        self.atoms.get_port_mut(port_id)
    }

    pub fn get_link(&self, link_id: LinkID) -> Option<&Link> {
        self.atoms.get_link(link_id)
    }

    pub fn get_link_mut(&mut self, link_id: LinkID) -> Option<&mut Link> {
        self.atoms.get_link_mut(link_id)
    }

    pub fn get_antiport_id(&self, port_id: PortID) -> Option<PortID> {
        self.atoms.get_antiport_id(port_id)
    }
}
