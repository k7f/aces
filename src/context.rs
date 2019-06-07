use crate::nodes::NodeSpace;
use crate::atoms::{AtomSpace, Atom, Source, Sink, Link};

#[derive(Debug)]
pub struct Context {
    pub(crate) nodes: NodeSpace,
    pub(crate) atoms: AtomSpace,
}

impl Context {
    pub fn new() -> Self {
        Self {
            nodes: NodeSpace::new(),
            atoms: AtomSpace::new(),
        }
    }

    pub fn get_node_name(&self, node_id: usize) -> Option<&str> {
        self.nodes.get_name(node_id)
    }

    pub fn get_node_id(&self, node_name: &str) -> Option<usize> {
        self.nodes.get_id(node_name)
    }

    pub fn take_node_id(&mut self, node_name: &str) -> usize {
        self.nodes.take_id(node_name)
    }

    pub fn take_atom(&mut self, atom: Atom) -> usize {
        self.atoms.take_atom(atom)
    }

    pub fn take_source(&mut self, source: Source) -> usize {
        self.atoms.take_source(source)
    }

    pub fn take_sink(&mut self, sink: Sink) -> usize {
        self.atoms.take_sink(sink)
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

    pub fn get_source(&self, atom_id: usize) -> Option<&Source> {
        self.atoms.get_source(atom_id)
    }

    pub fn get_source_mut(&mut self, atom_id: usize) -> Option<&mut Source> {
        self.atoms.get_source_mut(atom_id)
    }

    pub fn get_sink(&self, atom_id: usize) -> Option<&Sink> {
        self.atoms.get_sink(atom_id)
    }

    pub fn get_sink_mut(&mut self, atom_id: usize) -> Option<&mut Sink> {
        self.atoms.get_sink_mut(atom_id)
    }

    pub fn get_link(&self, atom_id: usize) -> Option<&Link> {
        self.atoms.get_link(atom_id)
    }

    pub fn get_link_mut(&mut self, atom_id: usize) -> Option<&mut Link> {
        self.atoms.get_link_mut(atom_id)
    }
}
