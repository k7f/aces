use std::{cmp, fmt};
use crate::sat::{self, FromAtomID};

#[derive(Debug)]
pub(crate) struct AtomSpace {
    atoms: Vec<Atom>,
}

impl AtomSpace {
    pub(crate) fn new() -> Self {
        Self { atoms: vec![Atom::Bottom] }
    }

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

    pub(crate) fn take_source(&mut self, source: Source) -> usize {
        self.take_atom(Atom::Source(source))
    }

    pub(crate) fn take_sink(&mut self, sink: Sink) -> usize {
        self.take_atom(Atom::Sink(sink))
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

    pub(crate) fn get_source(&self, id: usize) -> Option<&Source> {
        match self.get_atom(id) {
            Some(Atom::Source(a)) => Some(a),
            _ => None,
        }
    }

    pub(crate) fn get_source_mut(&mut self, id: usize) -> Option<&mut Source> {
        match self.get_atom_mut(id) {
            Some(Atom::Source(a)) => Some(a),
            _ => None,
        }
    }

    pub(crate) fn get_sink(&self, id: usize) -> Option<&Sink> {
        match self.get_atom(id) {
            Some(Atom::Sink(a)) => Some(a),
            _ => None,
        }
    }

    pub(crate) fn get_sink_mut(&mut self, id: usize) -> Option<&mut Sink> {
        match self.get_atom_mut(id) {
            Some(Atom::Sink(a)) => Some(a),
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
    Source(Source),
    Sink(Sink),
    Link(Link),
    Bottom,
}

impl Atom {
    fn set_id(&mut self, id: usize) {
        use Atom::*;
        let prev_id = match self {
            Source(a) => &mut a.atom_id,
            Sink(a) => &mut a.atom_id,
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
            Source(a) => a.atom_id,
            Sink(a) => a.atom_id,
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
            Source(a) => if let Source(o) = other { a == o } else { false },
            Sink(a) => if let Sink(o) = other { a == o } else { false },
            Link(a) => if let Link(o) = other { a == o } else { false },
            Bottom => false, // irreflexive, hence no `impl cmp::Eq for Atom`
        }
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Atom::*;
        match self {
            Source(a) => a.fmt(f),
            Sink(a) => a.fmt(f),
            Link(a) => a.fmt(f),
            Bottom => panic!("Attempt to display the bottom atom"),
        }
    }
}

#[derive(Debug)]
pub struct Source {
    atom_id:   usize,
    node_name: String,
    node_id:   usize,
}

impl Source {
    pub fn new(node_name: String, node_id: usize) -> Self {
        Self { atom_id: 0, node_name, node_id }
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

impl cmp::PartialEq for Source {
    fn eq(&self, other: &Self) -> bool {
        self.node_id == other.node_id
    }
}

impl cmp::Eq for Source {}

impl fmt::Display for Source {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{} >]", &self.node_name)
    }
}

#[derive(Debug)]
pub struct Sink {
    atom_id:   usize,
    node_name: String,
    node_id:   usize,
}

impl Sink {
    pub fn new(node_name: String, node_id: usize) -> Self {
        Self { atom_id: 0, node_name, node_id }
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

impl cmp::PartialEq for Sink {
    fn eq(&self, other: &Self) -> bool {
        self.node_id == other.node_id
    }
}

impl cmp::Eq for Sink {}

impl fmt::Display for Sink {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{} <]", &self.node_name)
    }
}

#[derive(Debug)]
pub struct Link {
    atom_id:          usize,
    source_atom_id:   usize,
    source_node_name: String,
    source_node_id:   usize,
    sink_atom_id:     usize,
    sink_node_name:   String,
    sink_node_id:     usize,
}

impl Link {
    pub fn new(
        source_atom_id:   usize,
        source_node_name: String,
        source_node_id:   usize,
        sink_atom_id:     usize,
        sink_node_name:   String,
        sink_node_id:     usize,
    ) -> Self
    {
        Self {
            atom_id: 0,
            source_atom_id,
            source_node_name,
            source_node_id,
            sink_atom_id,
            sink_node_name,
            sink_node_id,
        }
    }

    pub fn get_id(&self) -> usize {
        self.atom_id
    }

    pub fn get_source_atom_id(&self) -> usize {
        self.source_atom_id
    }

    pub fn get_source_node_name(&self) -> &str {
        self.source_node_name.as_str()
    }

    pub fn get_source_node_id(&self) -> usize {
        self.source_node_id
    }

    pub fn get_sink_atom_id(&self) -> usize {
        self.sink_atom_id
    }

    pub fn get_sink_node_name(&self) -> &str {
        self.sink_node_name.as_str()
    }

    pub fn get_sink_node_id(&self) -> usize {
        self.sink_node_id
    }

    pub fn as_sat_literal(&self, negated: bool) -> sat::Lit {
        sat::Lit::from_atom_id(self.atom_id, negated)
    }
}

impl cmp::PartialEq for Link {
    fn eq(&self, other: &Self) -> bool {
        self.source_node_id == other.source_node_id && self.sink_node_id == other.sink_node_id
    }
}

impl cmp::Eq for Link {}

impl fmt::Display for Link {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} > {})", &self.source_node_name, &self.sink_node_name)
    }
}
