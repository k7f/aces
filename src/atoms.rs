use std::fmt;

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
        let ref mut prev_id = match self {
            Source(a) => a.atom_id,
            Sink(a) => a.atom_id,
            Link(a) => a.atom_id,
            Bottom => panic!("Attempt to set ID of the bottom atom"),
        };

        assert_eq!(*prev_id, 0, "Attempt to reset ID of atom {}", self);

        *prev_id = id
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

#[derive(Default, Debug)]
pub struct AtomSpace {
    atoms: Vec<Atom>,
}

impl AtomSpace {
    pub fn new() -> Self {
        let mut result: Self = Default::default();

        result.atoms.push(Atom::Bottom);

        result
    }

    pub fn take_atom(&mut self, mut atom: Atom) -> usize {
        let id = self.atoms.len();
        atom.set_id(id);
        self.atoms.push(atom);

        id
    }

    pub fn get_atom(&self, id: usize) -> Option<&Atom> {
        assert_ne!(id, 0, "Attempt to get to the bottom atom");

        self.atoms.get(id)
    }

    pub fn get_atom_mut(&mut self, id: usize) -> Option<&mut Atom> {
        assert_ne!(id, 0, "Attempt to modify the bottom atom");

        self.atoms.get_mut(id)
    }

    pub fn get_source(&self, id: usize) -> Option<&Source> {
        match self.get_atom(id) {
            Some(Atom::Source(a)) => Some(a),
            _ => None,
        }
    }

    pub fn get_source_mut(&mut self, id: usize) -> Option<&mut Source> {
        match self.get_atom_mut(id) {
            Some(Atom::Source(a)) => Some(a),
            _ => None,
        }
    }

    pub fn get_sink(&self, id: usize) -> Option<&Sink> {
        match self.get_atom(id) {
            Some(Atom::Sink(a)) => Some(a),
            _ => None,
        }
    }

    pub fn get_sink_mut(&mut self, id: usize) -> Option<&mut Sink> {
        match self.get_atom_mut(id) {
            Some(Atom::Sink(a)) => Some(a),
            _ => None,
        }
    }

    pub fn get_link(&self, id: usize) -> Option<&Link> {
        match self.get_atom(id) {
            Some(Atom::Link(a)) => Some(a),
            _ => None,
        }
    }

    pub fn get_link_mut(&mut self, id: usize) -> Option<&mut Link> {
        match self.get_atom_mut(id) {
            Some(Atom::Link(a)) => Some(a),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct Source {
    node_name: String,
    node_id: usize,
    atom_id: usize,
}

impl Source {
    pub fn new(node_name: &str, node_id: usize) -> Self {
        let node_name = node_name.to_owned();
        let atom_id = 0;

        Self { node_name, node_id, atom_id }
    }

    fn create_link(&self, coport: &Sink) -> Link {
        Link::new(self, coport)
    }

    pub fn get_host_name(&self) -> &str {
        self.node_name.as_str()
    }

    pub fn get_host_id(&self) -> usize {
        self.node_id
    }
}

impl fmt::Display for Source {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{} >]", &self.node_name)
    }
}

#[derive(Debug)]
pub struct Sink {
    node_name: String,
    node_id: usize,
    atom_id: usize,
}

impl Sink {
    pub fn new(node_name: &str, node_id: usize) -> Self {
        let node_name = node_name.to_owned();
        let atom_id = 0;

        Self { node_name, node_id, atom_id }
    }

    fn create_link(&self, coport: &Source) -> Link {
        Link::new(coport, self)
    }

    pub fn get_host_name(&self) -> &str {
        self.node_name.as_str()
    }

    pub fn get_host_id(&self) -> usize {
        self.node_id
    }
}

impl fmt::Display for Sink {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{} <]", &self.node_name)
    }
}

#[derive(Debug)]
pub struct Link {
    source_name: String,
    sink_name: String,
    source_id: usize,
    sink_id: usize,
    atom_id: usize,
}

impl Link {
    pub fn new(source: &Source, sink: &Sink) -> Self {
        Self {
            source_name: source.get_host_name().to_owned(),
            sink_name: sink.get_host_name().to_owned(),
            source_id: source.get_host_id(),
            sink_id: sink.get_host_id(),
            atom_id: 0,
        }
    }

    pub fn get_source_name(&self) -> &str {
        self.source_name.as_str()
    }

    pub fn get_sink_name(&self) -> &str {
        self.sink_name.as_str()
    }

    pub fn get_source_id(&self) -> usize {
        self.source_id
    }

    pub fn get_sink_id(&self) -> usize {
        self.sink_id
    }
}

impl fmt::Display for Link {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} > {})", &self.source_name, &self.sink_name)
    }
}
