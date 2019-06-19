use std::{
    fmt,
    error::Error,
    sync::{Arc, Mutex},
};
use crate::{
    Port, Link, NodeID, PortID, LinkID,
    name::NameSpace,
    atom::{AtomSpace, AtomID},
};

/// A handle to a [`Context`] instance.
///
/// All [`Context`] handles used in _aces_ have type
/// `Arc<Mutex<Context>>`.  They are stored permanently in the
/// following structs: [`CES`], [`sat::Formula`], [`sat::Solver`], and
/// [`sat::Solution`].
///
/// For another way of binding [`Context`] to data see [`Contextual`]
/// trait and [`WithContext`] struct.
///
/// [`CES`]: crate::CES
/// [`sat::Formula`]: crate::sat::Formula
/// [`sat::Solver`]: crate::sat::Solver
/// [`sat::Solution`]: crate::sat::Solution
pub type ContextHandle = Arc<Mutex<Context>>;

/// A representation of shared state.
///
/// This is an umbrella type which, currently, includes a collection
/// of [`Atom`]s and two symbol tables, one for structure names, and
/// another for node names.
///
/// For usage, see [`ContextHandle`] type, [`Contextual`] trait and
/// [`WithContext`] struct.
///
/// [`Atom`]: crate::atom::Atom
#[derive(Debug)]
pub struct Context {
    pub(crate) globals: NameSpace,
    pub(crate) nodes:   NameSpace,
    pub(crate) atoms:   AtomSpace,
}

impl Context {
    fn new() -> Self {
        Self {
            globals: Default::default(),
            nodes:   Default::default(),
            atoms:   Default::default(),
        }
    }

    /// Creates a new `Context` instance and returns a corresponding
    /// [`ContextHandle`].
    ///
    /// Calling this method is the only public way of creating new
    /// `Context` instances.
    pub fn new_as_handle() -> ContextHandle {
        Arc::new(Mutex::new(Context::new()))
    }

    // Nodes

    pub fn take_node_id<S: AsRef<str>>(&mut self, node_name: S) -> NodeID {
        NodeID(self.nodes.take_id(node_name))
    }

    pub fn get_node_name(&self, node_id: NodeID) -> Option<&str> {
        self.nodes.get_name(node_id.get())
    }

    pub fn get_node_id<S: AsRef<str>>(&self, node_name: S) -> Option<NodeID> {
        self.nodes.get_id(node_name).map(NodeID)
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

/// A trait for binding objects to [`Context`] temporarily, without
/// permanently storing (and synchronizing) context references inside
/// the objects.
///
/// See [`WithContext`] for more details.
pub trait Contextual {
    fn format(&self, ctx: &Context) -> Result<String, Box<dyn Error>>;
}

/// A short-term binding of [`Context`] and any data implementing the
/// [`Contextual`] trait.
///
/// [`Context`] can't be modified through `WithContext`.  The purpose
/// of this type is to allow a transparent read access to shared data,
/// like names etc.
pub struct WithContext<'a, D: Contextual>(&'a Context, &'a D);

impl<'a, D: Contextual> fmt::Display for WithContext<'a, D> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let WithContext(ctx, data) = *self;
        write!(f, "{}", data.format(ctx).expect("Can't display"))
    }
}

impl Context {
    pub fn with<'a, T: Contextual>(&'a self, value: &'a T) -> WithContext<'a, T> {
        WithContext(self, value)
    }
}
