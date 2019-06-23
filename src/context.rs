use std::{
    fmt,
    error::Error,
    sync::{Arc, Mutex},
};
use crate::{
    Port, Link, NodeID, PortID, LinkID, node,
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
/// trait and [`InContext`] struct.
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
/// [`InContext`] struct.
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

    pub fn share_node_name<S: AsRef<str>>(&mut self, node_name: S) -> NodeID {
        NodeID(self.nodes.share_name(node_name))
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

    pub fn share_port(&mut self, port: &mut Port) -> PortID {
        self.atoms.share_port(port)
    }

    pub fn share_link(&mut self, link: &mut Link) -> LinkID {
        self.atoms.share_link(link)
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
/// See [`InContext`] for more details.
pub trait Contextual {
    fn format(&self, ctx: &Context) -> Result<String, Box<dyn Error>>;
}

/// A short-term binding of [`Context`] and any data implementing the
/// [`Contextual`] trait.
///
/// [`Context`] can't be modified through `InContext`.  The purpose
/// of this type is to allow a transparent read access to shared data,
/// like names etc.
pub struct InContext<'a, D: Contextual> {
    context: &'a Context,
    dock: Option<node::Face>,
    thing: &'a D,
}

impl<D: Contextual> InContext<'_, D> {
    #[inline]
    pub fn get_context(&self) -> &Context {
        self.context
    }

    #[inline]
    pub fn get_dock(&self) -> Option<node::Face> {
        self.dock
    }

    #[inline]
    pub fn get_thing(&self) -> &D {
        self.thing
    }
}

impl<'a, D: Contextual> fmt::Display for InContext<'a, D> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.thing.format(self.context).expect("Can't display"))
    }
}

impl Context {
    pub fn with<'a, T: Contextual>(&'a self, thing: &'a T) -> InContext<'a, T> {
        InContext { context: self, dock: None, thing }
    }

    pub fn with_docked<'a, T: Contextual>(&'a self, face: node::Face, thing: &'a T) -> InContext<'a, T> {
        InContext { context: self, dock: Some(face), thing }
    }
}
