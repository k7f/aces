use std::{
    cmp, fmt,
    sync::{Arc, Mutex},
    error::Error,
};
use crate::{
    ContentOrigin, Port, Link, ID, NodeID, PortID, LinkID, node,
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
    pub(crate) magic_id: usize,
    pub(crate) name_id:  ID,
    pub(crate) origin:   ContentOrigin,
    pub(crate) globals:  NameSpace,
    pub(crate) nodes:    NameSpace,
    pub(crate) atoms:    AtomSpace,
}

impl Context {
    /// Creates a new toplevel `Context` instance and returns a
    /// corresponding [`ContextHandle`].
    ///
    /// Calling this method is the only public way of creating
    /// toplevel `Context` instances.
    pub fn new_toplevel<S: AsRef<str>>(name: S, origin: ContentOrigin) -> ContextHandle {
        let magic_id = rand::random();

        let mut globals = NameSpace::default();
        let name_id = globals.share_name(name);

        let ctx = Self {
            magic_id,
            name_id,
            origin,
            globals,
            nodes: Default::default(),
            atoms: Default::default(),
        };

        Arc::new(Mutex::new(ctx))
    }

    pub fn reset(&mut self, new_origin: ContentOrigin) {
        self.origin = new_origin;
        // FIXME clear
    }

    /// Creates a new derived `Context` instance and returns a
    /// corresponding [`ContextHandle`].
    ///
    /// Calling this method is the only public way of creating derived
    /// `Context` instances.
    pub fn new_derived<S: AsRef<str>>(name: S, parent: ContextHandle) -> ContextHandle {
        let ctx = {
            let mut parent = parent.lock().unwrap();

            let magic_id = parent.magic_id;

            // ID of child name in parent and child is the same (but
            // not in further ancestors).  See `partial_cmp`.
            let name_id = parent.globals.share_name(name);

            let origin = parent.origin.clone();
            let globals = parent.globals.clone();
            let nodes = parent.nodes.clone();
            let atoms = parent.atoms.clone();

            Self { magic_id, name_id, origin, globals, nodes, atoms }
        };

        Arc::new(Mutex::new(ctx))
    }

    pub fn is_toplevel(&self) -> bool {
        self.name_id == unsafe { ID::new_unchecked(1) }
    }

    pub fn is_derived(&self) -> bool {
        self.name_id > unsafe { ID::new_unchecked(1) }
    }

    pub fn get_name(&self) -> &str {
        self.globals.get_name(self.name_id).expect("Invalid context.")
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

impl cmp::PartialEq for Context {
    fn eq(&self, other: &Self) -> bool {
        self.magic_id == other.magic_id && self.name_id == other.name_id
    }
}

impl cmp::Eq for Context {}

impl cmp::PartialOrd for Context {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        if self.magic_id == other.magic_id {
            if other.globals.get_id(self.get_name()) == Some(self.name_id) {
                // ID of self's name in self and other is the same...
                if self.name_id < other.name_id {
                    // ...self is an ancestor of other,
                    Some(cmp::Ordering::Greater)
                } else if self.name_id > other.name_id {
                    // ...other is _the_ parent of self,
                    Some(cmp::Ordering::Less)
                } else {
                    // ...they are equal.
                    Some(cmp::Ordering::Equal)
                }
            } else if self.globals.get_id(other.get_name()) == Some(other.name_id) {
                // ID of other's name in self and other is the same...
                if self.name_id < other.name_id {
                    // ...self is _the_ parent of other: this case
                    // should have already been covered,
                    unreachable!()
                } else if self.name_id > other.name_id {
                    // ...other is an ancestor of self,
                    Some(cmp::Ordering::Less)
                } else {
                    // ...they are equal: this case should have
                    // already been covered.
                    unreachable!()
                }
            } else if self.name_id == other.name_id {
                // This case should have already been covered.
                unreachable!()
            } else {
                None
            }
        } else {
            None
        }
    }
}

/// A trait for binding objects to [`Context`] temporarily, without
/// permanently storing (and synchronizing) context references inside
/// the objects.
///
/// See [`InContext`] for more details.
pub trait Contextual {
    fn format(&self, ctx: &Context, dock: Option<node::Face>) -> Result<String, Box<dyn Error>>;
}

/// A short-term binding of [`Context`] and any immutable object of a
/// type that implements the [`Contextual`] trait.
///
/// [`Context`] can't be modified through `InContext`.  The purpose
/// of this type is to allow a transparent read access to shared data,
/// like names etc.
pub struct InContext<'a, D: Contextual> {
    context: &'a Context,
    dock:    Option<node::Face>,
    thing:   &'a D,
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
        write!(f, "{}", self.thing.format(self.context, self.dock).expect("Can't display"))
    }
}

/// A short-term binding of [`Context`] and any mutable object of a
/// type that implements the [`Contextual`] trait.
///
/// [`Context`] can't be modified through `InContextMut`.  The purpose
/// of this type is to allow a transparent read access to shared data,
/// like names etc.
pub struct InContextMut<'a, D: Contextual> {
    context: &'a Context,
    dock:    Option<node::Face>,
    thing:   &'a mut D,
}

impl<D: Contextual> InContextMut<'_, D> {
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

    #[inline]
    pub fn get_thing_mut(&mut self) -> &mut D {
        self.thing
    }
}

impl<'a, D: Contextual> fmt::Display for InContextMut<'a, D> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.thing.format(self.context, self.dock).expect("Can't display"))
    }
}

impl Context {
    pub fn with<'a, T: Contextual>(&'a self, thing: &'a T) -> InContext<'a, T> {
        InContext { context: self, dock: None, thing }
    }

    pub fn with_mut<'a, T: Contextual>(&'a self, thing: &'a mut T) -> InContextMut<'a, T> {
        InContextMut { context: self, dock: None, thing }
    }

    pub fn with_docked<'a, T: Contextual>(
        &'a self,
        face: node::Face,
        thing: &'a T,
    ) -> InContext<'a, T> {
        InContext { context: self, dock: Some(face), thing }
    }

    pub fn with_docked_mut<'a, T: Contextual>(
        &'a self,
        face: node::Face,
        thing: &'a mut T,
    ) -> InContextMut<'a, T> {
        InContextMut { context: self, dock: Some(face), thing }
    }
}
